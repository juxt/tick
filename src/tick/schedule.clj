(ns tick.schedule
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
   [java.time.temporal ChronoUnit]
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))

(defn schedule-next [next-time {:keys [clock executor callback]}]
  (when next-time
    (let [dly (.until (.instant clock) (:tick/date next-time) ChronoUnit/MILLIS)]
      (.schedule executor ^Callable callback dly TimeUnit/MILLISECONDS))))

(defn trigger-events [f values error-handler]
  (loop [[v] values]
    (when v
      (let [r
            (try
              (f v)
              :continue ; TODO: Might be a good feature to allow a trigger to influence continuation
              (catch Exception e
                (error-handler e)))]
        (if (= :continue r)
          (recur (next values))
          :abort)))))

(defn callback [state timeline {:keys [clock trigger executor promise error-handler] :as opts}]
  (let [[due next-timeline] (split-with #(not (.isAfter (.toInstant (:tick/date %)) (.instant clock))) timeline)]

    (if-not (empty? next-timeline)
      ;; Schedule next
      (do
        (let [fut (schedule-next (first next-timeline)
                                                {:clock clock
                                                 :executor executor
                                                 :callback #(callback state next-timeline opts)})]
          (swap! state assoc
                 :timeline next-timeline
                 :scheduled-future fut)
          (when (= :abort (trigger-events trigger due error-handler))
            (.cancel fut false)
            (.shutdown executor)
            ;; Early delivery of promise
            (deliver promise :fail))))

      ;; Set status to :done
      (do (swap! state (fn [s] (-> s
                                   (assoc :status :done)
                                   (dissoc :timeline :scheduled-future))))
          (trigger-events trigger due error-handler)
          (.shutdown executor)
          (deliver promise :done)))))

(defprotocol ITicker
  "A ticker travels across a timeline, usually triggering some action for each time on the timeline."
  (start [_ clock] "Start a ticker. If requirefd, deref the result to block until the schedule is complete.")
  (pause [_] "If supported by the ticker, pause. Can be resumed.")
  (resume [_] "Resume a paused ticker.")
  (stop [_] "Stop the ticker. Can be restarted with start.")
  (remaining [_] "Return the remaining timeline yet to be visited by the ticker.")
  (clock [_] "Return the clock indicating where the ticker is in the timeline."))

(defrecord SchedulingTicker [trigger timeline executor state promise error-handler]
  ITicker
  (start [_ clock]
    (swap! state assoc
           :status :running
           :timeline timeline
           :scheduled-future (schedule-next
                              (first timeline)
                              {:clock clock
                               :executor executor
                               :callback #(callback state timeline
                                                    {:clock clock
                                                     :trigger trigger
                                                     :executor executor
                                                     :promise promise
                                                     :error-handler error-handler})})
           :clock clock
           :executor executor)
    promise)

  (pause [_]
    (let [st @state]
      (when-let [fut (:scheduled-future st)]
        (.cancel fut false))
      (swap! state (fn [st] (-> st (assoc :status :paused) (dissoc :scheduled-future))))
      :ok))

  (resume [_]
    (let [st @state]
      (when (= :paused (:status st))
        (let [timeline (:timeline st)
              executor (:executor st)
              clock (:clock st)]
          (swap! state (fn [st] (-> st (assoc :status :running
                                              :scheduled-future (schedule-next
                                                                 (first timeline)
                                                                 {:clock clock
                                                                  :executor executor
                                                                  :callback #(callback state timeline
                                                                                       {:clock clock
                                                                                        :trigger trigger
                                                                                        :executor executor
                                                                                        :promise promise
                                                                                        :error-handler error-handler})}))))))

        :ok)))

  (stop [_]
    (when-let [fut (:scheduled-future @state)]
      (.cancel fut false))
    (swap! state (fn [s] (-> s (assoc :status :stopped) (dissoc :scheduled-future))))
    (.shutdown executor)
    :ok)

  (remaining [_]
    (:timeline @state))
  (clock [_]
    (:clock @state)))

(defn default-error-handler [err]
  (println "Error" err)
  (.printStackTrace err)
  :continue)

(defn schedule
  "Think of this like map, but applying a function over a timeline. Returns a ticker."
  ([trigger timeline]
   (schedule trigger timeline {}))
  ([trigger timeline {:keys [executor error-handler]}]
   (map->SchedulingTicker
    {:trigger trigger
     :timeline timeline
     :state (atom {})
     :executor (or executor (new ScheduledThreadPoolExecutor 16))
     :promise (promise)
     :error-handler (or error-handler default-error-handler)})))

(defrecord ImpatientTicker [trigger timeline state executor error-handler]
  ITicker
  (start [this clock]
    (swap! state assoc
           :status :running
           :timeline timeline
           :clock clock)
    (.submit
     executor
     ^Runnable
     (fn []
       (loop [timeline timeline]
         (when-let [tick (first timeline)]
           (when (= (:status @state) :running)
             ;; Advance clock
             (swap! state assoc
                    :clock (Clock/fixed (.toInstant (:tick/date tick)) (.getZone clock))
                    :timeline (next timeline))
             ;; Call trigger
             (when-not (= :abort (trigger-events trigger [tick] error-handler))
               (recur (next timeline))
               ;; TODO: Early delivery of promise?
               )))))))

  (pause [this] :unsupported)
  (resume [this] :unsupported)
  (stop [this]
    (swap! state assoc :status :stopped)
    (.shutdown executor)
    :ok)
  (remaining [this] (:timeline @state))
  (clock [this] (:clock @state)))

(defn simulate
  "Like schedule, but return a ticker that eagerly advances the clock
  to the next time in the timeline and serially executes the trigger."
  ([trigger timeline]
   (simulate trigger timeline {}))
  ([trigger timeline {:keys [executor error-handler]}]
   (map->ImpatientTicker {:trigger trigger
                          :timeline timeline
                          :state (atom {})
                          :executor (or executor (new ScheduledThreadPoolExecutor 1))
                          :error-handler (or error-handler default-error-handler)})))
