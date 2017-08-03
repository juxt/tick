(ns tick.schedule
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
   [java.time.temporal ChronoUnit]
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))

(defn schedule-next [next-time {:keys [clock executor callback]}]
  (when next-time
    (let [dly (.until (.instant clock) (:tick/date next-time) ChronoUnit/MILLIS)]
      (.schedule executor ^Callable callback dly TimeUnit/MILLISECONDS))))

(defn callback [state timeline {:keys [clock trigger executor promise] :as opts}]
  (let [[due next-timeline] (split-with #(not (.isAfter (.toInstant (:tick/date %)) (.instant clock))) timeline)]

    (if-not (empty? next-timeline)
      ;; Schedule next
      (do
        (swap! state assoc
               :timeline next-timeline
               :scheduled-future (schedule-next (first next-timeline)
                                                {:clock clock
                                                 :executor executor
                                                 :callback #(callback state next-timeline opts)}))
        (dorun (map trigger due)))

      ;; Set status to :done
      (do (swap! state (fn [s] (-> s
                                   (assoc :status :done)
                                   (dissoc :timeline :scheduled-future))))
          (dorun (map trigger due))
          (deliver promise :done)))))


(defprotocol ITicker
  "A ticker travels across a timeline, usually triggering some action for each time on the timeline."
  (start [_ clock] "Start a ticker. If required, deref the result to block until the schedule is complete.")
  (pause [_] "If supported by the ticker, pause. Can be resumed.")
  (resume [_] "Resume a paused ticker.")
  (stop [_] "Stop the ticker. Can be restarted with start.")
  (remaining [_] "Return the remaining timeline yet to be visited by the ticker.")
  (clock [_] "Return the clock indicating where the ticker is in the timeline."))

(defrecord SchedulingTicker [trigger timeline executor state promise]
  ITicker
  (start [_ clock]
    (swap! state assoc
           :status :running
           :timeline timeline
           :scheduled-future (schedule-next
                              (first timeline)
                              {:clock clock
                               :executor executor
                               :callback #(callback state timeline {:clock clock :trigger trigger :executor executor :promise promise})})
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
                                                                  :callback #(callback state timeline {:clock clock :trigger trigger :executor executor :promise promise})}))))))

        :ok)))

  (stop [_]
    (when-let [fut (:scheduled-future @state)]
      (.cancel fut false))
    (swap! state (fn [s] (-> s (assoc :status :stopped) (dissoc :scheduled-future))))
    :ok)

  (remaining [_]
    (:timeline @state))
  (clock [_]
    (:clock @state)))

(defn schedule
  "Think of this like map, but applying a function over a timeline. Returns a ticker."
  ([trigger timeline]
   (schedule trigger timeline {}))
  ([trigger timeline {:keys [executor]}]
   (map->SchedulingTicker
    {:trigger trigger
     :timeline timeline
     :state (atom {})
     :executor (or executor (new ScheduledThreadPoolExecutor 16))
     :promise (promise)})))

(defrecord ImpatientTicker [trigger timeline state executor]
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
             (trigger tick)
             (recur (next timeline))))))))

  (pause [this] :unsupported)
  (resume [this] :unsupported)
  (stop [this]
    (swap! state assoc :status :stopped)
    :ok)
  (remaining [this] (:timeline @state))
  (clock [this] (:clock @state)))

(defn simulate
  "Like schedule, but return a ticker that eagerly advances the clock
  to the next time in the timeline and serially executes the trigger."
  ([trigger timeline]
   (simulate trigger timeline {}))
  ([trigger timeline {:keys [executor]}]
   (map->ImpatientTicker {:trigger trigger
                          :timeline timeline
                          :state (atom {})
                          :executor (or executor (new ScheduledThreadPoolExecutor 1))})))
