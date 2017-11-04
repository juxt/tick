(ns tick.schedule
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
   [java.time.temporal ChronoUnit]
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))

(defn schedule-next [next-time {:keys [clock executor callback]}]
  (when next-time
    (let [dly (.until (.instant clock) (:tick/date next-time) ChronoUnit/MILLIS)]
      (.schedule executor ^Callable callback dly TimeUnit/MILLISECONDS))))

(defn callback [state {:keys [clock trigger executor promise] :as opts}]
  ;; The time is now. We have been called for a reason. That reason
  ;; might be that we have to run a task.
  (let [{:keys [timeline due status] :as result}
        (swap! state
               (fn [st]
                 (if-not (= :running (:status st))
                   ;; If we are stopped, paused or done, then we do nothing. We don't
                   ;; schedule another task. We just exit.
                   (dissoc st :due)

                   (let [[due next-timeline] (split-with #(not (.isAfter (.toInstant (:tick/date %)) (.instant clock))) (:timeline st))]

                     (cond-> st
                       (not (empty? next-timeline)) (assoc :timeline next-timeline)
                       (empty? next-timeline) (-> (assoc :status :done)
                                                  (dissoc :timeline))
                       due (assoc :due due))))))]

    (when due
      (when (and (= status :running) timeline)
        (schedule-next (first timeline)
                       {:clock clock
                        :executor executor
                        :callback #(callback state opts)}))

      (doseq [job due]
        (.submit executor #(trigger job))))

    (when (= status :done)
      (deliver promise :done))))

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
    (let [{:keys [timeline]} (swap! state assoc
                                    :status :running
                                    :timeline timeline
                                    :clock clock
                                    :executor executor)]
      (schedule-next
       (first timeline)
       {:clock clock
        :executor executor
        :callback #(callback state {:clock clock :trigger trigger :executor executor :promise promise})}))

    promise)

  (pause [_]
    (let [st @state]
      (swap! state (fn [st] (-> st (assoc :status :paused))))
      :ok))

  (resume [_]
    (let [st @state]
      (when (= :paused (:status st))
        (let [timeline (:timeline st)
              executor (:executor st)
              clock (:clock st)]
          (let [{:keys [timeline]}
                (swap! state (fn [st] (-> st (assoc :status :running
                                                    ))))]
            (schedule-next
             (first timeline)
             {:clock clock
              :executor executor
              :callback #(callback state {:clock clock :trigger trigger :executor executor :promise promise})})))
        :ok)))

  (stop [_]
    (swap! state (fn [s] (-> s (assoc :status :stopped))))
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
