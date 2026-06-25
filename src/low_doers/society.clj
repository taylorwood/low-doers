(ns low-doers.society
  "Temporal dynamics over a society of agents."
  (:require [low-doers.agent :as agent]))

(def default-params
  (merge agent/default-params
         {:probability-hire-HH-type 0.5  ; prob. new hire is high-minded
          :high-type :hs1    ; type key hired into the high-minded role
          :low-type :ls1     ; type key hired into the low-minded role
          :collab-probability 0.5 ; prob. an edge collaborates this year
          :quantile 0.15     ; bottom fraction of yearly earners that exit early
          :age-of-retirement 65
          :age-min 25
          :age-max 65
          :postpone 0          ; years a new hire is shielded from early exit
          ;; rewards & sanctions off unless :reward-pct is set:
          ;; every :reward-frequency years each agent's window payoff is
          ;; raised by :reward-pct (per HH share) and cut by :sanction-pct (per LL share),
          ;; and reconsideration at that tick weighs the same expectation.
          :reward-frequency 1
          :reward-pct nil
          :sanction-pct nil}))

(defn- complete-network [ids]
  (let [v (vec ids)]
    (set (for [i (range (count v))
               j (range (inc i) (count v))]
           [(nth v i) (nth v j)]))))

(defn world
  "A world over a network of agents, using params for every interaction."
  ([agents] (world agents agent/default-params))
  ([agents params]
   {:agents  (into {} (map (juxt :my-id identity)) agents)
    :network (complete-network (map :my-id agents))
    :params  params
    :tick    0}))

(defn play-edge
  "Play the edge [i j] on the current agents, writing both endpoints back.
  pays-out? controls whether the round pays out."
  ([w edge] (play-edge w edge true))
  ([w [i j] pays-out?]
   (let [{:keys [agents params]} w
         [a b] (agent/play-round (agents i) (agents j) params pays-out?)]
     (update w :agents assoc i a j b))))

(defn h-counts
  "[H-count total-count] over every agent's most-recent directed action."
  [s]
  (let [latest (for [a (vals (:agents s))
                     m (vals (:memory a))
                     :let [la (:last-action m)]
                     :when la]
                 la)]
    [(count (filter #{:H} latest))
     (count latest)]))

(defn- fresh-agent
  [id rng {:keys [high-type low-type age-min age-max postpone]
           hire-p :probability-hire-HH-type}]
  (let [type (if (< (.nextDouble rng) (double hire-p)) high-type low-type)
        age  (+ age-min (.nextInt rng (inc (- age-max age-min))))]
    (assoc (agent/agent id type)
           :age age
           :age-hired age
           :delay-counter postpone
           :payoff-this-year 0
           :payoff-between-rewards 0
           :window-hh 0
           :window-ll 0
           :retired? false)))

(defn society
  "An n-agent society on a complete network, seeded for reproducibility."
  ([n] (society n default-params 0))
  ([n params seed]
   (let [rng (java.util.Random. seed)
         agents (for [id (range n)]
                  (fresh-agent id rng params))]
     (-> (world agents params)
         (assoc :seed (.nextLong rng))))))

(defn reinit-year [s]
  (update s :agents update-vals #(assoc % :payoff-this-year 0)))

(defn hire
  "Replace flagged-retired agents with a fresh stranger.
   Remove other agents' memory of the retired agents."
  [s rng]
  (let [retired (->> (:agents s) keys (filter #(get-in s [:agents % :retired?])) sort)]
    (if (empty? retired)
      s
      (let [gone (set retired)
            replaced (into {} (map (fn [id] [id (fresh-agent id rng (:params s))])) retired)]
        (update s :agents
                (fn [as]
                  (into {}
                        (map (fn [[id a]]
                               [id (or (replaced id)
                                       (update a :memory #(apply dissoc % gone)))]))
                        as)))))))

(defn mandatory-retire [s]
  (let [limit (get-in s [:params :age-of-retirement])
        set-retired #(cond-> %
                       (> (:age %) limit) (assoc :retired? true))]
    (update s :agents update-vals set-retired)))

(defn early-retire
  "Flag the lowest yearly earners below the quantile cut, except agents still
  within their postpone window. Everyone's shield counts down a year."
  [s]
  (let [agents  (:agents s)
        payoffs (vec (sort (map #(:payoff-this-year % 0) (vals agents))))
        n       (count payoffs)
        pos     (min (dec n) (Math/round (* (double (get-in s [:params :quantile])) n)))
        cutoff  (nth payoffs pos)]
    (update s :agents update-vals
            (fn [a]
              (let [dc (:delay-counter a 0)]
                (cond-> a
                  (and (< dc 1) (< (:payoff-this-year a 0) cutoff)) (assoc :retired? true)
                  (pos? dc) (update :delay-counter dec)))))))

(defn- play-year-edges [s rng]
  (let [p (double (get-in s [:params :collab-probability] 1.0))]
    (-> (reduce (fn [w e]
                  (let [pays-out? (< (.nextDouble rng) p)]
                    (play-edge w e pays-out?)))
                s
                (sort (:network s)))
        (update :tick inc))))

(defn- grow-old [s]
  (update s :agents update-vals #(update % :age inc)))

(defn- rewards-enabled? [params]
  (boolean (:reward-pct params)))

(defn- reward-maxes
  "Society-wide max of this window's HH and LL exchange counts - the
  denominators of each agent's per-exchange reward/sanction probability."
  [s]
  {:max-hh (reduce max 0 (map #(:window-hh % 0) (vals (:agents s))))
   :max-ll (reduce max 0 (map #(:window-ll % 0) (vals (:agents s))))})

(defn- reward-tick?
  "True on years that close a reward period (every :reward-frequency years)."
  [s]
  (let [{:keys [reward-frequency] :as params} (:params s)]
    (and (rewards-enabled? params)
         (pos? (long reward-frequency))
         (zero? (mod (inc (:tick s)) (long reward-frequency))))))

(defn apply-rewards
  "Close a reward period: raise each agent's yearly and total payoff by :reward-pct
   of its window payoff per HH share, cut it by :sanction-pct per LL share, then clear the window."
  [s]
  (let [{:keys [reward-pct sanction-pct]} (:params s)
        {:keys [max-hh max-ll]} (reward-maxes s)]
    (update s :agents update-vals
            (fn [a]
              (let [p-hh (if (pos? max-hh) (min 1.0 (/ (double (:window-hh a 0)) max-hh)) 0.0)
                    p-ll (if (pos? max-ll) (min 1.0 (/ (double (:window-ll a 0)) max-ll)) 0.0)
                    pay  (:payoff-between-rewards a 0)
                    adj  (- (* (double reward-pct) p-hh pay)
                            (* (double sanction-pct) p-ll pay))]
                (-> a
                    (update :total-payoff + adj)
                    (update :payoff-this-year (fnil + 0) adj)
                    (assoc :window-hh 0 :window-ll 0 :payoff-between-rewards 0)))))))

(defn sim-year
  "Runs one year/tick of simulation on given society."
  [s]
  (let [rng (java.util.Random. (:seed s))
        rt? (reward-tick? s)
        s   (cond-> s
              (rewards-enabled? (:params s))
              (update :params assoc :reward-tick? rt? :reward-maxes (reward-maxes s)))]
    (-> s
        (reinit-year)
        (hire rng)
        (play-year-edges rng)
        (grow-old)
        (mandatory-retire)
        (cond-> rt? apply-rewards)
        (early-retire)
        (assoc :seed (.nextLong rng)))))

(defn type-counts
  "Map of type -> number of living agents."
  [s]
  (->> (vals (:agents s))
       (map :type-of-academic)
       (frequencies)))
