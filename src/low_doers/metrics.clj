(ns low-doers.metrics
  "Calculates metrics over simulated agent interaction outcomes."
  (:require [low-doers.society :as society]))

(defn- retirees [s]
  (let [limit (get-in s [:params :age-of-retirement])]
    (for [a (vals (:agents s)) :when (:retired? a)]
      {:type   (:type-of-academic a)
       :age    (:age a)
       :tenure (- (:age a) (:age-hired a))
       :reason (if (> (:age a) limit) :mandatory :early)})))

(defn collect
  "Step society s for years, returning per-year `:snapshots`
  (H-fraction and type counts) and the flat list of `:retirements`."
  [s years]
  (loop [s s
         y 0
         snaps []
         retires []]
    (if (> y years)
      {:snapshots snaps :retirements retires}
      (let [{:keys [hs1 ls1]} (society/type-counts s)
            [h n] (society/h-counts s)]
        (recur (society/sim-year s)
               (inc y)
               (conj snaps {:year y
                            :h-fraction (if (zero? n) 0 (double (/ h n)))
                            :h h :n n
                            :hs1 (or hs1 0) :ls1 (or ls1 0)})
               (into retires (map #(assoc % :year y)) (retirees s)))))))

(defn rows
  "Flat seq of tagged maps from one run per seed over n agents, params, years."
  [n params years seeds]
  (mapcat (fn [seed]
            (let [{:keys [snapshots retirements]} (collect (society/society n params seed) years)]
              (concat (map #(assoc % :sim seed :kind :year) snapshots)
                      (map #(assoc % :sim seed :kind :retire) retirements))))
          seeds))

(defn- mean [xs]
  (when (seq xs) (/ (double (reduce + xs)) (count xs))))

(defn- year-rows [rs] (filter #(= :year (:kind %)) rs))
(defn- retire-rows [rs] (filter #(= :retire (:kind %)) rs))

(defn- h-frac [ys]
  (let [h (reduce + (map :h ys))
        n (reduce + (map :n ys))]
    (when (pos? n) (/ (double h) n))))

(defn- retire-attr-mean [rs type attr]
  (->> (retire-rows rs)
       (filter #(= type (:type %)))
       (map attr)
       (mean)))

(defn cumulative-h
  "H-fraction over all directed actions in the run - action-grain sum, not a mean of per-year fractions."
  [rs]
  (-> rs year-rows h-frac))

(defn steady-h
  "H-fraction over the final tail years - steady-state after the early transient."
  [rs max-year tail]
  (->> (year-rows rs)
       (filter #(>= (:year %) (- max-year tail)))
       (h-frac)))

(defn final-count
  "Mean number of living agents of type at the last collected year, averaged across sims."
  [rs max-year type]
  (->> (year-rows rs)
       (filter #(= max-year (:year %)))
       (map type)
       (mean)))

(defn mean-tenure
  "Mean years-before-retirement for type agents."
  [rs type]
  (retire-attr-mean rs type :tenure))

(defn mean-age
  "Mean age-at-retirement for type agents."
  [rs type]
  (retire-attr-mean rs type :age))

(defn retire-counts
  "Map of {[type reason] count} over all logged retirements."
  [rs]
  (->> (retire-rows rs)
       (map (juxt :type :reason))
       (frequencies)))

(defn summary
  "Reproduction summary for one experiment across seeds: steady-state H, mean
  final composition, and mean tenure (time-before-retirement) per type."
  [n params years seeds]
  (let [rs   (rows n params years seeds)
        tail (min years 300)]
    {:seeds        (count seeds)
     :probability-hire-HH-type (:probability-hire-HH-type params)
     :cumulative-h (cumulative-h rs)
     :steady-h     (steady-h rs years tail)
     :final        {:hs1 (final-count rs years :hs1)
                    :ls1 (final-count rs years :ls1)}
     :mean-tenure  {:hs1 (mean-tenure rs :hs1)
                    :ls1 (mean-tenure rs :ls1)}
     :mean-age     {:hs1 (mean-age rs :hs1)
                    :ls1 (mean-age rs :ls1)}}))

(comment
  ;; 20 agents, 1500 years, 10 seeds. H actions fall below 20%, low-doers take the majority, and
  ;; surviving low-doers out-last high-doers in both tenure and retirement age.
  (summary 20 society/default-params 1500 (range 10))
  ;; hire-p sweep: feeding in more high-doers slows the takeover.
  (for [hire-p [0.5 0.7 0.9]
        :let [params (assoc society/default-params :probability-hire-HH-type hire-p)]]
    (summary 50 params 1500 (range 3))))
