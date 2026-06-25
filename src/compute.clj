(ns compute
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [low-doers.metrics :as metrics]
            [low-doers.society :as society]))

(def years 1500)
(def seeds (range 5))
(def sizes [10 20 30 40 50]) ;; swept by every figure

(def trajectory-agents 20)

(def collect-memo
  (memoize (fn [n params seed years]
             (metrics/collect (society/society n params seed) years))))

(defn- h-rate [n params]
  (or (:steady-h (metrics/summary n params years seeds))
      0.0))

(defn trajectory
  "Per-year H-fraction and low-doer population share, over 1500 years for 20-agent society, one line per seed."
  []
  (tap> (str "trajectory (" trajectory-agents " agents, " (count seeds) " seeds, " years " years)..."))
  (vec
    (for [seed seeds
          {:keys [year h-fraction hs1 ls1]}
          (:snapshots (collect-memo trajectory-agents society/default-params seed years))
          :let [pop (+ hs1 ls1)]]
      {:year year
       :h-fraction h-fraction
       :ls-share (if (zero? pop) 0.0 (double (/ ls1 pop)))
       :seed (str seed)})))

(defn figure-2
  "H-action rate by society size, sweeping change-of-strategy threshold and postpone (15% quantile)."
  []
  (tap> (str "figure 2 (" (* (count sizes) 3 2) " cells * " (count seeds) " seeds * " years " years - using all cores)..."))
  (vec
    (pmap (fn [[n threshold postpone]]
            (tap> (str "  n=" n " threshold=" threshold " postpone=" postpone))
            (let [params (assoc society/default-params
                                :change-of-strategy-threshold threshold
                                :postpone postpone)]
              {:n         n
               :threshold (str "t=" threshold)
               :postpone  postpone
               :h-rate    (h-rate n params)}))
          (for [n sizes threshold [1 6 12] postpone [0 5]]
            [n threshold postpone]))))

(defn figure-3
  "Figure 2 under modified Table 4 payoffs at the 5% quantile (breaks Prop 1's condition d)."
  []
  (tap> (str "figure 3 (modified Table 4 payoffs, 5% quantile) - " (* (count sizes) 3 2) " cells * " (count seeds) " seeds * " years " years..."))
  (vec
    (pmap (fn [[n threshold postpone]]
            (tap> (str "  n=" n " threshold=" threshold " postpone=" postpone))
            (let [params (assoc society/default-params
                                :high-type :hs1-t4
                                :low-type :ls1-t4
                                :quantile 0.05
                                :change-of-strategy-threshold threshold
                                :postpone postpone)]
              {:n         n
               :threshold (str "t=" threshold)
               :postpone  postpone
               :h-rate    (h-rate n params)}))
          (for [n sizes threshold [1 6 12] postpone [0 5]]
            [n threshold postpone]))))

(defn figure-4
  "Effect of hiring policy: H-action rate when hiring 70% vs 90% hs1 (postpone=5)."
  []
  (tap> "figure 4...")
  (vec
    (pmap (fn [[n hire-p threshold]]
            (let [params (assoc society/default-params
                                :probability-hire-HH-type hire-p
                                :change-of-strategy-threshold threshold
                                :postpone 5)]
              {:n         n
               :hire-p    (str (int (* 100 hire-p)) "% hs1")
               :threshold (str "t=" threshold)
               :h-rate    (h-rate n params)}))
          (for [n sizes
                hire-p [0.7 0.9]
                threshold [1 6 12]]
            [n hire-p threshold]))))

(defn hero-trajectory
  "Section 4.8 trajectory: per-year H-fraction for hs1/hn1/hn2 (each paired with ls1),
  one line per type per seed."
  []
  (tap> "Section 4.8 hero/saint vs hs1 baseline trajectory...")
  (vec
    (for [high-type [:hs1 :hn1 :hn2]
          seed seeds
          :let [snapshots (:snapshots (collect-memo trajectory-agents
                                                      (assoc society/default-params :high-type high-type)
                                                      seed years))]
          {:keys [year h-fraction]} snapshots]
      {:year       year
       :h-fraction h-fraction
       :type       (name high-type)
       :seed       (str seed)})))

(defn- steady-h-seed
  "Pooled steady-state H-fraction (H actions / total actions) over the final tail years, for a single seed."
  [n params seed tail]
  (let [snaps (filter #(>= (:year %) (- years tail))
                      (:snapshots (metrics/collect (society/society n params seed) years)))
        h     (reduce + (map :h snaps))
        tot   (reduce + (map :n snaps))]
    (if (pos? tot) (/ (double h) tot) 0.0)))

(defn- mean-ci
  "[mean half-width-of-95%-CI] of xs (normal approximation, 1.96*SE)."
  [xs]
  (let [n (count xs)
        m (/ (reduce + xs) (double n))
        var (if (> n 1)
              (/ (reduce + (map #(let [d (- % m)] (* d d)) xs)) (dec n))
              0.0)]
    [m (* 1.96 (Math/sqrt (/ var n)))]))

(defn rewards-table
  "Tables 5-8: H-fraction (%) for a 30-agent society (15% quantile, threshold 6)
  sweeping LL-sanction x HH-reward at a fixed reward frequency and hs1-hiring probability."
  [{:keys [freq hire-p]}]
  (tap> (str "rewards table (freq=" freq " hire=" hire-p ")..."))
  (vec
   (pmap (fn [[sanction reward]]
           (tap> (str "  freq=" freq " hire=" hire-p " sanction=" sanction " reward=" reward))
           (let [params (assoc society/default-params
                               :change-of-strategy-threshold 6
                               :quantile 0.15
                               :probability-hire-HH-type hire-p
                               :reward-frequency freq
                               :reward-pct reward
                               :sanction-pct sanction)
                 [m ci] (mean-ci (map #(steady-h-seed 30 params % 300) seeds))]
             {:freq     freq
              :hire     (int (* 100 hire-p))
              :sanction (* 100.0 sanction)
              :reward   (* 100.0 reward)
              :h        (* 100.0 m)
              :ci       (* 100.0 ci)}))
         (for [sanction [0.0 0.5 1.0] reward [1.0 0.5]]
           [sanction reward]))))

(defn retirements
  "Raw per-agent retirement records (type, tenure, age, reason)."
  []
  (tap> "retirements (20 agents, 5 seeds, 1500 years, postpone=5)...")
  (let [params (assoc society/default-params :postpone 5)]
    (vec
     (for [seed seeds
           {:keys [type tenure age reason]}
           (:retirements (metrics/collect (society/society trajectory-agents params seed) years))]
       {:type (name type) :tenure tenure :age age :reason (name reason) :seed (str seed)}))))

(def ^:private all-figures
  {"trajectory"      #(trajectory)
   "retirements"     #(retirements)
   "figure-2"        #(figure-2)
   "figure-3"        #(figure-3)
   "figure-4"        #(figure-4)
   "hero-trajectory" #(hero-trajectory)
   "table-5"         #(rewards-table {:freq 3 :hire-p 0.5})
   "table-6"         #(rewards-table {:freq 1 :hire-p 0.5})
   "table-7"         #(rewards-table {:freq 3 :hire-p 0.65})
   "table-8"         #(rewards-table {:freq 1 :hire-p 0.65})})

(defn- write-data [updates]
  (let [path "notebooks/data.edn"
        existing (try (edn/read-string (slurp path))
                      (catch Exception _ {}))
        merged (merge existing updates)]
    (spit path (with-out-str (pprint/pprint merged)))
    (tap> (str "wrote " (str/join ", " (map name (keys updates))) " -> " path))))

(defn -main [& [figure & _]]
  (add-tap println)
  (if figure
    (if-let [f (get all-figures figure)]
      (write-data {(keyword figure) (f)})
      (do (tap> (str "unknown figure: " figure ". known: " (str/join ", " (keys all-figures))))
          (System/exit 1)))
    (write-data (into {} (map (fn [[k f]] [(keyword k) (f)]) all-figures))))
  (System/exit 0))

(comment
  (add-tap println)
  (figure-4)
  (hero-trajectory))
