(ns low-doers.types
  "Enumerate player types and classify by mindedness/selfishness.")

(defn rank
  "Ordinal rank of an own-profile in an ordering (0 = worst, 3 = best)."
  [ordering profile]
  (first (keep-indexed #(when (= %2 profile) %1) ordering)))

(defn high-minded?
  "Rank of mutual-high beats rank of mutual-low."
  [ordering]
  (> (rank ordering [:H :H]) (rank ordering [:L :L])))

(defn selfish?
  "Best outcome is giving L while receiving H."
  [ordering]
  (= (last ordering) [:L :H]))

(defn classify [ordering]
  {:ordering   (vec ordering)
   :mindedness (if (high-minded? ordering) :high :low)
   :selfish?   (selfish? ordering)})

(def all-types
  "All 24 strict preference orderings (Table 2), chain = worst to best."
  [{:no 1  :name :hs1 :chain [[:H :L] [:L :L] [:H :H] [:L :H]]}
   {:no 2  :name :hs2 :chain [[:L :L] [:H :L] [:H :H] [:L :H]]}
   {:no 3  :name :ls1 :chain [[:H :L] [:H :H] [:L :L] [:L :H]]}
   {:no 4  :name :ls2 :chain [[:H :H] [:H :L] [:L :L] [:L :H]]}
   {:no 5  :name :hs3 :chain [[:L :L] [:H :H] [:H :L] [:L :H]]}
   {:no 6  :name :ls3 :chain [[:H :H] [:L :L] [:H :L] [:L :H]]}
   {:no 7  :name :hn1 :chain [[:L :L] [:L :H] [:H :L] [:H :H]]}
   {:no 8  :name :hn2 :chain [[:L :H] [:L :L] [:H :L] [:H :H]]}
   {:no 9  :name :hn3 :chain [[:L :L] [:H :L] [:L :H] [:H :H]]}
   {:no 10 :name :hn4 :chain [[:H :L] [:L :L] [:L :H] [:H :H]]}
   {:no 11 :name :hn5 :chain [[:L :H] [:H :L] [:L :L] [:H :H]]}
   {:no 12 :name :hn6 :chain [[:H :L] [:L :H] [:L :L] [:H :H]]}
   {:no 13 :name :ln1 :chain [[:L :H] [:H :L] [:H :H] [:L :L]]}
   {:no 14 :name :ln2 :chain [[:H :L] [:L :H] [:H :H] [:L :L]]}
   {:no 15 :name :ln3 :chain [[:L :H] [:H :H] [:H :L] [:L :L]]}
   {:no 16 :name :ln4 :chain [[:H :H] [:L :H] [:H :L] [:L :L]]}
   {:no 17 :name :ln5 :chain [[:H :L] [:H :H] [:L :H] [:L :L]]}
   {:no 18 :name :ln6 :chain [[:H :H] [:H :L] [:L :H] [:L :L]]}
   {:no 19 :name :ln7 :chain [[:L :H] [:H :H] [:L :L] [:H :L]]}
   {:no 20 :name :ln8 :chain [[:H :H] [:L :H] [:L :L] [:H :L]]}
   {:no 21 :name :hn7 :chain [[:L :H] [:L :L] [:H :H] [:H :L]]}
   {:no 22 :name :hn8 :chain [[:L :L] [:L :H] [:H :H] [:H :L]]}
   {:no 23 :name :ln9 :chain [[:H :H] [:L :L] [:L :H] [:H :L]]}
   {:no 24 :name :hn9 :chain [[:L :L] [:H :H] [:L :H] [:H :L]]}])

(def name->chain
  (into {} (map (juxt :name :chain)) all-types))

;; Agent types: payoff = ground + cumulative gaps. Chain is derived from all-types;
;; variants use :base to reference their canonical ordering.
(def types
  {:hs1    {:doc   "selfish, high-minded: HL < LL < HH < LH (canonical)"
            :ground 1 :gaps [1 1 1]}
   :ls1    {:doc   "selfish, low-minded: HL < HH < LL < LH (canonical)"
            :ground 1 :gaps [1 1 1]}
   ;; Table 4 robustness config (NetLogo experiment e1_4): hs1 receives a
   ;; higher LL payoff than ls1, violating Proposition 1 condition (d). The
   ;; simulation shows ls1 still dominates despite the prop-1 failure.
   :hs1-t4 {:doc   "Table 4 hs1: HL=1 LL=8 HH=9 LH=10"
            :base :hs1 :ground 1 :gaps [7 1 1]}
   :ls1-t4 {:doc   "Table 4 ls1: HL=1 HH=4 LL=7 LH=10"
            :base :ls1 :ground 1 :gaps [3 3 3]}
   ;; "heroes/saints": non-selfish high-minded types. Swapping hs1 -> hn1/hn2
   ;; (leaving ls1 unaltered) yields a 50% H-action equilibrium - a hero plays H
   ;; no matter what, so it never capitulates to a low-doer.
   :hn1    {:doc   "Table 2 type 7 hn1: LL=1 LH=2 HL=3 HH=4 (LL < LH < HL < HH)"
            :ground 1 :gaps [1 1 1]}
   :hn2    {:doc   "Table 2 type 8 hn2: LH=1 LL=2 HL=3 HH=4 (LH < LL < HL < HH)"
            :ground 1 :gaps [1 1 1]}})

(defn payoff-table
  "Own-profile -> payoff map for a registered type key."
  [type-key]
  (let [{:keys [base ground gaps]} (types type-key)
        chain (name->chain (or base type-key))]
    (zipmap chain (reductions + ground gaps))))

(defn own-payoff
  "Payoff to a type-key player who played mine against opp."
  [type-key mine opp]
  ((payoff-table type-key) [mine opp]))

(defn payoffs [t1 t2 [a1 a2]]
  [(own-payoff t1 a1 a2)
   (own-payoff t2 a2 a1)])

(defn baseline-action
  "The action a type plays by default: H when HH > LL, L otherwise."
  [type-key]
  (let [pt (payoff-table type-key)]
    (if (> (pt [:H :H]) (pt [:L :L]))
      :H
      :L)))
