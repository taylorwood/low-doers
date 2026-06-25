(ns low-doers.agent
  "Agents and the repeated HL game with strategy adaptation."
  (:refer-clojure :exclude [agent])
  (:require [low-doers.types :as typ]))



(def default-params {:change-of-strategy-threshold 1})

(defn agent
  "A fresh agent of type (e.g. :hs1, :ls1) with the given id."
  [id type]
  {:my-id id :type-of-academic type :age 0 :total-payoff 0 :memory {}})

(defn- reconsidering? [baseline shortfall threshold]
  (and (= baseline :H) (>= shortfall threshold)))

(defn- other [a]
  (if (= a :H) :L :H))

(defn- reward-prob
  "Per-exchange reward/sanction probability for an agent (Appendix B): its share
  of XX exchanges this window over the society-wide maximum, clamped to [0,1]."
  [n maxn]
  (if (and maxn (pos? maxn)) (min 1.0 (/ (double n) maxn)) 0.0))

(defn- expected-rs
  "Expected reward (HH) or sanction (LL) for own action `mine` against `opp`:
  payoff for that profile scaled by the matching percentage and per-exchange prob.
  Only mutual-high earns a reward and only mutual-low draws a sanction."
  [t mine opp p-hh p-ll reward-pct sanction-pct]
  (cond
    (and (= mine :H) (= opp :H)) (* reward-pct p-hh (typ/own-payoff t :H :H))
    (and (= mine :L) (= opp :L)) (- (* sanction-pct p-ll (typ/own-payoff t :L :L)))
    :else 0.0))

(defn- reward-balance
  "Appendix B reconsider term: summed over the exchanges with this partner since
  the last switch, the expected reward/sanction for the actions actually played
  minus that for the counterfactual (opposite) actions. Adding it to BALANCE lets
  a standing LL-sanction pull a capitulated agent back to H (paper note 29)."
  [t recon-exch p-hh p-ll reward-pct sanction-pct]
  (reduce-kv (fn [acc [mine opp] cnt]
               (+ acc (* cnt (- (expected-rs t mine opp p-hh p-ll reward-pct sanction-pct)
                                (expected-rs t (other mine) opp p-hh p-ll reward-pct sanction-pct)))))
             0.0 recon-exch))

(defn decide
  "Action agent plays against partner-id: baseline until shortfall threshold, then
  balance-gated switch. On a reward tick the BALANCE also weighs the expected
  reward/sanction differential (Appendix B reconsider)."
  [agent partner-id {threshold :change-of-strategy-threshold
                     :keys [reward-tick? reward-pct sanction-pct reward-maxes]}]
  (let [t    (:type-of-academic agent)
        base (typ/baseline-action t)
        m    (get-in agent [:memory partner-id])
        last (:last-action m base)
        reconsider (reconsidering? base (:difference-from-optimal m 0) threshold)]
    (if reconsider
      (let [rb (if reward-tick?
                 (reward-balance t (:recon-exch m {})
                                 (reward-prob (:window-hh agent 0) (:max-hh reward-maxes))
                                 (reward-prob (:window-ll agent 0) (:max-ll reward-maxes))
                                 (double (or reward-pct 0)) (double (or sanction-pct 0)))
                 0.0)]
        (if (neg? (+ (:balance m 0) rb))
          (other last)
          last))
      base)))

(defn- record-exchange
  [agent partner-id act partner-act payoff pays-out?]
  (let [base    (typ/baseline-action (:type-of-academic agent))
        m       (get-in agent [:memory partner-id])
        switch? (not= act (:last-action m base))
        balance0 (if switch? 0 (:balance m 0))
        short0  (if switch? 0 (:difference-from-optimal m 0))
        recon0  (if switch? {} (:recon-exch m {}))]
    (if pays-out?
      (let [t       (:type-of-academic agent)
            counter (typ/own-payoff t (other act) partner-act)
            optimal (typ/own-payoff t base base)]
        (-> agent
            (update :total-payoff + payoff)
            (update :payoff-this-year (fnil + 0) payoff)
            (update :payoff-between-rewards (fnil + 0) payoff)
            (cond-> (and (= act :H) (= partner-act :H)) (update :window-hh (fnil inc 0)))
            (cond-> (and (= act :L) (= partner-act :L)) (update :window-ll (fnil inc 0)))
            (update-in [:memory partner-id]
                       (fn [pm]
                         (-> (or pm {})
                             (update :actions (fnil conj []) act)
                             (update :payoffs (fnil conj []) payoff)
                             (assoc :last-action act
                                    :balance   (+ balance0 (- payoff counter))
                                    :difference-from-optimal (+ short0 (- optimal payoff))
                                    :recon-exch (update recon0 [act partner-act] (fnil inc 0))))))))
      ;; inactive: commit only strategy (last action and any reset); no payoff, no growth of recorded history
      (assoc-in agent [:memory partner-id]
                (assoc (or m {}) :last-action act :balance balance0
                       :difference-from-optimal short0 :recon-exch recon0)))))

(defn play-round
  "One simultaneous HL round; pays-out? gates payouts (strategy always updates)."
  ([a1 a2] (play-round a1 a2 default-params true))
  ([a1 a2 params] (play-round a1 a2 params true))
  ([a1 a2 params pays-out?]
   (let [id1 (:my-id a1) id2 (:my-id a2)
         act1 (decide a1 id2 params)
         act2 (decide a2 id1 params)
         [p1 p2] (typ/payoffs (:type-of-academic a1) (:type-of-academic a2) [act1 act2])]
     [(record-exchange a1 id2 act1 act2 p1 pays-out?)
      (record-exchange a2 id1 act2 act1 p2 pays-out?)])))

(defn play-rounds
  "Play n repeated rounds between two agents, threading their evolving state.
  Returns [a1' a2'] after the last round."
  ([a1 a2 n] (play-rounds a1 a2 n default-params))
  ([a1 a2 n params]
   (reduce (fn [[x y] _] (play-round x y params)) [a1 a2] (range n))))

(comment
  ;; hs1 is exploited, then capitulates to L; ls1 never budges.
  (let [[hs ls] (play-rounds (agent 1 :hs1) (agent 2 :ls1) 5)]
    [(get-in hs [:memory 2 :actions])   ; [:H :L :L :L :L]
     (get-in ls [:memory 1 :actions])   ; [:L :L :L :L :L]
     (:total-payoff hs)            ; 1 + 2*4 = 9
     (:total-payoff ls)])          ; 4 + 3*4 = 16
  )
