(ns figures
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :fold}}
  (:require [nextjournal.clerk :as clerk]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [low-doers.types :as types]
            [specs]))

;; # Social Norms and the Dominance of Low-Doers
;; This notebook reproduces figures from the Proietti & Franco (2018) paper.
;; See the [fundamentals notebook](fundamentals.clj) first for the basic mechanics.

;; Sections mentioning "Figure N" or "Table N" refer to the paper; others are supplementary.

;; Generate the data first (one-time, ~5-10 min):
;; ```sh
;; clojure -M:compute
;; ```

^{::clerk/visibility {:code :hide :result :hide}}
(def ^::clerk/no-cache data
  (-> "notebooks/data.edn" slurp edn/read-string))

;; ## 24 Player Types
;; All preference orderings over the four outcomes (HH, HL, LH, LL) become 24 possible types,
;; each classifiable along two axes: *selfishness* and *mindedness*. A type is **selfish** when
;; free-riding (LH) is its top choice, and **high-minded** when it ranks mutual-high (HH) above mutual-low (LL).

(clerk/table
 {:head ["Type" "Preferential order (worst to best)" "Selfishness" "Mindedness" "Name"]
  :rows (for [{:keys [no chain] nm :name} types/all-types
              :let [c (types/classify chain)]]
          [no (str/join " < " (map #(str/join (map name %)) chain))
           (if (:selfish? c) "selfish" "non-selfish")
           (name (:mindedness c)) (name nm)])})

;; Table 2 in the paper.

;; ## High-Quality (H) Actions Collapse
;; Given a society of 20 agents hired evenly from hs1/ls1, H actions begin around 20% of
;; exchanges, then collapse to a ~9% steady state within the first decade or two and remain
;; there for 1,500 simulated years. Time axis is log-scale to show early collapse and the long flat tail.

(clerk/vl (specs/h-collapse data))

;; Faint blue lines are each RNG seed's 5-year rolling mean; bold blue line is all-seed median.
;; 20 agents, 5 seeds over 1500 years.

;; ## Figure 2 - H-Action Rate by Society Size
;; The finding holds across different simulation parameters for society sizes and strategy-change thresholds.

(clerk/vl (specs/fig2 data))

;; Left: postpone = 0. Right: postpone = 5.
;; 15% quantile retirement, 50/50 hiring, 5 seeds each cell.

;; ## Figure 3 - Modified Payoff Distances (Table 4)
;; Table 4 breaks the payoff asymmetry by raising hs1's LL payoff *above* ls1's, so an hs1 stuck
;; in mutual-low no longer trails its partner as it does under normal payoffs — yet the simulated
;; outcome is largely unchanged. 5% quantile, threshold t={1, 6, 12}.
;; Left: postpone = 0. Right: postpone = 5 (paper section 4.7: H settles at 30-35%).
;;
;; NOTE: The *printed* Figure 3 in the JASSS PDF is (probably not supposed to be) a copy of Figure 2.
;; We reproduce the experiment the paper describes (Table 4, 5% quantile) so this figure intentionally differs from the paper.

(clerk/vl (specs/fig3 data))

;; ## Section 4.8 - Heroes & Saints Equilibrium
;; What works better than hiring *more* high-minded agents is changing *what kind*. These two
;; non-selfish, high-minded types protect the H equilibrium:
;; - **hero `hn1`**: always plays H, even as the sucker, though it still ranks free-riding (LH) on top
;; - **saint `hn2`**: same behavior, but goes further and ranks LH *below* LL, so it wouldn't even want to exploit
;;
;; Both types resist the capitulation mechanism entirely: since they always play H regardless, their
;; shortfall/balance never push them to capitulate. Swap hs1 for either and the H-rate hovers around 50%.
;; 20 agents, 50/50 hiring, 15% quantile, postpone = 0, threshold = 1, 5 seeds over 1500 years.
;;
;; > Under such conditions, the efficiency of an institution can be sustained if high-minded
;; > people are not selfish, we may call them "heroes" or "saints" (paper section 4.8).

(clerk/vl (specs/hero data))

;; ## Career Churn
;; The agent turnover driving the collapse shows up in career tenures and retirement reasons.

(clerk/vl (specs/tenure-pyramid data))

;; hs1 (left) piles up on the short end while ls1 (right) generally lasts longer.

(clerk/vl (specs/retirement-reason-pyramid data))

;; hs1 dominates the early-retirement side (bottom-% earners forced out under sustained low payoffs),
;; while agents that survive to mandatory retirement age reach it at similar tenures regardless of type.
;; ls1's longer careers come from rarely capitulating to H, not from outlasting hs1 once it does.

;; ## Effect of Hiring Policy
;; The first lever is the mix of agent types in the society. Unsurprisingly, hiring more hs1
;; agents helps, but as the paper notes:
;;
;; > Furthermore, it is quite challenging for a policy maker or employer to succeed in hiring
;; > such a high percentage of high-minded individuals.

(clerk/vl (specs/fig4 data))

;; Figure 4 in paper. 15% quantile, postpone = 5, threshold = 1, after 1500 years.
;; NOTE: like Figure 3 above, the printed Figure 4 in the JASSS PDF also appears to be a copy of Figure 2.

;; ## Rewards and Sanctions
;; The second lever leaves the agent mix alone and changes the *incentives*. As it turns out,
;; rewarding HH exchanges has little effect, but sanctioning LL exchanges pulls capitulated hs1
;; agents back to H: the looming penalty enters the reconsider calculation and tips the balance.

;; Frequency also matters: sanctions every round push H-rates above 65%, while sanctions every
;; three rounds barely help at all. Sanctions only fire when an agent is in range of a
;; reconsideration, so the more hs1 agents you start with, the more often the penalty has
;; something to penalize. Hiring 65% hs1 lifts the baseline and reduces the sanction-frequency
;; impact, recovering much of the sanction benefit even at the every-three-years cadence where
;; 50/50 hiring collapses.

^{::clerk/visibility {:result :hide}}
(defn rewards-view
  "Pivot a Table 5-8 data to match paper's layout: one row per LL-sanction,
  one column per HH-reward, cells `H% (95% CI)`."
  [rows]
  (let [by (group-by :sanction rows)
        cell (fn [s r]
               (let [d (first (filter #(= (double r) (:reward %)) (by (double s))))]
                 (format "%.2f (%.2f)" (:h d) (:ci d))))]
    (clerk/table
     {:head ["Punishment for LL exchanges" "Reward for HH = 100%" "Reward for HH = 50%"]
      :rows (for [s [0.0 50.0 100.0]]
              [(format "%.0f%%" s) (cell s 100.0) (cell s 50.0)])})))

;; Sanctioning LL is what helps. Frequent sanctions help more (bottom-left);
;; hiring more hs1 pushes the effect toward ~90% (bottom-right).
(clerk/vl (specs/rewards-sanctions data))

(rewards-view (:table-5 data))
;; Table 5: reward frequency 3, 50% hiring of hs1

;; With sanctions every round, a 50% LL-sanction alone pushes the H-rate past 65%.
(rewards-view (:table-6 data))
;; Table 6: reward frequency 1, 50% hiring of hs1

;; Hiring 65% hs1 lifts the baseline and recovers much of the sanction benefit
;; even at the slow `f = 3` frequency.
(rewards-view (:table-7 data))
;; Table 7: reward frequency 3, 65% hiring of hs1

;; With selective hiring and frequent, strong sanctions, the H-rate climbs to ~90%
;; — the most effective policy tested in the paper.
(rewards-view (:table-8 data))
;; Table 8: reward frequency 1, 65% hiring of hs1

;; All four tables fix a 30-agent society at a 15% quantile and change-of-strategy
;; threshold 6, sweeping the LL-sanction (0 / 50 / 100%) against the HH-reward
;; (100 / 50%). Values are the steady-state fraction of H exchanges (%) over the
;; final 300 of 1500 years; parentheses give the 95% CI over 5 seeds.
