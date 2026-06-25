(ns fundamentals
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :fold}}
  (:require [nextjournal.clerk :as clerk]
            [low-doers.types :as typ]
            [low-doers.agent :as agent]))

;; # H/L Fundamentals
;; Basic mechanics of the Proietti & Franco (2018) model before the population
;; dynamics in the [figures notebook](figures.clj).

;; ## The Exchange
;; Agents independently choose how much effort to put in: **H** (high) or **L** (low).
;; Neither knows the other's choice in advance. The four possible outcomes for any agent:

(clerk/table
 {:head ["You" "Them" ""]
  :rows [["H" "H" "Quality work on both sides"]
         ["H" "L" "You submitted quality work, but they coasted"]
         ["L" "H" "You coasted, but they submitted quality work"]
         ["L" "L" "Lazy slop from both sides"]]})

;; ## Agent Types
;; Every agent has a *type*: a strict preference ordering over those four outcomes.
;; Proietti & Franco focus on two types to start: **hs1** and **ls1**. Both are *selfish*
;; (their top preference is free-riding — giving L while receiving H) but they differ in their
;; *mindedness* (which they'd prefer if selfishness isn't on the table).

(clerk/vl
 {:schema "https://vega.github.io/schema/vega-lite/v5.json"
  :title "Payoffs by type (higher = preferred)"
  :width 280 :height 120
  :data {:values (let [hs (typ/payoff-table :hs1)
                       ls (typ/payoff-table :ls1)]
                   (for [[type pt] [["hs1" hs] ["ls1" ls]]
                         [profile label] [[[:H :H] "HH"] [[:H :L] "HL"]
                                          [[:L :H] "LH"] [[:L :L] "LL"]]]
                     {:type type :profile label :payoff (pt profile)}))}
  :layer [{:mark "rect"
           :encoding {:x {:field "profile" :type "nominal"
                          :sort ["HL" "LL" "HH" "LH"]
                          :title "Outcome"}
                      :y {:field "type" :type "nominal" :title nil}
                      :color {:field "payoff" :type "quantitative"
                              :scale {:scheme "blues" :domain [1 4]}
                              :legend nil}}}
          {:mark {:type "text" :fontSize 16 :fontWeight "bold"}
           :encoding {:x {:field "profile" :type "nominal"
                          :sort ["HL" "LL" "HH" "LH"]}
                      :y {:field "type" :type "nominal"}
                      :text {:field "payoff" :type "quantitative"}
                      :color {:condition {:test "datum.payoff < 3" :value "#333"}
                              :value "white"}}}]})

;; **hs1** (high-minded) prefers mutual-high (HH) over mutual-low (LL). Starts out playing H.

;; **ls1** (low-minded) prefers mutual-low (LL) over mutual-high (HH). Starts out playing L.

;; The `:type-of-academic` field name is from the paper, which frames hs1/ls1 as academics:
;;
;; > Such agents are arguably likely to be found in a competitive society where individuals are
;; > incentivized to participate in many activities (for example improving their CV by publishing,
;; > teaching, participating to conferences and research projects) while at the same time
;; > economizing their efforts and getting the most out of them.

;; ## Example rounds
;; Agents can reconsider their strategy after each exchange. Each agent tracks two running
;; tallies per partner, reset whenever the agent changes strategy:
;; - **Shortfall**: cumulative gap between actual payoff and what both playing baseline would have earned
;; - **Balance**: running difference between actual payoff and what the *opposite* action would have earned with this partner
;;
;; When shortfall crosses a threshold, a negative balance triggers a switch in the agent's baseline strategy.

(def trace
  (->> (iterate (fn [[hs ls]] (agent/play-round hs ls))
                [(agent/agent 0 :hs1)
                 (agent/agent 1 :ls1)])
       (rest)
       (take 3)
       (map-indexed
        (fn [i [hs ls]]
          {:round     (inc i)
           :hs1       (name (get-in hs [:memory 1 :last-action]))
           :ls1       (name (get-in ls [:memory 0 :last-action]))
           :hs1-pay   (last (get-in hs [:memory 1 :payoffs]))
           :ls1-pay   (last (get-in ls [:memory 0 :payoffs]))
           :shortfall (get-in hs [:memory 1 :difference-from-optimal])
           :balance   (get-in hs [:memory 1 :balance])}))))

^{::clerk/visibility {:code :hide}}
(clerk/table
 {:head ["Round" "hs1" "ls1" "hs1 payoff" "ls1 payoff" "hs1 shortfall" "hs1 balance"]
  :rows (map (juxt :round :hs1 :ls1 :hs1-pay :ls1-pay :shortfall :balance) trace)})

;; **Round 1**: hs1 plays H, ls1 plays L. hs1 earns 1 (sucker); ls1 earns 4.
;; Shortfall +2, balance −1 — playing L against ls1's L would have yielded 2.
;; Shortfall ≥ threshold and balance < 0: hs1 switches to L.

;; **Round 2 onward**: both play L. The switch resets the counters. hs1 earns 2;
;; switching back to H would only earn 1 — stuck. The shortfall keeps growing
;; but the balance stays positive, so hs1 never switches back.

;; ## The Gap Widens
;; The asymmetry for the hs1 vs ls1 collaborations is apparent from the opening play:
;; hs1 starts at H and can fall short, but ls1 starts at L and in an LL exchange
;; *already earns its second-best outcome*. LL pays **ls1** three and hs1 only two.
;; The cumulative gap starts in round 1 and keeps growing:

(clerk/vl
 {:schema "https://vega.github.io/schema/vega-lite/v5.json"
  :title {:text "Cumulative payoffs: hs1 vs. ls1"
          :subtitle "hs1 capitulates after round 1; ls1 earns more in every round"}
  :width 500 :height 260
  :data {:values (loop [i 0
                        hs (agent/agent 0 :hs1)
                        ls (agent/agent 1 :ls1)
                        acc []]
                   (if (= i 30)
                     acc
                     (let [[hs' ls'] (agent/play-round hs ls)]
                       (recur (inc i) hs' ls'
                              (conj acc
                                    {:round (inc i) :type "hs1 (high-minded)" :payoff (:total-payoff hs')}
                                    {:round (inc i) :type "ls1 (low-doer)"    :payoff (:total-payoff ls')})))))}
  :mark {:type "line" :point true}
  :encoding {:x {:field "round" :type "quantitative" :title "Round"}
             :y {:field "payoff" :type "quantitative" :title "Cumulative payoff"}
             :color {:field "type" :type "nominal" :title nil
                     :scale {:domain ["hs1 (high-minded)" "ls1 (low-doer)"]
                             :range ["#e45756" "#4c78a8"]}}}})

;; These one-on-one exchanges are the core mechanic of the larger "social" simulation.

;; Continue reading the [figures notebook](figures.clj) to see what happens when:
;; 1. the H/L dynamic plays out in a society of many agents over many years
;; 1. different retirement and strategy thresholds are used
;; 1. different, non-selfish agent types are introduced with different preferences
;; 1. hiring filters and performance management (rewards, sanctions) are applied
