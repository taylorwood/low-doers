(ns low-doers.agent-test
  (:require [clojure.test :refer [deftest is testing]]
            [low-doers.agent :as agent]))

(deftest hs1-capitulates-to-exploiter
  (testing "exploited hs1 switches H->L; ls1 never moves (low-doer dominance)"
    (let [[hs ls] (agent/play-rounds (agent/agent 1 :hs1) (agent/agent 2 :ls1) 5)]
      ;; hs1 plays H once (shortfall builds, balance goes negative), then defects
      ;; to L for good; ls1 plays L throughout.
      (is (= [:H :L :L :L :L] (get-in hs [:memory 2 :actions])))
      (is (= [:L :L :L :L :L] (get-in ls [:memory 1 :actions])))
      ;; ls1 is never below its LL baseline, so it never reconsiders.
      (is (= [:L] (distinct (get-in ls [:memory 1 :actions]))))
      ;; the low-doer comes out ahead every round of the run.
      (is (< (:total-payoff hs) (:total-payoff ls)))
      (is (= 9 (:total-payoff hs)))    ; 1 + 2+2+2+2
      (is (= 16 (:total-payoff ls))))))  ; 4 + 3+3+3+3

(deftest switch-resets-accumulators
  (testing "balance/shortfall zero out at the moment of a switch"
    (let [[hs _] (agent/play-rounds (agent/agent 1 :hs1) (agent/agent 2 :ls1) 2)
          m (get-in hs [:memory 2])]
      (is (= :L (:last-action m)))
      ;; round 2 switched H->L, reset to 0, then booked LL: opt 3 - payoff 2 = 1
      (is (= 1 (:difference-from-optimal m)))
      ;; balance after reset: payoff 2 - counterfactual p_hs1(H,L)=1 = +1
      (is (= 1 (:balance m))))))
