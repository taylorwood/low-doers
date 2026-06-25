(ns low-doers.society-test
  (:require [clojure.test :refer [deftest is testing]]
            [low-doers.agent :as agent]
            [low-doers.society :as society]))

(defn soc
  "A society over the given agent maps, with default params and seed 0."
  [agents]
  (assoc (society/world agents society/default-params) :seed 0))

(defn agent+
  "An agent of type carrying the extra society fields."
  [id type extra]
  (merge (agent/agent id type)
         {:age 30 :age-hired 30 :delay-counter 0
          :payoff-this-year 0 :retired? false}
         extra))

(deftest reinit-year-zeroes-yearly-payoff
  (let [s (soc [(agent+ 1 :hs1 {:payoff-this-year 42})
                (agent+ 2 :ls1 {:payoff-this-year 17})])]
    (is (every? zero? (map :payoff-this-year (vals (:agents (society/reinit-year s))))))))

(deftest mandatory-retire-flags-only-past-the-limit
  (testing "age strictly greater than :age-of-retirement (65) is flagged"
    (let [s  (soc [(agent+ 1 :hs1 {:age 65}) (agent+ 2 :ls1 {:age 66})])
          s1 (society/mandatory-retire s)]
      (is (false? (get-in s1 [:agents 1 :retired?])))
      (is (true?  (get-in s1 [:agents 2 :retired?]))))))

(deftest early-retire-culls-lowest-earners
  (testing "the bottom-quantile earner is flagged, the rest survive"
    (let [s  (-> (soc [(agent+ 1 :hs1 {:payoff-this-year 1})
                       (agent+ 2 :ls1 {:payoff-this-year 100})
                       (agent+ 3 :ls1 {:payoff-this-year 100})
                       (agent+ 4 :ls1 {:payoff-this-year 100})])
                 (assoc-in [:params :quantile] 0.25))
          s1 (society/early-retire s)]
      (is (true? (get-in s1 [:agents 1 :retired?])))
      (is (not-any? true? (map #(get-in s1 [:agents % :retired?]) [2 3 4]))))))

(deftest early-retire-shields-and-decrements-postpone-window
  (testing "a low earner still inside its postpone window is spared, dc counts down"
    (let [s  (-> (soc [(agent+ 1 :hs1 {:payoff-this-year 1 :delay-counter 3})
                       (agent+ 2 :ls1 {:payoff-this-year 100})
                       (agent+ 3 :ls1 {:payoff-this-year 100})
                       (agent+ 4 :ls1 {:payoff-this-year 100})])
                 (assoc-in [:params :quantile] 0.25))
          s1 (society/early-retire s)]
      (is (false? (get-in s1 [:agents 1 :retired?])))
      (is (= 2 (get-in s1 [:agents 1 :delay-counter]))))))

(deftest hire-replaces-retirees-and-scrubs-memory
  (testing "a flagged agent becomes a fresh stranger; survivors forget it"
    (let [s   (-> (soc [(agent+ 1 :hs1 {:age 70 :retired? true
                                        :memory {2 {:last-action :H}}})
                        (agent+ 2 :hs1 {:memory {1 {:last-action :H}
                                                 3 {:last-action :H}}})
                        (agent+ 3 :ls1 {:memory {1 {:last-action :L}
                                                 2 {:last-action :L}}})]))
          s1  (society/hire s (java.util.Random. 0))
          a1' (get-in s1 [:agents 1])]
      ;; slot 1 is a brand-new hire: empty memory, not retired, fresh age window
      (is (false? (:retired? a1')))
      (is (empty? (:memory a1')))
      (is (<= (get-in s [:params :age-min]) (:age a1') (get-in s [:params :age-max])))
      ;; survivors no longer remember the departed agent 1
      (is (nil? (get-in s1 [:agents 2 :memory 1])))
      (is (nil? (get-in s1 [:agents 3 :memory 1])))
      ;; but their memory of each other is untouched
      (is (= :H (get-in s1 [:agents 2 :memory 3 :last-action]))))))

(deftest hire-is-no-op-without-retirees
  (let [s (soc [(agent+ 1 :hs1 {}) (agent+ 2 :ls1 {})])]
    (is (= s (society/hire s (java.util.Random. 0))))))

(deftest strategies-are-per-partner-in-network
  (testing "an hs1 cooperates with another hs1 but capitulates to an ls1"
    (let [w (nth (iterate #(reduce society/play-edge % (:network %))
                          (society/world [(agent/agent 1 :hs1) (agent/agent 2 :hs1) (agent/agent 3 :ls1)]))
                 20)
          a1 (get-in w [:agents 1])
          a3 (get-in w [:agents 3])]
      ;; hs1 #1 keeps HH with hs1 #2, but plays L against the exploiter #3
      (is (= :H (get-in a1 [:memory 2 :last-action])))
      (is (= :L (get-in a1 [:memory 3 :last-action])))
      ;; the ls1 never moves off L against anyone
      (is (= :L (get-in a3 [:memory 1 :last-action])))
      (is (= :L (get-in a3 [:memory 2 :last-action]))))))
