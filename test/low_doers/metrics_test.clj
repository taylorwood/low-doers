(ns low-doers.metrics-test
  (:require [clojure.test :refer [deftest is testing]]
            [low-doers.society :as society]
            [low-doers.metrics :as metrics]))

(deftest reproduces-paper-findings
  ;; a 20-agent society over 1500 ticks (Figure 2 left / experiment e1, postpone = 0).
  (let [years 1500
        rs    (metrics/rows 20 society/default-params years [0 1])]
    (testing "low-doers take the majority"
      (is (> (metrics/final-count rs years :ls1)
             (metrics/final-count rs years :hs1))))
    (testing "H actions fall below 20% (cumulative and in steady state)"
      (is (< (metrics/cumulative-h rs) 0.20))
      (is (< (metrics/steady-h rs years 300) 0.20)))
    (testing "surviving low-doers out-last high-doers (time before retirement)"
      (is (> (metrics/mean-tenure rs :ls1) (metrics/mean-tenure rs :hs1))))
    (testing "ls1 mean retirement age exceeds hs1"
      (is (> (metrics/mean-age rs :ls1) (metrics/mean-age rs :hs1))))
    (testing "high-doers leave mostly by early (quantile) exit, not mandatory age"
      (let [c (metrics/retire-counts rs)]
        (is (> (get c [:hs1 :early] 0) (get c [:hs1 :mandatory] 0)))))))

(deftest hiring-more-high-doers-slows-the-takeover
  (testing "raising :hire-p leaves more high-doers alive at the end"
    (let [years   200
          final-h (fn [hire-p]
                    (let [p (assoc society/default-params :probability-hire-HH-type hire-p)]
                      (metrics/final-count (metrics/rows 30 p years [0 1]) years :hs1)))]
      (is (< (final-h 0.5) (final-h 0.9))))))
