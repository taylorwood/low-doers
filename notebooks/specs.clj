(ns specs
  "Vega-Lite spec builders, shared by the Clerk notebook and the static JSON exporter (export.clj)."
  (:require [low-doers.types :as typ]))

(defn- payoff-heatmap*
  "Generic payoff heatmap."
  [title rows [lo hi :as domain]]
  (let [mid (/ (+ lo hi) 2)]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :title title
     :width 280 :height 120
     :data {:values (for [[label type-key] rows
                          :let [pt (typ/payoff-table type-key)]
                          [profile plabel] [[[:H :H] "HH"] [[:H :L] "HL"]
                                            [[:L :H] "LH"] [[:L :L] "LL"]]]
                      {:type label :profile plabel :payoff (pt profile)})}
     :layer [{:mark "rect"
              :encoding {:x {:field "profile" :type "nominal"
                             :sort ["HL" "LL" "HH" "LH"]
                             :title "Outcome"}
                         :y {:field "type" :type "nominal" :title nil}
                         :color {:field "payoff" :type "quantitative"
                                 :scale {:scheme "blues" :domain domain}
                                 :legend nil}}}
             {:mark {:type "text" :fontSize 16 :fontWeight "bold"}
              :encoding {:x {:field "profile" :type "nominal"
                             :sort ["HL" "LL" "HH" "LH"]}
                         :y {:field "type" :type "nominal"}
                         :text {:field "payoff" :type "quantitative"}
                         :color {:condition {:test (str "datum.payoff < " mid) :value "#333"}
                                 :value "white"}}}]}))

(defn payoff-heatmap
  "Canonical hs1/ls1 payoffs as a heatmap (4 = most preferred)."
  [_data]
  (payoff-heatmap* "Payoffs by Type (higher = preferred)"
                   [["hs1" :hs1] ["ls1" :ls1]] [1 4]))

(defn payoff-heatmap-t4
  "Table 4 adjusted payoff distances for hs1/ls1 (10 = most preferred)."
  [_data]
  (payoff-heatmap* "Table 4 - Adjusted Payoff Distances"
                   [["hs1" :hs1-t4] ["ls1" :ls1-t4]] [1 10]))

(defn h-collapse
  "Fig 1: H-action rate collapses to a ~9% steady state."
  [data]
  (let [steady (let [tail (filterv #(>= (:year %) 300) (:trajectory data))]
                 (/ (reduce + (map :h-fraction tail))
                    (count tail)))
        smooth [{:filter "datum.year >= 1"}
                {:sort [{:field "year"}]
                 :window [{:op "mean" :field "h-fraction" :as "smooth"}]
                 :frame [-2 2] :groupby ["seed"]}]
        x      {:field "year" :title "Year (log scale)" :scale {:type "log" :domain [1 1500]}}
        y      {:type "quantitative" :title "Rate of H Actions" :axis {:format ".0%"}}]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :title {:text "H-Action Rate Collapses to ~9%"
             :subtitle "Hired 50/50 from hs1 (high-minded) and ls1 (low-minded), both selfish"}
     :width 600 :height 240
     :data {:values (:trajectory data)}
     :layer [{:transform smooth
              :mark {:type "line" :strokeWidth 1 :opacity 0.3 :color "#4c78a8"}
              :encoding {:x x :y (assoc y :field "smooth") :detail {:field "seed"}}}
             {:transform (conj smooth {:aggregate [{:op "median" :field "smooth" :as "median"}]
                                       :groupby ["year"]})
              :mark {:type "line" :strokeWidth 2.5 :color "#4c78a8"}
              :encoding {:x x :y (assoc y :field "median")}}
             {:mark {:type "rule" :strokeDash [4 4] :color "#999"}
              :encoding {:y {:datum steady}}}]}))

(defn- fig2-panel [data postpone-val]
  {:title (str "postpone = " postpone-val " and quantile threshold = 15")
   :width 250 :height 250
   :data {:values (filterv #(= (:postpone %) postpone-val) (:figure-2 data))}
   :mark {:type "line" :point true}
   :encoding {:x {:field "n" :type "quantitative" :title "Number of academics"
                  :scale {:zero false}}
              :y {:field "h-rate" :type "quantitative" :title "Fraction of H exchanges"
                  :scale {:domain [0 1]} :axis {:format ".0%"}}
              :color {:field "threshold" :type "nominal" :title "Change-of-strategy threshold"}}})

(defn fig2
  "Fig 2: H-action rate by society size (15% quantile)."
  [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :title "Figure 2 - H-Action Rate vs. Society Size (15% quantile)"
   :hconcat [(fig2-panel data 0) (fig2-panel data 5)]})

(defn- fig3-panel [data postpone-val]
  {:title (str "postpone = " postpone-val " and quantile threshold = 5")
   :width 250 :height 250
   :data {:values (filterv #(= (:postpone %) postpone-val) (:figure-3 data))}
   :mark {:type "line" :point true}
   :encoding {:x {:field "n" :type "quantitative" :title "Number of academics"
                  :scale {:zero false}}
              :y {:field "h-rate" :type "quantitative" :title "Fraction of h exchanges"
                  :scale {:domain [0 1]} :axis {:format ".0%"}}
              :color {:field "threshold" :type "nominal" :title "Change-of-strategy threshold"}}})

(defn fig3
  "Fig 3: modified payoff distances (Table 4, 5% quantile)."
  [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :title "Figure 3 - Modified Payoff Distances (Table 4, 5% quantile)"
   :hconcat [(fig3-panel data 0) (fig3-panel data 5)]})

(defn hero
  "Section 4.8: hero & saint avert the collapse."
  [data]
  (let [smooth [{:filter "datum.year >= 1"}
                {:calculate "datum.type == 'hn1' || datum.type == 'hn2' ? 'hn' : datum.type"
                 :as "group"}
                {:sort [{:field "year"}]
                 :window [{:op "mean" :field "h-fraction" :as "smooth"}]
                 :frame [-2 2] :groupby ["group" "seed"]}]
        x     {:field "year" :type "quantitative" :title "Year (log scale)"
               :scale {:type "log" :domain [1 1500]}}
        y     {:field "smooth" :type "quantitative" :title "Rate of H Actions"
               :scale {:domain [0 0.6]} :axis {:format ".0%"}}
        color {:field "group" :type "nominal" :title "Type"
               :scale {:domain ["hs1" "hn"] :range ["#e45756" "#4c78a8"]}
               :legend {:labelExpr "datum.label == 'hs1' ? 'hs1 (selfish)' : 'hn1/hn2 (hero/saint, identical)'"}}]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :title {:text "Heroes & Saints Avert Collapse"
             :subtitle "Selfish hs1 collapses to ~9%; non-selfish heroes/saints (hn1/hn2) hold ~50%."}
     :width 600 :height 280
     :data {:values (:hero-trajectory data)}
     :layer [{:mark {:type "rule" :strokeDash [4 4] :color "#999"}
              :encoding {:y {:datum 0.5}}}
             {:transform (conj smooth {:aggregate [{:op "median" :field "smooth" :as "smooth"}]
                                       :groupby ["year" "group"]})
              :mark {:type "line" :strokeWidth 2.5}
              :encoding {:x x :y y :color color}}]}))

(defn- fig4-panel [data label]
  {:title label
   :width 250 :height 250
   :data {:values (filterv #(= (:hire-p %) label) (:figure-4 data))}
   :mark {:type "line" :point true}
   :encoding {:x {:field "n" :type "quantitative" :title "Number of academics"
                  :scale {:zero false}}
              :y {:field "h-rate" :type "quantitative" :title "Rate of HH exchanges"
                  :scale {:domain [0 1]} :axis {:format ".0%"}}
              :color {:field "threshold" :type "nominal" :title "Change-of-strategy threshold"}}})

(defn fig4
  "Fig 4: effect of hiring policy (postpone = 5)."
  [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :title "Figure 4 - Effect of Hiring Policy (postpone = 5)"
   :hconcat [(fig4-panel data "70% hs1") (fig4-panel data "90% hs1")]})

(defn rewards-sanctions
  "Sections 4.13-4.16 (Tables 5-8): H-rate under an institution that rewards HH
  and sanctions LL exchanges."
  [data]
  (let [rows  (mapcat #(get data %) [:table-5 :table-6 :table-7 :table-8])
        x     {:field "sanction" :type "ordinal" :title "Punishment for LL exchanges"
               :axis {:labelExpr "datum.value + '%'"}}
        y     {:field "h" :type "quantitative" :title "Rate of H exchanges"
               :scale {:domain [0 100]} :axis {:format ".0f"}}
        color {:field "reward" :type "nominal" :title "Reward for HH"
               :scale {:domain [100.0 50.0] :range ["#4c78a8" "#f58518"]}
               :legend {:labelExpr "datum.label + '%'"}}]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :title {:text "Sanctions Drive H, and Compound with Hiring"
             :subtitle "Frequent LL-sanctions lift the H-rate; hiring more hs1 (right) lifts it further"}
     :data {:values rows}
     :transform [{:calculate "datum.h - datum.ci" :as "lo"}
                 {:calculate "datum.h + datum.ci" :as "hi"}
                 {:calculate "datum.freq == 1 ? 'sanction every year (f=1)' : 'sanction every 3rd year (f=3)'" :as "freq-label"}
                 {:calculate "datum.hire + '% hs1 hiring'" :as "hire-label"}]
     :facet {:column {:field "hire-label" :type "nominal" :title nil
                      :sort {:field "hire" :order "ascending"}}
             :row {:field "freq-label" :type "nominal" :title nil
                   :sort {:field "freq" :order "descending"}}}
     :spec {:width 220 :height 200
            :layer [{:mark {:type "errorbar" :ticks true}
                     :encoding {:x x :y (assoc y :field "lo")
                                :y2 {:field "hi"} :color color}}
                    {:mark {:type "line" :point true}
                     :encoding {:x x :y y :color color}}]}}))

(defn- bin-of [step v] (* step (long (Math/floor (/ (double v) step)))))

(defn- binned-counts
  "Group `rows` into [bin group stack] -> count, binning `bin-field` into `step`-wide buckets."
  [rows bin-field step group-field stack-field]
  (->> rows
       (map (fn [r] (assoc r :bin (bin-of step (get r bin-field)))))
       (group-by (juxt :bin group-field stack-field))
       (map (fn [[[bin group stack] rs]] {:bin bin :group group :stack stack :count (count rs)}))))

(defn- mirror-pyramid
  "A 3-column population pyramid."
  [{:keys [title subtitle binned left-group right-group left-color right-color x-title]}]
  (let [max-count (->> binned (map :count) (reduce max 0))
        bins      (->> binned (map :bin) distinct sort)
        y         {:field "bin" :type "ordinal" :sort "ascending" :axis nil}
        panel     (fn [group color reverse?]
                    {:width 220 :height 280
                     :data {:values (filterv #(= group (:group %)) binned)}
                     :mark "bar"
                     :encoding {:y y
                                :x {:field "count" :type "quantitative" :title x-title
                                    :scale {:domain [0 max-count] :reverse reverse?}
                                    :axis {:tickCount 4}}
                                :color color}})]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :title {:text title :subtitle subtitle}
     :resolve {:scale {:y "shared"}}
     :hconcat [(panel left-group left-color true)
               {:width 40 :height 280
                :view {:stroke nil}
                :data {:values (mapv (fn [b] {:bin b}) bins)}
                :mark {:type "text" :fontSize 11}
                :encoding {:y y :text {:field "bin" :type "ordinal"}}}
               (panel right-group right-color false)]}))

(defn tenure-pyramid
  "Tenure-at-retirement distribution for hs1 vs ls1, as a population pyramid."
  [data]
  (let [binned (binned-counts (:retirements data) :tenure 5 :type :type)]
    (mirror-pyramid
     {:title "Tenure at Retirement: hs1 vs ls1"
      :subtitle "ls1 (low-minded, right) lasts through longer tenures than hs1 (high-minded, left)"
      :binned binned :left-group "hs1" :right-group "ls1"
      :left-color {:value "#4c78a8"} :right-color {:value "#e45756"}
      :x-title "Retirements"})))

(defn retirement-reason-pyramid
  "Retirement reason by tenure, as a population pyramid."
  [data]
  (let [binned (binned-counts (:retirements data) :tenure 5 :reason :type)
        color  {:field "stack" :type "nominal" :title "Type"
                :scale {:domain ["hs1" "ls1"] :range ["#4c78a8" "#e45756"]}}]
    (mirror-pyramid
     {:title "Retirement Reason by Tenure: Early Capitulation vs Mandatory"
      :subtitle "Left: retired early under sustained low payoffs. Right: reached the mandatory retirement age."
      :binned binned :left-group "early" :right-group "mandatory"
      :left-color color :right-color color
      :x-title "Retirements"})))

(def all
  "Map of figure id -> {:build spec-fn, :render :svg|:png}, for the exporter."
  {"payoff-table"          {:build payoff-heatmap :render :svg}
   "payoff-table-t4"       {:build payoff-heatmap-t4 :render :svg}
   "fig1-h-collapse"       {:build h-collapse :render :png}
   "fig2-society-size"     {:build fig2 :render :svg}
   "fig3-payoff-distances" {:build fig3 :render :svg}
   "fig4-hiring-policy"    {:build fig4 :render :svg}
   "hero-saint"            {:build hero :render :png}
   "rewards-sanctions"     {:build rewards-sanctions :render :svg}
   "tenure-pyramid"        {:build tenure-pyramid :render :svg}
   "retirement-reason"     {:build retirement-reason-pyramid :render :svg}})
