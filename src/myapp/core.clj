(ns myapp.core
  (:use [myapp.genetic :as genetic])
  (:require [compojure.core :refer :all]
            [compojure.route :as cpj.route]
            [hiccup.core :as hiccup]
            [org.httpkit.server :refer [run-server]])) ; httpkit is a server

(defn d3-page []
  (hiccup/html
    [:head
      [:title "Equation fitter"]
      [:link {:rel "stylesheet" :type "text/css" :href "/main.css"}]
    ]
    [:body
      [:script (genetic/print-population)]
      [:div {:class "graphcontainer"}
       [:div {:class "graph" :id "goalGraph"}]
       [:h3  "Goal graph"]
      ]

     [:div  {:class "graphcontainer"}
      [:div {:class "graph" :id "bestGraph"}]
      [:h3 {:id "matchgraph"}  "Matched graph"]
      ]
      [:script {:src "http://d3js.org/d3.v3.min.js"}]
      [:script {:src "/linegraph.js" }]]))

(defroutes myapp
  ;render our graphs
  (GET "/" [] (fn [req] (d3-page)))
  ;static routes
  (cpj.route/files "/" {:root "public"})
  (cpj.route/not-found "Page not found"))

(defn -main [& args]
  (run-server myapp {:port 5000}))