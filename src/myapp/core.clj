(ns myapp.core
  (:require [compojure.core :refer :all]
            [compojure.route :as cpj.route]
            [hiccup.core :as hiccup]
            [org.httpkit.server :refer [run-server]])) ; httpkit is a server

(defn d3-page []
  (hiccup/html
    [:head
      [:title "Equation fitter"]
      [:link {:href "/main.css"}]
    ]
    [:body
      [:div {:class "graph" :id "goalGraph"}]
      [:div {:class "graph" :id "bestGraph"}]
      [:div {:class "graph" :id "worstGraph"}]
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