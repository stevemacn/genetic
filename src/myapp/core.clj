(ns myapp.core
  (:require [compojure.core :refer :all]

            [org.httpkit.server :refer [run-server]])) ; httpkit is a server



(defroutes myapp
  (GET "/" [] (fn [req] "hello"))
  (GET "/hello" [] "Hello World"))


(defn -main [& args]
  (run-server myapp {:port 5000}))