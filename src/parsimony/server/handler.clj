(ns parsimony.server.handler
  (:require [compojure.core :refer [routes GET POST]]
            [compojure.route :as route]
            [clojure.edn :as edn]
            [liberator.core :refer [resource]]
            [parsimony.server.algo :as algo]
            [ring.util.response :as response]))

(defn coerce-to-vector
  "Ring parameter parsing interprets a singleton vector as just a string. This coerces such singletons back to a vector"
  [ss]
  (if (string? ss)
    [ss]
    ss))

(defn sente-routes [{{ring-ajax-get-or-ws-handshake :ring-ajax-get-or-ws-handshake
                      ring-ajax-post :ring-ajax-post} :sente}]
  (routes
   (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
   (POST "/chsk" req (ring-ajax-post req))))

(defn app-routes [{:keys [algo] :as endpoint}]
  (routes
    (GET "/horizon" []
         (resource :available-media-types ["application/edn"]
                   :handle-ok
                   (fn [ctx]
                     (->> (get-in ctx [:request :params :strings])
                          (coerce-to-vector)
                          (algo/horizon algo)
                          (vec)))))))

(defn resource-routes [endpoint]
  (routes
    (route/resources "/")
    ;; serve index.html from /
    (GET "/" [] (-> (response/resource-response "index.html" {:root "public"})
                    ;; set content type header to prevent browser from downloading instead of rendering HTML
                    (response/content-type "text/html")))))

(defn all-routes [endpoint]
  (routes
    (resource-routes endpoint)
    (app-routes endpoint)
    (sente-routes endpoint)
    (route/not-found "Page not found")))
