(ns parsimony.server
  (:require [com.stuartsierra.component :as component]
            [environ.core :refer [env]]
            [fipp.edn :refer [pprint]]
            [parsimony.log :as log]
            [parsimony.server.handler :refer [all-routes]]
            [parsimony.server.algo :refer [new-algo]]
            [parsimony.server.compiler :refer [new-compiler]]
            [parsimony.server.sente-handler :refer [sente-handler-fn]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [schema.core :as s]
            [system.components.endpoint :refer [new-endpoint]]
            [system.components.middleware :refer [new-middleware]]
            [system.components.handler :refer [new-handler]]
            [system.components.immutant-web :refer [new-web-server]]
            [system.components.sente :refer [new-channel-socket-server]]
            [taoensso.sente.server-adapters.immutant :refer [sente-web-server-adapter]]))

(defn int-or-nil
  "Convert a String in base 10 format to an Integer if possible, otherwise return nil"
  [x]
  (when (and x (string? x))
    (try
      (Integer/parseInt x)
      (catch NumberFormatException _
        nil))))

(defn int-env [x]
  (int-or-nil (env x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/defschema config-schema
  {:port s/Int
   :db-path s/Str})

(def default-config
  {:port (or (int-env :parsimony-port) 9254)
   :db-path (or (env :parsimony-db-path) "rdag.db")})

(defn system
  ([]
   (system {}))
  ([config]
   (let [config (merge default-config config)
         startup-msg (str "Starting system with options: " config)]
     (log/info startup-msg)
     (s/validate config-schema config)
     (component/system-map
      :routes (component/using
               (new-endpoint all-routes)
               [:algo :sente])
      :middleware (new-middleware {:middleware [[wrap-defaults api-defaults]]})
      :handler (component/using
                (new-handler)
                [:routes :middleware])
      :sente (component/using
              (new-channel-socket-server sente-handler-fn sente-web-server-adapter {:wrap-component? true})
              [:algo :compiler])
      :http (component/using
             (new-web-server (:port config))
             [:handler])
      :algo (new-algo config)
      :compiler (new-compiler config)))))
