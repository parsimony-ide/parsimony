(ns parsimony.server.sente-handler
  "Sente event handler"
  (:require [parsimony.log :as log]
            [parsimony.server.algo :as algo]
            [parsimony.server.compiler :as compiler]))

(defn sente-handler-fn [{algo :algo
                         compiler :compiler}]
  (fn -sente-handler [{:keys [event id ?data ring-req ?reply-fn send-fn] :as ev-msg}]
    #_(log/debug id ?data)
    (case id

      ;; algo
      ;;
      :algo/horizon
      (when ?reply-fn
        (?reply-fn (log/spy :debug (algo/horizon algo ?data))))
      :algo/all-packages
      (when ?reply-fn
        (let [packages (algo/all-packages algo)]
          (log/debug "algo/all-packages:" (keys packages))
          (?reply-fn packages)))
      :algo/example-strings
      (when ?reply-fn
        (?reply-fn (log/spy :debug (algo/example-strings algo ?data))))

      ;; compiler
      ;;
      :compiler/compile-lexer
      (when ?reply-fn
        (?reply-fn (compiler/compile-lexer compiler ?data)))

      ;; sente built-in
      ;;
      :chsk/ws-ping nil ;; Sente-builtin
      :chsk/uidport-open nil ;; Sente-builtin
      :chsk/uidport-close nil ;; Sente-builtin

      ;; default case
      ;;
      (log/error "Unhandled event" id ?data))))
