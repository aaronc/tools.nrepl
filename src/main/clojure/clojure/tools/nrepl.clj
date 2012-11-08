;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "High level nREPL client support."
      :author "Chas Emerick"}
  clojure.tools.nrepl
  (:require [clojure.tools.nrepl.transport :as transport]
            clojure.set
            [clojure.clr.io :as io])
  (:use [clojure.tools.nrepl.misc :only (uuid)])
  (:import
   [System.Net.Sockets TcpClient]))

(defn response-seq
  "Returns a lazy seq of messages received via the given Transport.
   Called with no further arguments, will block waiting for each message.
   The seq will end only when the underlying Transport is closed (i.e.
   returns nil from `recv`) or if a message takes longer than `timeout`
   millis to arrive."
  ([transport] (response-seq transport Int64/MaxValue))
  ([transport timeout]
    (take-while identity (repeatedly #(transport/recv transport timeout)))))

(defn client
  "Returns a fn of zero and one argument, both of which return the current head of a single
   response-seq being read off of the given client-side transport.  The one-arg arity will
   send a given message on the transport before returning the seq.

   Most REPL interactions are best performed via `message` and `client-session` on top of
   a client fn returned from this fn."
  [transport response-timeout]
  (let [latest-head (atom nil)
        update #(swap! latest-head
                       (fn [[timestamp seq :as head] now]
                         (if (< timestamp now)
                           [now %]
                           head))
                       ; nanoTime appropriate here; looking to maintain ordering, not actual timestamps
                       (.Ticks DateTime/Now))
        tracking-seq (fn tracking-seq [responses]
                       (lazy-seq
                         (if (seq responses)
                           (let [rst (tracking-seq (rest responses))]
                             (update rst)
                             (cons (first responses) rst))
                           (do (update nil) nil))))
        restart #(let [head (-> transport
                              (response-seq response-timeout)
                              tracking-seq)]
                   (reset! latest-head [0 head])
                   head)]
    (fn this
      ([] (or (second @latest-head)
              (restart)))
      ([msg]
        (transport/send transport msg)
        (this)))))

(defn- take-until
  "Like (take-while (complement f) coll), but includes the first item in coll that
   returns true for f."
  [f coll]
  (let [[head tail] (split-with (complement f) coll)]
    (concat head (take 1 tail))))

(defn- delimited-transport-seq
  [client termination-statuses delimited-slots]
  (comp (partial take-until (comp #(seq (clojure.set/intersection % termination-statuses))
                                  set
                                  :status)) 
        (let [keys (keys delimited-slots)]
          (partial filter #(= delimited-slots (select-keys % keys))))
        client
        #(merge % delimited-slots)))

(defn message
  "Returns a function of one argument.  Accepts a message that is sent via the
   client provided with a fixed message :id added to it.  Returns the
   head of the client's response seq, filtered to include only
   messages related to the message :id that will terminate upon receipt of a
   \"done\" :status."
  [client {:keys [id] :as msg :or {id (uuid)}}]
  (let [f (delimited-transport-seq client #{"done"} {:id id})]
    (f (assoc msg :id id))))

(defn new-session
  "Provokes the creation and retention of a new session, optionally as a clone
   of an existing retained session, the id of which must be provided as a :clone
   kwarg.  Returns the new session's id."
  [client & {:keys [clone]}]
  (let [resp (first (message client (merge {:op "clone"} (when clone {:session clone}))))]
    (or (:new-session resp)
        (throw (InvalidOperationException.
                 (str "Could not open new session; :clone response: " resp))))))

(defn client-session
  "Returns a function of one argument.  Accepts a message that is sent via the
   client provided with a fixed :session id added to it.  Returns the
   head of the client's response seq, filtered to include only
   messages related to the :session id that will terminate when the session is
   closed."
  [client & {:keys [session clone]}]
  (let [session (or session (apply new-session client (when clone [:clone clone])))]
    (delimited-transport-seq client #{"session-closed"} {:session session})))

(defn combine-responses
  "Combines the provided seq of response messages into a single response map.

   Certain message slots are combined in special ways:

     - only the last :ns is retained
     - :value is accumulated into an ordered collection
     - :status is accumulated into a set
     - string values (associated with e.g. :out and :err) are concatenated"
  [responses]
  (reduce
    (fn [m [k v]]
      (case k
        (:id :ns) (assoc m k v)
        :value (update-in m [k] (fnil conj []) v)
        :status (update-in m [k] (fnil into #{}) v)
        (if (string? v)
          (update-in m [k] #(str % v))
          (assoc m k v))))            
    {} (apply concat responses)))

(defmacro code
  "Expands into a string consisting of the macro's body's forms
   (literally, no interpolation/quasiquoting of locals or other
   references), suitable for use in an :eval message, e.g.:

   {:op :eval, :code (code (+ 1 1) (slurp \"foo.txt\"))}"
  [& body]
  (apply str (map pr-str body)))

(defn read-response-value
  "Returns the provided response message, replacing its :value string with
   the result of (read)ing it.  Returns the message unchanged if the :value
   slot is empty or not a string."
  [{:keys [value] :as msg}]
  (if-not (string? value)
    msg
    (try
      (assoc msg :value (read-string value))
      (catch Exception e
        (throw (InvalidOperationException.
                (str "Could not read response value: " value) e))))))

(defn response-values
  "Given a seq of responses (as from response-seq or returned from any function returned
   by client or client-session), returns a seq of values read from :value slots found
   therein."
  [responses]
  (->> responses
    (map read-response-value)
    combine-responses
    :value))

(defn connect
  "Connects to a socket-based REPL at the given host (defaults to localhost) and port,
   returning the Transport (by default clojure.tools.nrepl.transport/bencode)
   for that connection.

   Transports are most easily used with `client`, `client-session`, and
   `message`, depending on the semantics desired."
  [& {:keys [port host transport-fn] :or {transport-fn transport/bencode
                                          host "localhost"}}]
  {:pre [transport-fn port]}
  (transport-fn (TcpClient. ^String host (int port))))

(defn- ^Uri to-uri
  [x]
  {:post [(instance? Uri %)]}
  (if (string? x)
    (Uri. x)
    x))

(defn- socket-info
  [x]
  (let [uri (to-uri x)
        port (.Port uri)]
    (merge {:host (.Host uri)}
           (when (pos? port)
             {:port port}))))

(def ^{:private false} uri-scheme #(-> (to-uri %) .Scheme .ToLower))

(defmulti url-connect
  "Connects to an nREPL endpoint identified by the given URL/URI.  Valid
   examples include:

      nrepl://192.168.0.12:7889
      telnet://localhost:5000
      http://your-app-name.heroku.com/repl

   This is a multimethod that dispatches on the scheme of the URI provided
   (which can be a string or java.net.URI).  By default, implementations for
   nrepl (corresponding to using the default bencode transport) and
   telnet (using the clojure.tools.nrepl.transport/tty transport) are
   registered.  Alternative implementations may add support for other schemes,
   such as HTTP, HTTPS, JMX, existing message queues, etc."
  uri-scheme)

;; TODO oh so ugly
(defn- add-socket-connect-method!
  [protocol connect-defaults]
  (defmethod url-connect protocol
    [uri]
    (apply connect (mapcat identity
                           (merge connect-defaults
                                  (socket-info uri))))))

(add-socket-connect-method! "nrepl" {:transport-fn transport/bencode
                                     :port 7888})
(comment (add-socket-connect-method! "telnet" {:transport-fn transport/tty}))

(defmethod url-connect :default
  [uri]
  (throw (ArgumentException.
           (format "No nREPL support known for scheme %s, url %s" (uri-scheme uri) uri))))

(def ^{:doc "Current version of nREPL, map of :major, :minor, :incremental, and :qualifier."}
  version
  {:major 0 :minor 0 :incremental 2 :qualifier "SNAPSHOT"})
