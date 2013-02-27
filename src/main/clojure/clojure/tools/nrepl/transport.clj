
(ns ^{:author "Chas Emerick"}
     clojure.tools.nrepl.transport
 (:require [clojure.tools.nrepl.bencode :as be]
            [clojure.clr.io :as io]
            (clojure walk set))
  (:use [clojure.tools.nrepl.misc :only (returning uuid)])
  (:refer-clojure :exclude (send))
  (:import
   [System.Text Encoding]
   [System.Net.Sockets Socket SocketException
    TcpClient]))
 
(defprotocol Transport
  "Defines the interface for a wire protocol implementation for use
   with nREPL."
  (recv [this] [this timeout]
    "Reads and returns the next message received.  Will block.
     Should return nil the a message is not available after `timeout`
     ms or if the underlying channel has been closed.")
  (send [this msg] "Sends msg. Implementations should return the transport."))

(deftype FnTransport [recv-fn send-fn close]
  Transport
  (send [this msg] (-> msg clojure.walk/stringify-keys send-fn) this)
  (recv [this] (.recv this Int64/MaxValue))
  (recv [this timeout] (clojure.walk/keywordize-keys (recv-fn timeout)))
  IDisposable
  (Dispose [this] (close)))

(defn synchronous-queue []
  (|System.Collections.Concurrent.BlockingCollection`1[System.Object]|.))

(def ^:private try-take-method (.GetMethod (type (synchronous-queue)) "TryTake" (into-array Type [(.MakeByRefType Object) Int32])))

(defn- queue-poll [queue timeout]
  (let [args (into-array Object [nil (int timeout)])]
    (when (.Invoke try-take-method queue args)
      (aget args 0))))

  ;; TODO this keywordization/stringification has no business being in FnTransport
(defn fn-transport
    "Returns a Transport implementation that delegates its functionality
   to the 2 or 3 functions provided."
    ([read write] (fn-transport read write nil))
    ([read write close]
       (let [read-queue (synchronous-queue)]
         (future (try
                   (while true
                     (.Enqueue read-queue (read)))
                   (catch Exception t
                     (.Enqueue read-queue t))))
         (FnTransport.
          (let [failure (atom nil)]
            (fn [timeout] (if @failure
                           (throw @failure)
                           (let [msg (queue-poll read-queue timeout)]
                             (if (instance? Exception msg)
                               (do (reset! failure msg) (throw msg))
                               msg)))))
          write
          close))))

(defmulti #^{:private true} <bytes class)

(defmethod <bytes :default
  [input]
  input)

(defmethod <bytes |System.Byte[]|
  [#^|System.Byte[]| input]
  (.GetString Encoding/UTF8 input))

(defmethod <bytes clojure.lang.IPersistentVector
  [input]
  (vec (map <bytes input)))

(defmethod <bytes clojure.lang.IPersistentMap
  [input]
  (->> input
       (map (fn [[k v]] [k (<bytes v)]))
       (into {})))

(defmacro ^{:private true} rethrow-on-disconnection
  [^TcpClient s & body]
  `(try
     ~@body
     (catch System.IO.EndOfStreamException e#
       (throw (Exception. "The transport's socket appears to have lost its connection to the nREPL server" e#)))
     (catch Exception e#
       (if (and ~s (not (.Connected ~s)))
         (throw (Exception. "The transport's socket appears to have lost its connection to the nREPL server" e#))
         (throw e#)))))

(defn bencode
  "Returns a Transport implementation that serializes messages
over the given Socket or InputStream/OutputStream using bencode."
  ([^TcpClient s]
     (let [stream (.GetStream s)]
       (bencode stream stream s)))
  ([in out & [^TcpClient s]]
     (let [in (io/input-stream in)
           out (io/output-stream out)]
       (fn-transport
        (fn read-fn []
          (let [payload (rethrow-on-disconnection s (be/read-bencode in))
                unencoded (<bytes (payload "-unencoded"))
                to-decode (apply dissoc payload "-unencoded" unencoded)]
            (merge (dissoc payload "-unencoded")
                   (when unencoded {"-unencoded" unencoded})
                   (<bytes to-decode))))
        (fn write-fn [x]
          (println "writing" x)
          (rethrow-on-disconnection s
                                    (locking out
                                      (doto out
                                        (be/write-bencode x)
                                        .Flush))))
        (fn []
          (.Dispose in)
          (.Dispose out)
          (when s (.Close s)))))))

(comment
  (defn tty
    "Returns a Transport implementation suitable for serving an nREPL backend
via simple in/out readers, as with a tty or telnet connection."
    ([^Socket s] (tty s s s))
    ([in out & [^Socket s]]
       (let [r (io/reader in)
             w (io/writer out)
             cns (atom "user")
             prompt (fn [newline?]
                      (when newline? (.write w (int \newline)))
                      (.write w (str @cns "=> ")))
             session-id (atom nil)
             read-msg #(let [code (read r)]
                         (merge {:op "eval" :code [code] :ns @cns :id (str "eval" (uuid))}
                                (when @session-id {:session @session-id})))
             read-seq (atom (cons {:op "clone"} (repeatedly read-msg)))
             write (fn [{:strs [out err value status ns new-session id] :as msg}]
                     (when new-session (reset! session-id new-session))
                     (when ns (reset! cns ns))
                     (doseq [^String x [out err value] :when x]
                       (.write w x))
                     (when (and (= status #{:done}) id (.startsWith ^String id "eval"))
                       (prompt true))
                     (.flush w))
             read #(let [head (promise)]
                     (swap! read-seq (fn [s]
                                       (deliver head (first s))
                                       (rest s)))
                     @head)]
         (fn-transport read write
                       (when s
                         (swap! read-seq (partial cons {:session @session-id :op "close"}))
                         #(.close s))))))

  (defn tty-greeting
    "A greeting fn usable with clojure.tools.nrepl.server/start-server,
meant to be used in conjunction with Transports returned by the
`tty` function.

Usually, Clojure-aware client-side tooling would provide this upon connecting
   to the server, but telnet et al. isn't that."
    [transport]
    (send transport {:out (str ";; Clojure " (clojure-version)
                               \newline "user=> ")})))

(deftype QueueTransport [^|System.Collections.Concurrent.BlockingCollection`1[System.Object]| in ^|System.Collections.Concurrent.BlockingCollection`1[System.Object]| out]
  Transport
  (send [this msg] (.Add out msg) this)
  (recv [this] (.Take in))
  (recv [this timeout] (queue-poll this timeout)))

(defn piped-transports
  "Returns a pair of Transports that read from and write to each other."
  []
  (let [a (synchronous-queue)
        b (synchronous-queue)]
    [(QueueTransport. a b) (QueueTransport. b a)]))
