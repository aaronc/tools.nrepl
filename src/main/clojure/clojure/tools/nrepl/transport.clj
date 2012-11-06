
(ns ^{:author "Chas Emerick"}
     clojure.tools.nrepl.transport
  (:require [clojure.tools.nrepl.bencode :as be]
            [clojure.java.io :as io]
            (clojure walk set))
  (:use [clojure.tools.nrepl.misc :only (returning uuid)])
  (:refer-clojure :exclude (send))
  (:import (java.io InputStream OutputStream PushbackInputStream
                    PushbackReader)
           java.net.Socket
           (java.util.concurrent SynchronousQueue LinkedBlockingQueue
                              BlockingQueue TimeUnit)
        clojure.lang.RT))

(defn fn-transport
"Returns a Transport implementation that delegates its functionality
to the 2 or 3 functions provided."
([read write] (fn-transport read write nil))
([read write close]
 (let [read-queue (synchronous-queue)]
   (future (while true
             (platform/queue-put read-queue (read))))
   (FnTransport.
     #(platform/queue-poll read-queue % TimeUnit/MILLISECONDS)
     write
     close))))

(defmulti #^{:private true} <bytes class)

(defmethod <bytes :default
[input]
input)

(defmethod <bytes bytes-class
;;[^{:tag bytes-class} input]
[input]
(String. input "UTF-8") )

(defmethod <bytes clojure.lang.IPersistentVector
[input]
(vec (map <bytes input)))

(defmethod <bytes clojure.lang.IPersistentMap
[input]
(->> input
 (map (fn [[k v]] [k (<bytes v)]))
 (into {})))

(defn bencode
"Returns a Transport implementation that serializes messages
over the given Socket or InputStream/OutputStream using bencode."
([^Socket s] (bencode s s s))
([in out & [^Socket s]]
 (let [in (PushbackInputStream. (io/input-stream in))
       out (io/output-stream out)]
   (fn-transport
     #(let [payload   (be/read-bencode in)
            unencoded (<bytes (payload "-unencoded"))
            to-decode (apply dissoc payload "-unencoded" unencoded)]
        (merge (dissoc payload "-unencoded")
               (when unencoded {"-unencoded" unencoded})
               (<bytes to-decode)))
     #(locking out
        (doto out
          (be/write-bencode %)
          .flush))
     (fn []
       (.close in)
       (.close out)
       (when s (.close s)))))))

(defn tty
"Returns a Transport implementation suitable for serving an nREPL backend
via simple in/out readers, as with a tty or telnet connection."
([^Socket s] (tty s s s))
([in out & [^Socket s]]
 (let [r (PushbackReader. (io/reader in))
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
                             \newline "user=> ")}))

(deftype QueueTransport [^BlockingQueue in ^BlockingQueue out]
  clojure.tools.nrepl.transport.Transport
  (send [this msg] (.put out msg) this)
  (recv [this] (.take in))
  (recv [this timeout] (.poll in timeout TimeUnit/MILLISECONDS)))

(defn piped-transports
  "Returns a pair of Transports that read from and write to each other."
  []
  (let [a (LinkedBlockingQueue.)
        b (LinkedBlockingQueue.)]
    [(QueueTransport. a b) (QueueTransport. b a)]))
