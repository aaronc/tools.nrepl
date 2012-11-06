(ns clojure.tools.nrepl.platform)

(def long-max-value Long/MAX_VALUE)

(defn ticks [] (System/nanoTime))

(defn illegal-state-exception
  ([msg]
     (IllegalStateException. msg))
  ([msg e]
     (IllegalStateException. msg e)))

(defn socket [host port]
  (java.net.Socket. ^String host (int port)))

(deftype FnTransport [recv-fn send-fn close]
  Transport
  (send [this msg] (-> msg clojure.walk/stringify-keys send-fn) this)
  (recv [this] (.recv this Long/MAX_VALUE))
  (recv [this timeout] (clojure.walk/keywordize-keys (recv-fn timeout)))
  java.io.Closeable
  (close [this] (close)))

(deftype QueueTransport [^BlockingQueue in ^BlockingQueue out]
  Transport
  (send [this msg] (.put out msg) this)
  (recv [this] (.take in))
  (recv [this timeout] (.poll in timeout TimeUnit/MILLISECONDS)))

(defn queue-put [queue item]
  (.put queue item))

(defn queue-poll [queue timeout]
  (.poll queue timeout TimeUnit/MILLISECONDS))


