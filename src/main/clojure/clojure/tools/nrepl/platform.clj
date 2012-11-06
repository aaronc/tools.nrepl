(ns clojure.tools.nrepl.platform
  (:import
   [System.Net.Sockets Socket]))

(def long-max-value Int64/MaxValue)

(defn ticks [] (.Ticks DateTime/Now))

(defn illegal-state-exception
  ([msg]
     (InvalidOperationException. msg))
  ([msg e]
     (InvalidOperationException. msg e)))

(defn socket [host port]
  ())

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

(deftype QueueTransport [^|System.Collections.Concurrent.BlockingCollection`1[System.Object]| in ^|System.Collections.Concurrent.BlockingCollection`1[System.Object]| out]
  Transport
  (send [this msg] (.Add out msg) this)
  (recv [this] (.Take in))
  (recv [this timeout]
    (comment
      (let [res (str)]
        (when (.TryTake in (by-ref res) timeout))
        res))
    (.Take in)))

(defn synchronous-queue []
  (|System.Collections.Concurrent.ConcurrentQueue`1)[System.Object]|.)

(defn queue-put [queue item]
  (.Enqueue queue item))

(defn queue-poll [queue timeout]
  (.Take queue))

(def bytes-class |System.Byte[]|)

(comment
  (java.io Reader StringReader Writer PrintWriter)
  (java.util.concurrent Future TimeUnit TimeoutException)
  (:import
   java.io.ByteArrayOutputStream
   java.io.InputStream
   java.io.OutputStream
   java.io.PushbackInputStream
   clojure.lang.RT)
  (java.io File StringReader)
  (:import (java.net Socket ServerSocket InetSocketAddress))

  (:import (java.io InputStream OutputStream PushbackInputStream
                    PushbackReader)
           java.net.Socket
           (java.util.concurrent SynchronousQueue LinkedBlockingQueue
                                 BlockingQueue TimeUnit)
           clojure.lang.RT)

  (:import clojure.lang.LineNumberingPushbackReader
           (java.io StringReader Writer)
           java.util.concurrent.atomic.AtomicLong
           (java.util.concurrent LinkedBlockingQueue
                                 TimeUnit ThreadPoolExecutor
                                 ThreadFactory))

  (:import clojure.tools.nrepl.transport.Transport
           (java.io PipedReader PipedWriter Reader Writer PrintWriter StringReader)
           clojure.lang.LineNumberingPushbackReader)

  (:import
   [java.io ; System.IO
    Reader ; TextReader
    StringReader ; StringReader
    Writer ; TextWriter
    PrintWriter ; TextWriter
    ByteArrayOutputStream ; MemoryStream
    InputStream ; Stream
    OutputStream ; Stream
    PushbackInputStream ; see clojure-clr
    File ; File
    PushbackReader ; see clojure-clr
    PipedReader PipedWriter ; see System.IO.Pipes
    ]
   [java.net ; System.Net.Sockets
    Socket ; Socket
    ServerSocket ; TcpListener
    InetSocketAddress ; n/a
    ]
   [java.util.concurrent ; System.Collections.Concurrent
    SynchronousQueue ; SynchronousQueue
    LinkedBlockingQueue ; BlockingCollection
    BlockingQueue ; BlockingCollection
    TimeUnit
    ThreadPoolExecutor
    ThreadFactory
    ]
   [java.util.concurrent.atomic
    AtomicLong ; Interlocked
    ])
)