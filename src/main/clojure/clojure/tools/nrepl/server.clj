(ns ^{:doc "Default server implementations"
      :author "Chas Emerick"}
     clojure.tools.nrepl.server
  (:require [clojure.tools.nrepl :as repl]
            (clojure.tools.nrepl [ack :as ack]
                                 [transport :as t]
                                 [middleware :as middleware])
            (clojure.tools.nrepl.middleware interruptible-eval
                                            pr-values
                   ;;                         session
                                            load-file))
  (:use [clojure.tools.nrepl.misc :only (returning response-for log)])
  ;;(:import (java.net Socket ServerSocket InetSocketAddress))
  (:import
   [System.Net Dns]
   [System.Net.Sockets TcpListener]))

(defn handle*
  [msg handler transport]
  (try
    (handler (assoc msg :transport transport))
    (catch Exception t
      (log t "Unhandled REPL handler exception processing message" msg))))

(defn handle
  "Handles requests received via `transport` using `handler`.
   Returns nil when `recv` returns nil for the given transport."
  [handler transport]
  (when-let [msg (t/recv transport)]
    (future (handle* msg handler transport))
    (recur handler transport)))

(defn- accept-connection
  [{:keys [^TcpListener server-socket open-transports transport greeting handler]
    :as server}]
  (when-not false ;;(.isClosed server-socket)
    (let [sock (.AcceptTcpClient server-socket)]
      (future (let [transport (transport sock)]
                (try
                  (swap! open-transports conj transport)
                  (when greeting (greeting transport))
                  (handle handler transport)
                  (finally
                    (swap! open-transports disj transport)
                    (.Dispose transport)))))
      (future (accept-connection server)))))

(defn- safe-close
  [x]
  (try
    (.Close x)
    (catch Exception e
      (log e "Failed to close " x))))

(defn stop-server
  "Stops a server started via `start-server`."
  [{:keys [open-transports ^ServerSocket server-socket] :as server}]
  (returning server
    (.Close server-socket)
    (swap! open-transports #(reduce
                              (fn [s t]
                                ; should always be true for the socket server...
                                (if (instance? java.io.Closeable t)
                                  (do
                                    (safe-close t)
                                    (disj s t))
                                  s))
                              % %))))

(defn unknown-op
  "Sends an :unknown-op :error for the given message."
  [{:keys [op transport] :as msg}]
  (t/send transport (response-for msg :status #{:error :unknown-op :done} :op op)))

(def default-middlewares
  [#'clojure.tools.nrepl.middleware/wrap-describe
   #'clojure.tools.nrepl.middleware.interruptible-eval/interruptible-eval
   #'clojure.tools.nrepl.middleware.load-file/wrap-load-file
   #'clojure.tools.nrepl.middleware.session/add-stdin
   #'clojure.tools.nrepl.middleware.session/session])

(defn default-handler
  "A default handler supporting interruptible evaluation, stdin, sessions, and
   readable representations of evaluated expressions via `pr`.

   Additional middlewares to mix into the default stack may be provided; these
   should all be values (usually vars) that have an nREPL middleware descriptor
   in their metadata (see clojure.tools.nrepl.middleware/set-descriptor!)."
  [& additional-middlewares]
  (let [stack (middleware/linearize-middleware-stack (concat default-middlewares
                                                             additional-middlewares))]
    ((apply comp (reverse stack)) unknown-op)))

;; TODO
#_(defn- output-subscriptions
  [h]
  (fn [{:keys [op sub unsub] :as msg}]
    (case op
      "sub" ;; TODO
      "unsub"
      (h msg))))

(defrecord Server [server-socket port open-transports transport greeting handler]
  IDisposable
  (Dispose [this] (stop-server this))
  ;; TODO here for backward compat with 0.2.x; drop eventually
  clojure.lang.IDeref
  (deref [this] this))

(defn start-server
  "Starts a socket-based nREPL server.  Configuration options include:
 
   * :port — defaults to 0, which autoselects an open port on localhost
   * :bind — bind address, by default any (0.0.0.0)
   * :handler — the nREPL message handler to use for each incoming connection;
       defaults to the result of `(default-handler)`
   * :transport-fn — a function that, given a java.net.Socket corresponding
       to an incoming connection, will return an value satisfying the
       clojure.tools.nrepl.Transport protocol for that Socket.
   * :ack-port — if specified, the port of an already-running server
       that will be connected to to inform of the new server's port.
       Useful only by Clojure tooling implementations.

   Returns a (map) handle to the server that is started, which may be stopped
   either via `stop-server`, (.close server), or automatically via `with-open`.
   The port that the server is open on is available in the :port slot of the
   server map (useful if the :port option is 0 or was left unspecified."
  [& {:keys [port bind transport-fn handler ack-port greeting-fn] :or {port 0}}]
  (let [bind-addr (when bind (first (.AddressList (Dns/GetHostEntry bind))))
        ss
        (if bind-addr
          (ServerSocket. bind-addr port)
          (ServerSocket. port))
        server (assoc
                 (Server. ss
                          (.getLocalPort ss)
                          (atom #{})
                          (or transport-fn t/bencode)
                          greeting-fn
                          (or handler (default-handler)))
                 ;; TODO here for backward compat with 0.2.x; drop eventually
                 :ss ss)]
    (future (accept-connection server))
    (when ack-port
      (ack/send-ack (:port server) ack-port))
    server))
