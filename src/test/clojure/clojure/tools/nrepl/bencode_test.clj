;-
;   Copyright (c) Meikel Brandmeyer. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; ns-mapping
; (add-ns-load-mapping "clojure.tools"
;                     "d:\\mtkoan\\dev\\tools.nrepl\\src\\main\\clojure\\clojure\\tools")

(ns clojure.tools.nrepl.bencode-test
  (:import
   [System.IO Stream SeekOrigin MemoryStream]
   [System.Text Encoding]
    ;java.io.ByteArrayInputStream
    ;java.io.ByteArrayOutputStream
    ;java.io.PushbackInputStream
    clojure.lang.RT
   )
  (:require [clojure.clr.io :as io]
            [clojure.tools.nrepl.bencode :as bencode])
  (:use
    [clojure.test :only [deftest is are]]))

(defn #^{:private true} >bytes
  [#^String input]
  (.GetBytes Encoding/UTF8 input))

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

(defn- decode
  [bytes & {:keys [reader]}]
  (reader (MemoryStream. bytes)))

(defn get-bytes [string]
  (.GetBytes Encoding/UTF8 string))

(defn get-string [bytes]
  (.GetString Encoding/UTF8 bytes))

(defn- >input
  [^String input & args]
  (-> input
      (get-bytes)
      (#(apply decode % args))
      <bytes))

(deftest test-netstring-reading
  (are [x y] (= (>input x :reader bencode/read-netstring) y)
    "0:,"                ""
    "13:Hello, World!,"  "Hello, World!"
    "17:Hällö, Würld!,"  "Hällö, Würld!"
    "14:Здравей, Свят!," "Здравей, Свят!"))

(deftest test-string-reading
  (are [x y] (= (>input x :reader bencode/read-bencode) y)
    "0:"                ""
    "13:Hello, World!"  "Hello, World!"
    "17:Hällö, Würld!"  "Hällö, Würld!"
    "14:Здравей, Свят!" "Здравей, Свят!"))

(deftest test-integer-reading
  (are [x y] (= (>input x :reader bencode/read-bencode) y)
    "i0e"     0
    "i42e"   42
    "i-42e" -42))

(deftest test-list-reading
  (are [x y] (= (>input x :reader bencode/read-bencode) y)
    "le"                    []
    "l6:cheesee"            ["cheese"]
    "l6:cheese3:ham4:eggse" ["cheese" "ham" "eggs"]))

(deftest test-map-reading
  (are [x y] (= (>input x :reader bencode/read-bencode) y)
    "de"            {}
    "d3:ham4:eggse" {"ham" "eggs"}))

(deftest test-nested-reading
  (are [x y] (= (>input x :reader bencode/read-bencode) y)
    "l6:cheesei42ed3:ham4:eggsee" ["cheese" 42 {"ham" "eggs"}]
    "d6:cheesei42e3:haml4:eggsee" {"cheese" 42 "ham" ["eggs"]}))

(defn- >stream
  [thing & {:keys [writer]}]
  (doto (MemoryStream.)
    (writer thing)))

(defn- >output
  [& args]
  (get-string (.ToArray (apply >stream args))))

(deftest test-netstring-writing
  (are [x y] (= (>output (>bytes x) :writer bencode/write-netstring) y)
    ""               "0:,"
    "Hello, World!"  "13:Hello, World!,"
    "Hällö, Würld!"  "17:Hällö, Würld!,"
    "Здравей, Свят!" "14:Здравей, Свят!,"))

(deftest test-byte-array-writing
  (are [x y] (= (>output (>bytes x) :writer bencode/write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "17:Hällö, Würld!"
    "Здравей, Свят!" "14:Здравей, Свят!"))

(deftest test-string-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "17:Hällö, Würld!"
    "Здравей, Свят!" "14:Здравей, Свят!"))

(deftest test-input-stream-writing
  (are [x y] (= (>output (MemoryStream. (>bytes x))
                         :writer bencode/write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "17:Hällö, Würld!"
    "Здравей, Свят!" "14:Здравей, Свят!"))

(deftest test-integer-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
      0 "i0e"
     42 "i42e"
    -42 "i-42e"

    ; Works for all integral types.
    ; Note: BigInts (42N) not tested, since they are not
    ; supported in 1.2.
    (byte  "42")    "i42e"
    (short  "42")   "i42e"
    (int "42") "i42e"
    (long  "42")    "i42e"))

(deftest test-named-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
    :foo      "3:foo"
    :foo/bar  "7:foo/bar"
    'foo      "3:foo"
    'foo/bar  "7:foo/bar"))

(deftest test-list-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
    nil                     "le"
    []                      "le"
    ["cheese"]              "l6:cheesee"
    ["cheese" "ham" "eggs"] "l6:cheese3:ham4:eggse"))

(deftest test-map-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
    {}             "de"
    {"ham" "eggs"} "d3:ham4:eggse"))

(deftest test-nested-writing
  (are [x y] (= (>output x :writer bencode/write-bencode) y)
    ["cheese" 42 {"ham" "eggs"}] "l6:cheesei42ed3:ham4:eggsee"
    {"cheese" 42 "ham" ["eggs"]} "d6:cheesei42e3:haml4:eggsee"))

(deftest test-lexicographic-sorting
  (let [source   ["ham" "eggs" "hamburg" "hamburger" "cheese"]
        expected ["cheese" "eggs" "ham" "hamburg" "hamburger"]
        to-test  (->> source
                   (map >bytes)
                   (sort @#'clojure.tools.nrepl.bencode/lexicographically)
                   (map <bytes))]
    (is (= to-test expected))))

(deftest unencoded-values
  ; just some PNG data that won't round-trip cleanly through UTF-8 encoding, so
  ; any default encoding in the bencode implementation will be caught immediately
  (let [binary-data (->> [ 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 100 0 0 0 100 8 6 0 0 0 112 226 149 84 0 0 3 240 105 67 67 80 73 67 67 32 80 114 111 102 105 108 101 0 0 40 145 141 85 221 111 219 84 20 63 137 111 92 164 22 63 160 177 142 14 21 139 175 85 83 91 185 27 26 173 198 6 73 147 165 233 66 26 185 205 216 42 164 201 117 110 ]
                      (map byte)
                      (into-array Byte))]
    (is (= (seq binary-data)
           (-> {"data" binary-data}
             (>stream :writer bencode/write-bencode)
             .ToArray
             (decode :reader bencode/read-bencode)
             (get "data")
             seq)))))

