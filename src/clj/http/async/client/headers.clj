;; Lazy headers.

; Copyright 2010 Hubert Iwaniuk
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns http.async.client.headers "Asynchrounous HTTP Client - Clojure - Lazy headers"
    {:author "Hubert Iwaniuk"}
    (:import (org.asynchttpclient HttpResponseHeaders)
             (io.netty.handler.codec.http HttpHeaders))
    (:require [clojure.string :as string]))

(defn http-headers->map
  "Converts an instance of HttpHeaders into a map with keyword keys."
  [^HttpHeaders headers]
  (reduce (fn [m entry]
            (let [k (-> entry ^String key string/lower-case keyword)
                  v (val entry)]
              (assoc m k (if-let [vals (get m k)]
                           (conj (if (vector? vals) vals [vals]) v)
                           v))))
          nil
          (.entries headers)))

(defn convert-headers-to-map
  "Converts Http Response Headers to lazy map."
  [#^HttpResponseHeaders headers]
  (http-headers->map (.getHeaders headers)))

;; Creates cookies from headers.
(defn create-cookies
  "Creates cookies from headers."
  [headers]
  (if (contains? headers :set-cookie)
    (for [cookie-string (let [set-cookie (:set-cookie headers)]
                          (if (string? set-cookie) (vector set-cookie) set-cookie))]
      (let [name-token (atom true)]
        (into {}
              (for [cookie (string/split cookie-string #";")]
                (let [keyval (map string/trim (string/split cookie #"=" 2))]
                  (if @name-token
                    (do
                      (compare-and-set! name-token true false)
                      {:name (first keyval) :value (second keyval)})
                    [(keyword (first keyval)) (second keyval)]))))))
    (println "No Set-Cookie header.")))
