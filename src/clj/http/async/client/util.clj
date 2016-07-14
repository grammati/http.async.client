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

(ns http.async.client.util
  "Asynchronous HTTP Client - Clojure - Utils"
  {:author "Hubert Iwaniuk"}
  (:import (org.asynchttpclient.proxy ProxyServer
                                      ProxyServer$Builder)
           (org.asynchttpclient DefaultAsyncHttpClientConfig$Builder
                                RequestBuilder
                                Realm
                                Realm$AuthScheme
                                Realm$Builder)
           (io.netty.handler.ssl ClientAuth)))

(defn- type->auth-scheme [type]
  (or ({:basic Realm$AuthScheme/BASIC
        :digest Realm$AuthScheme/DIGEST} type)
      Realm$AuthScheme/BASIC))

(defn build-realm
  "Build and return a Realm"
  ^Realm
  [{:keys [type user password realm preemptive target-proxy]
    :or {:type :basic}}]
  (when (nil? user)
    (if (nil? password)
      (throw (IllegalArgumentException. "For authentication user and password is required"))
      (throw (IllegalArgumentException. "For authentication user is required"))))
  (when (nil? password)
    (throw (IllegalArgumentException. "For authentication password is required")))
  (when (and (= :digest type) (nil? realm))
    (throw (IllegalArgumentException. "For DIGEST authentication realm is required")))
  (let [rbld (Realm$Builder. user password)]
    (.setScheme rbld (type->auth-scheme type))
    (when (= :digest type)
      (.setRealmName rbld realm))
    (when-not (nil? preemptive)
      (.setUsePreemptiveAuth rbld preemptive))
    (when-not (nil? target-proxy)
      ;;(.setTargetProxy rbld target-proxy)
      (throw (IllegalArgumentException. "target-proxy parameter is no longer supported")))
    (.build rbld)))

(defn set-realm
  "Sets realm on builder.
  Note that in v.1.0.0 you must set a realm to enable HTTPS traffic
  via the proxy."
  [auth b]
  ;; b can be a RequestBuilder or a DefaultAsyncHttpClientConfig$Builder, so
  ;; there is reflection here
  (.setRealm b (build-realm auth)))

(defn- build-proxy
  ^ProxyServer
  [{:keys [protocol host port user password] :as config}]
  {:pre [(or (nil? protocol)
             (contains? #{:http :https} protocol))
         host port
         (or (and (nil? user) (nil? password))
             (and user password))]}
  (let [psb (ProxyServer$Builder. host port)]
    (when user
      (.setRealm psb (build-realm config)))
    (.build psb)))

(defn set-proxy
  "Sets proxy on builder.
  Note that in v1.0.0 you must also set a realm to enable HTTPS
  traffic via the proxy."
  [config ^RequestBuilder b]
  (.setProxyServer b (build-proxy config)))


(defprotocol ISslContext
  "Things that can be turned into an io.netty.handler.ssl.SslContext"
  (->ssl-context [this]))

(extend-protocol ISslContext
  io.netty.handler.ssl.SslContext
  (->ssl-context [this] this)
  javax.net.ssl.SSLContext
  (->ssl-context [this]
    (io.netty.handler.ssl.JdkSslContext. this true ClientAuth/NONE)))

(defn set-ssl-context
  "Sets SSL Context on the builder."
  [ssl-context ^DefaultAsyncHttpClientConfig$Builder b]
  (.setSslContext b (->ssl-context ssl-context)))
