(ns com.fulcrologic.fulcro-native.net.browser-cookies
  "Functions necessary to have your mobile app pretend to be a browser that
  supports cookies. To use this with a Fulcro HTTP remote in a Native app:

  * Make sure you call `initialize-cookie-store!`, which is async. Do not continue building your app until this returns.
  * Add `wrap-native-cookie-request` and `wrap-native-cookie-response` into the http-remote middleware.

  If you are using websockets, then....TODO.
  "
  (:require
    #?(:cljs ["expo-secure-store" :as SecureStore])
    [com.fulcrologic.fulcro.algorithms.transit :as fcutil]
    [clojure.string :as str]
    [clojure.core.async :as async]
    [taoensso.timbre :as log]
    [clojure.string :as str])
  #?(:clj (:import
            (java.text SimpleDateFormat)
            (java.util Locale TimeZone)
            (java.net URLDecoder))))

(defn- now []
  #?(:clj (java.util.Date.) :cljs (js/Date.)))

(defn- parse-int [v]
  (let [v (str/trim v)]
    #?(:clj  (Long/parseLong v)
       :cljs (js/parseInt v))))

(defn parse-http-date [v]
  (let [v (str/trim v)]
    #?(:clj  (let [format (SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss zzz" Locale/US)]
               (.setTimeZone format (TimeZone/getTimeZone "UTC"))
               (.parse format v))
       :cljs (js/Date. (.parse js/Date v)))))

(defn- parse-assignment [assignment]
  (let [idx (str/index-of assignment \=)]
    [(str/trim (subs assignment 0 idx)) (str/trim (subs assignment (inc idx)))]))

(defn- parse-assignment-directive [raw-directive]
  (let [[name value] (parse-assignment raw-directive)
        name  (keyword (str/trim (str/lower-case name)))
        value (case name
                :max-age (parse-int value)
                :expires (parse-http-date value)
                :samesite (keyword (str/lower-case value))
                value)]
    {name value}))

(defn- parse-directive [raw-directive]
  (let [lc (str/trim (str/lower-case raw-directive))]
    (cond
      (= lc "httponly") {:http-only? true}
      (= lc "secure") {:secure? true}
      :else (parse-assignment-directive raw-directive))))

(defn parse-set-cookie
  "Attempt to parse a set-cookie header value."
  [header]
  (let [[assignment & directives] (str/split header #"[;]")
        [name value] (parse-assignment assignment)
        base-cookie {:created (now)
                     :name    name
                     :value   value}]
    (reduce
      (fn [cookie raw-directive]
        (merge cookie (parse-directive raw-directive)))
      base-cookie
      directives)))

(defn- decode [x]
  (when x
    (try
      (fcutil/transit-str->clj x)
      (catch :default e
        (log/debug "could not decode" (pr-str x))
        x))))

;; Write through cache
(defonce cookie-cache (atom {}))

(def cache-key "fulcro-browser-cookie-cache")

(defn- save-cache!
  "Make sure the cache is in secure store. Does nothing in CLJ."
  []
  #?(:cljs
     (-> SecureStore
       (.setItemAsync cache-key (fcutil/transit-clj->str @cookie-cache) #js {})
       (.catch (fn [e] (log/error e "Unable to save cookie cache!"))))))

(defn save-cookie!
  "Put a cookie into the cookie store. Cookies are available for fetch immediately, though this does queue an async
   save of the cookies to secure storage."
  [cookie-name cookie-map]
  (swap! cookie-cache assoc cookie-name cookie-map)
  (save-cache!))

(defn expired?
  "looks in the given cookie map and returns true if that cookie should be expired."
  [{:keys [created max-age expires] :as cookie}]
  (let [now-ms     (inst-ms (now))
        created-ms (or (some-> created (inst-ms)) 0)
        age-ms     (- now-ms created-ms)
        max-age-ms (some-> max-age (* 1000))
        expires-ms (some-> expires (inst-ms))]
    (or
      (and max-age-ms (> max-age-ms age-ms))
      (and expires-ms (> now-ms expires-ms)))))

(defn- expire-cookies [cookies]
  (reduce-kv
    (fn [c k v]
      (if (expired? v)
        (do
          (log/debug "Expiring cookie " k v)
          c)
        (assoc c k v)))
    {}
    cookies))

(defn initialize-cookie-store!
  "Initialize the cookie store. MUST be called when the application starts, before attempting to use the store. This
   is an async request that will call `complete` when done. Does nothing in CLJ but call `complete`."
  [complete]
  #?(:clj  (complete)
     :cljs (-> SecureStore
             (.getItemAsync cache-key #js {})
             (.then (fn [v]
                      (reset! cookie-cache (expire-cookies (decode v)))
                      (when complete (complete))))
             (.catch (fn [e]
                       (log/error e "UNABLE TO RESTORE COOKIE STORE!")
                       (when complete (complete)))))))

