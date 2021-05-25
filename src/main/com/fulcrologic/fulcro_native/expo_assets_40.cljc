(ns com.fulcrologic.fulcro-native.expo-assets-40
  (:require
    #?@(:cljs [["expo-app-loading" :default AppLoading]
               ["expo-asset" :refer [Asset]]
               ["expo-font" :as Font]])
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro-native.alpha.components :as comp]))

(defn- cache-images
  [images]
  #?(:cljs (for [image images]
             (when image
               (.loadAsync Asset image)))))

(defn- cache-fonts
  [fonts]
  #?(:cljs (for [[n f] fonts]
             (.loadAsync Font n f))))

(defn- cast-as-array
  [coll]
  #?(:cljs (if (or (array? coll)
                 (not (reduceable? coll)))
             coll
             (into-array coll))))

(defn all
  [coll]
  #?(:cljs (.call (aget js/Promise "all") js/Promise (cast-as-array coll))))

(defn cache-assets
  [fonts images cb]
  (log/info "fonts:" fonts)
  (log/info "images:" images)
  #?(:cljs
     (->
       (all
         (concat
           (if (seq images)
             (cache-images (clj->js images)))
           (if (seq fonts)
             (cache-fonts (clj->js fonts)))))
       (.then (fn [resp]
                (if cb (cb))))
       (.catch (fn [err]
                 (log/error "Loading assets failed: " (aget err "message")))))))

(def app-loading #?(:cljs (comp/react-factory AppLoading) :clj :stub))