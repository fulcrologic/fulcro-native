(ns com.fulcrologic.fulcro-native.expo-assets
  (:require ["expo" :refer [AppLoading]]
            ["expo-asset" :refer [Asset]]
            ["expo-font" :as Font]
            [taoensso.timbre :as log]
            [com.fulcrologic.fulcro-native.alpha.components :as comp]))

(defn- cache-images
  [images]
  (for [image images]
    (when image
      (.loadAsync Asset image))))

(defn- cache-fonts
  [fonts]
  (for [[n f] fonts]
    (.loadAsync Font n f)))

(defn- cast-as-array
  [coll]
  (if (or (array? coll)
          (not (reduceable? coll)))
    coll
    (into-array coll)))

(defn all
  [coll]
  (.call (aget js/Promise "all") js/Promise (cast-as-array coll)))

(defn cache-assets
  [fonts images cb]
  (log/info "fonts:" fonts)
  (log/info "images:" images)
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
              (log/error "Loading assets failed: " (aget err "message"))))))

(def app-loading (comp/react-factory AppLoading))