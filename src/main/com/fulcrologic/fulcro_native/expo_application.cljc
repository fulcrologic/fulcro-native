
(ns dtvm-rad.fulcro-native-expo-substitute
  (:require
   #?@(:cljs [["expo" :as expo]
              ["create-react-class" :as crc]])
   [taoensso.timbre :as log]
   [com.fulcrologic.fulcro-native.expo-assets :as assets]
   [com.fulcrologic.fulcro.application :as app]
   [com.fulcrologic.fulcro.rendering.keyframe-render :as kr]))

(defonce root-ref (atom nil))
(defonce root-component-ref (atom nil))

(defn render-root
  "A function that can be used as the main Fulcro render for an expo app."
  [fonts images root]
  #?(:cljs
     (let [first-call? (nil? @root-ref)]
       (reset! root-ref root)

       (if-not first-call?
         (when-let [root @root-component-ref]
           (.forceUpdate ^js root))
         (let [Root
               (crc
                #js {:componentDidMount
                     (fn []
                       (this-as this
                         (reset! root-component-ref this)))
                     :componentWillUnmount
                     (fn []
                       (reset! root-component-ref nil))
                     :render
                     (fn []
                       (let [body @root-ref]
                         (if (fn? body)
                           (body)
                           body)))})]

           (expo/registerRootComponent Root))))))

(defn fulcro-app
  "Identical to com.fulcrologic.fulcro.application/fulcro-app, but modifies a few options
  to ensure initial mount works properly for Native Expo apps."
  [{:keys [cached-fonts cached-images] :as options}]
  (app/fulcro-app
   (merge
    {:optimized-render! kr/render!
     :render-root!      (partial render-root cached-fonts cached-images)}
    options)))
