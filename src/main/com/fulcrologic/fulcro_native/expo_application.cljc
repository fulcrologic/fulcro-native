(ns com.fulcrologic.fulcro-native.expo-application
  (:require
    #?@(:cljs [["expo" :as expo]
               ["create-react-class" :as crc]])
    [com.fulcrologic.fulcro-native.expo-assets :as assets]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.rendering.keyframe-render :as kr]
    [taoensso.timbre :as log]))

(defonce root-ref (atom nil))
(defonce root-component-ref (atom nil))

(defn render-root
  "A function that can be used as the main Fulcro render for an expo app."
  [fonts images root]
  #?(:cljs
     (try
       (let [first-call? (nil? @root-ref)]
         (reset! root-ref root)

         (if-not first-call?
           (when-let [root @root-component-ref]
             (.forceUpdate ^js root)
             root-component-ref)
           (let [Root
                 (crc
                   #js {:getInitialState
                        (fn []
                          #js {:assetsLoaded false})
                        :componentDidMount
                        (fn []
                          (this-as ^js this
                            (reset! root-component-ref this)
                            (assets/cache-assets fonts images (fn []
                                                                (.setState this #js {:assetsLoaded true})))))
                        :componentWillUnmount
                        (fn []
                          (reset! root-component-ref nil))
                        :render
                        (fn []
                          (this-as this
                            (if this.state.assetsLoaded
                              (let [body @root-ref]
                                (try
                                  (if (fn? body)
                                    (body)
                                    body)
                                  (catch :default e
                                    (log/error e "Render failed"))))
                              (assets/app-loading))
                            ))})]
             (expo/registerRootComponent Root))))
       (catch :default e
         (log/error e "Unable to mount/refresh")))))

(defn fulcro-app
  "Identical to com.fulcrologic.fulcro.application/fulcro-app, but modifies a few options
  to ensure initial mount works properly for Native Expo apps."
  [{:keys [cached-fonts cached-images] :as options}]
  (app/fulcro-app
    (merge
      {:optimized-render! kr/render!
       :render-root!      (partial render-root cached-fonts cached-images)}
      options)))
