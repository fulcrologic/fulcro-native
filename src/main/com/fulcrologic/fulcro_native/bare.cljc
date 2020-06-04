(ns com.fulcrologic.fulcro-native.bare
  "A Base Fulcro wrapper for building apps that work on Bare expo React Native apps.

  Usage:

  When you create your Fulcro app, set the root render function to the one in this namespace:

  ```
  (defonce app
    (app/fulcro-app
      {:render-root!      bare/render-root}))
  ```

  Note that this uses expo's registerRootComponent, which is the recommended setting for Bare expo projects.
  "
  (:require
    #?@(:cljs [["create-react-class" :as crc]
               ["expo" :refer [registerRootComponent]]])
    [taoensso.timbre :as log]))

(defonce root-ref (atom nil))
(defonce root-component-ref (atom nil))

(defn render-root!
  "A function that can be used as the main Fulcro render for a native app."
  [root]
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
                   #js {:componentDidMount
                        (fn []
                          (this-as ^js this
                            (reset! root-component-ref this)))
                        :componentWillUnmount
                        (fn [] (reset! root-component-ref nil))
                        :render
                        (fn []
                          (when-let [body @root-ref]
                            (try
                              (if (fn? body)
                                (body)
                                body)
                              (catch :default e
                                (log/error e "Render failed")))))})]
             (registerRootComponent Root))))
       (catch :default e
         (log/error e "Unable to mount/refresh")))))
