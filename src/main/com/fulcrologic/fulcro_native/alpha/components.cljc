(ns com.fulcrologic.fulcro-native.alpha.components
  (:require
    [com.fulcrologic.fulcro-native.events :as evt]
    [com.fulcrologic.fulcro.components :as comp :refer [force-children defsc]]
    #?@(:cljs [[goog.object :as gobj]
               ["react-native" :as react-native-package]
               ["react" :as react-package]])
    [clojure.string :as str]
    [clojure.walk :as w]
    [clojure.set :as set]
    [taoensso.timbre :as log]))

#?(:clj (def clj->js identity))
(def r #?(:cljs react-package :clj {}))
(def rn #?(:cljs react-native-package :clj {}))
(def create-element #?(:cljs react-package/createElement :clj identity))

(defn- remove-separators [s]
  (when s
    (str/replace s #"^[.#]" "")))

(defn- get-tokens [k]
  (re-seq #"[#.]?[^#.]+" (name k)))

(defn parse-style-keyword
  "Parse shorthand keyword and return sequence of individual style keys.

  (parse :.klass3.klass1.klass2)
  => [:klass3 :klass1 :klass2]"
  [k]
  (if (keyword? k)
    (let [tokens  (get-tokens k)
          classes (->> tokens (clojure.core/filter #(re-matches #"^\..*" %)))]
      (when-not (re-matches #"^(\.[^.#]+|#[^.#]+)+$" (name k))
        (throw (ex-info "Invalid style keyword." {:item k})))
      (into []
        (comp
          (keep remove-separators)
          (map keyword))
        classes))
    nil))

(defn fix-style
  "Set the style on `props` to whatever the component-local style is on the
  in-context *parent* at key `s` (if and only if it exists). Otherwise dissociates
  the :style prop altogether."
  [props]
  (let [style  (:style props)
        styles (:styles props)
        props  (dissoc props :styles)]
    (cond-> (if-let [ks (seq
                          (keep identity
                            (concat
                              (mapcat parse-style-keyword styles)
                              (parse-style-keyword style))))]
              (assoc props
                :style
                (reduce
                  (fn [style k]
                    (if-let [declared-style (some-> comp/*parent*
                                              (comp/component-options :style)
                                              (get k))]
                      (merge style declared-style)
                      style))
                  {}
                  ks))
              props)
      (map? style) (update :style merge style))))

(defn rewrite-props
  "Handle any magical props that Fulcro supports"
  [props]
  (if (map? props)
    (fix-style props)
    props))

(declare ui-text)

(defn react-factory
  "Returns a factory for raw JS React classes.

  options can include:

  `:ui-text` - Specify how to do auto-wrapping of raw strings (default is to use native base Text component).  You
  may specify `false` to create a factory that does no special wrapping or a `(fn [str] element)` that returns the
  element-wrapped string.

  ```
  (def ui-thing (react-factory SomeReactLibComponent))

  ...

  (defsc X [_ _]
    (ui-thing {:value 1}))
  ```

  The returned function will accept CLJS maps as props (not optional) and then any number of children. The CLJS props
  will be converted to js for interop. You may pass js props as an optimization."
  ([js-component-class {:keys [ui-text]}]
   (fn [props & children]
     (let [cs    (force-children children)
           props (rewrite-props props)
           c     (first cs)]
       (if (and c ui-text (string? c) (= 1 (count cs)))
         (create-element
           js-component-class
           (clj->js props)
           (ui-text c))
         (apply create-element
           js-component-class
           (clj->js props)
           cs)))))
  ([js-component-class]
   (react-factory js-component-class {:ui-text (fn [child]
                                                 (create-element
                                                   (comp/isoget rn "Text")
                                                   nil
                                                   child))})))

(defn ui-text
  "Create a rn Text component."
  ([props s] (create-element (comp/isoget rn "Text") (clj->js (rewrite-props props)) s))
  ([s] (create-element (comp/isoget rn "Text") #js {} s)))

(defsc WrappedInput
  "A component that can wrap a react native text input and properly coordinate Fulcro props changes using
   a React component-local state buffer."
  [this {:keys [onChange] :as input-props} {:keys [element-factory]}]
  {:componentDidMount  (fn [this]
                         (if-let [v (some-> this comp/props :value)]
                           (comp/set-state! this {:value v})))
   :componentDidUpdate (fn [this pp ps]
                         (let [visible-value   (comp/get-state this :value)
                               old-props-value (:value pp)
                               props-value     (-> this comp/props :value)]
                           (when (and props-value (not= props-value old-props-value) (not= props-value visible-value))
                             (comp/set-state! this {:value props-value}))))
   :initLocalState     (fn [this]
                         {:on-change (fn [evt] (comp/set-state! this {:value (evt/event-text evt)}))
                          :save-ref! (fn [r] #?(:cljs (gobj/set this "wrapped-input" r)))})}
  (let [{:keys [save-ref! on-change value]} (comp/get-state this)]
    (when element-factory
      (let [props (-> input-props
                    (assoc
                      :value value
                      :onChange (fn [evt]
                                  (on-change evt)
                                  (when onChange
                                    (onChange evt)))
                      :ref save-ref!)
                    clj->js)]
        (element-factory props)))))

(def ui-wrapped-input (comp/computed-factory WrappedInput))

(defn wrap-text-input
  "Wraps a native text input factory such that Fulcro props updates will work without the cursor jumping about.

  `input-factory` is a low-level (js) React control that acts like a TextInput.

  Returns a `(fn [js-props])` that can be used as a UI element factory."
  [input-factory]
  (fn [props]
    (ui-wrapped-input props {:element-factory input-factory})))

;; copy from natal-shell
(def camel-rx #"([a-z])([A-Z])")

(defn to-kebab [s]
  (-> s
    (str/replace camel-rx "$1-$2")
    (str/replace "." "-")
    str/lower-case))

(defn sp [js-name]
  (str/split js-name #"\."))

(defn kebab-case->camel-case
  "Converts from kebab case to camel case, eg: on-click to onClick"
  [input]
  (let [words      (str/split input #"-")
        capitalize (->> (rest words)
                     (map #(apply str (str/upper-case (first %)) (rest %))))]
    (apply str (first words) capitalize)))

(defn map-keys->camel-case
  "Stringifys all the keys of a cljs hashmap and converts them
   from kebab case to camel case. If :html-props option is specified,
   then rename the html properties values to their dom equivalent
   before conversion"
  [data & {:keys [html-props]}]
  (let [convert-to-camel (fn [[key value]]
                           [(kebab-case->camel-case (name key)) value])]
    (w/postwalk (fn [x]
                  (if (map? x)
                    (let [new-map (if html-props
                                    (set/rename-keys x {:class :className :for :htmlFor})
                                    x)]
                      (into {} (map convert-to-camel new-map)))
                    x))
      data)))

;; apis
(def platform (comp/isoget rn "Platform"))
(def style-sheet (comp/isoget rn "StyleSheet"))

(defn create-style-sheet
  "Create a React Native style sheet. You can use kebab case and it will be auto-converted to camel case."
  [styles]
  #?(:cljs
     (react-native-package/StyleSheet.create
       (clj->js
         (zipmap (keys styles)
           (map #(map-keys->camel-case % :html-props true) (vals styles)))))))

(def ui-activity-indicator (react-factory (comp/isoget rn "ActivityIndicator")))
(def ui-button (react-factory (comp/isoget rn "Button")))
(def ui-drawer-layout-android (react-factory (comp/isoget rn "DrawerLayoutAndroid")))
(def ui-flat-list (react-factory (comp/isoget rn "FlatList")))
(def ui-image (react-factory (comp/isoget rn "Image")))
(def ui-image-background (react-factory (comp/isoget rn "ImageBackground")))
(def ui-input-accessory-view (react-factory (comp/isoget rn "InputAccessoryView")))
(def ui-keyboard-avoiding-view (react-factory (comp/isoget rn "KeyboardAvoidingView")))
(def ui-modal (react-factory (comp/isoget rn "Modal")))
(def ui-refresh-control (react-factory (comp/isoget rn "RefreshControl")))
(def ui-safe-area-view (react-factory (comp/isoget rn "SafeAreaView")))
(def ui-scroll-view (react-factory (comp/isoget rn "ScrollView")))
(def ui-section-list (react-factory (comp/isoget rn "SectionList")))
(def ui-status-bar (react-factory (comp/isoget rn "StatusBar")))
(def ui-switch (react-factory (comp/isoget rn "Switch")))
(def ui-tab-bar-ios (react-factory (comp/isoget rn "TabBarIOS")))
;(def ui-tab-bar-ios-item (react-factory (.-Item (comp/isoget rn "TabBarIOS"))))
(def ui-text-input (react-factory (comp/isoget rn "TextInput")))
(def ui-touchbar-android (react-factory (comp/isoget rn "TouchbarAndroid")))
(def ui-touchable-highlight (react-factory (comp/isoget rn "TouchableHighlight")))
(def ui-touchable-native-feedback (react-factory (comp/isoget rn "TouchableNativeFeedback")))
(def ui-touchable-opacity (react-factory (comp/isoget rn "TouchableOpacity")))
(def ui-touchable-without-feedback (react-factory (comp/isoget rn "TouchableWithoutFeedback")))
(def ui-view (react-factory (comp/isoget rn "View")))
(def ui-virtualized-list (react-factory (comp/isoget rn "VirtualizedList")))

(defn ios?
  "returns true if running in iOS"
  []
  (= "ios" (comp/isoget platform "OS")))

(defn android?
  "returns true if running in Android"
  []
  (= "android" (comp/isoget platform "OS")))
