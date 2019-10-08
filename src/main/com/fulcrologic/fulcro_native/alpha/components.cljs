(ns com.fulcrologic.fulcro-native.alpha.components
  (:require
    [com.fulcrologic.fulcro.components :refer [force-children]]
    ["react-native" :as rn]
    ["react" :as r]
    [clojure.string :as str]
    [clojure.walk :as w]
    [clojure.set :as set]
    [taoensso.timbre :as log]))

(def create-element r/createElement)

(defn ui-text
  "Create a rn Text component."
  ([props s] (create-element rn/Text (clj->js props) s))
  ([s] (create-element rn/Text #js {} s)))

(defn react-factory
  "Returns a factory for raw JS React classes.

  ```
  (def ui-thing (react-factory SomeReactLibComponent))

  ...
  (defsc X [_ _]
    (ui-thing {:value 1}))
  ```

  The returned function will accept CLJS maps as props (not optional) and then any number of children. The CLJS props
  will be converted to js for interop. You may pass js props as an optimization."
  [js-component-class]
  (fn [props & children]
    (let [cs (force-children children)
          c  (first cs)]
      (if (and c (string? c) (= 1 (count cs)))
        (create-element
          js-component-class
          (clj->js props)
          (ui-text {} c))
        (apply create-element
          js-component-class
          (clj->js props)
          cs)))))


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
(def platform rn/Platform)
(def style-sheet rn/StyleSheet)

(defn create-style-sheet
  "Create a React Native style sheet. You can use kebab case and it will be auto-converted to camel case."
  [styles]
  (rn/StyleSheet.create
    (clj->js
      (zipmap (keys styles)
        (map #(map-keys->camel-case % :html-props true) (vals styles))))))

(def ui-activity-indicator (react-factory rn/ActivityIndicator))
(def ui-button (react-factory rn/Button))
(def ui-date-picker-ios (react-factory rn/DatePickerIOS))
(def ui-drawer-layout-android (react-factory rn/DrawerLayoutAndroid))
(def ui-flat-list (react-factory rn/FlatList))
(def ui-image (react-factory rn/Image))
(def ui-image-background (react-factory rn/ImageBackground))
(def ui-input-accessory-view (react-factory rn/InputAccessoryView))
(def ui-keyboard-avoiding-view (react-factory rn/KeyboardAvoidingView))
(def ui-modal (react-factory rn/Modal))
(def ui-picker (react-factory rn/Picker))
(def ui-picker-ios (react-factory rn/PickerIOS))
(def ui-progress-view-ios (react-factory rn/ProgressViewIOS))
(def ui-refresh-control (react-factory rn/RefreshControl))
(def ui-safe-area-view (react-factory rn/SafeAreaView))
(def ui-scroll-view (react-factory rn/ScrollView))
(def ui-section-list (react-factory rn/SectionList))
(def ui-segmented-control-ios (react-factory rn/SegmentedControlIOS))
(def ui-slider (react-factory rn/Slider))
(def ui-status-bar (react-factory rn/StatusBar))
(def ui-switch (react-factory rn/Switch))
(def ui-tab-bar-ios (react-factory rn/TabBarIOS))
;(def ui-tab-bar-ios-item (react-factory (.-Item rn/TabBarIOS)))
(def ui-text-input (react-factory rn/TextInput))
(def ui-touchbar-android (react-factory rn/TouchbarAndroid))
(def ui-touchable-highlight (react-factory rn/TouchableHighlight))
(def ui-touchable-native-feedback (react-factory rn/TouchableNativeFeedback))
(def ui-touchable-opacity (react-factory rn/TouchableOpacity))
(def ui-touchable-without-feedback (react-factory rn/TouchableWithoutFeedback))
(def ui-view (react-factory rn/View))
(def ui-view-pager-android (react-factory rn/ViewPagerAndroid))
(def ui-virtualized-list (react-factory rn/VirtualizedList))

(defn ios?
  "returns true if running in iOS"
  []
  (= "ios" (.-OS platform)))

(defn android?
  "returns true if running in Android"
  []
  (= "android" (.-OS platform)))
