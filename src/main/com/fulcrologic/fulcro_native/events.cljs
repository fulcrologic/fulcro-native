(ns com.fulcrologic.fulcro-native.events)

(defn event-text
  "Returns the text value produced by a TextInput onChange event."
  [text-event]
  (some-> text-event .-nativeEvent .-text))
