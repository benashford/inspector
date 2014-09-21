(ns inspector.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s])
  (:import [java.lang.reflect Field Modifier]))

(defn- class-hierarchy [^Class class]
  (let [parent-classes (fn parent-classes [^Class class]
                         (lazy-seq
                          (when-let [parent-class (.getSuperclass class)]
                            (cons parent-class (parent-classes parent-class)))))]
    (lazy-seq (cons class (parent-classes class)))))

(defn- is-static? [modifiers]
  (Modifier/isStatic modifiers))

(declare inspect)

(defn- inspect-fields [seen ^Object obj ^Class class]
  {:class  class
   :fields (for [^Field field (.getDeclaredFields class)
                 :let [modifiers (.getModifiers field)]
                 :when (not (is-static? modifiers))]
             (do
               (.setAccessible field true)
               {:type  (.getType field)
                :name  (.getName field)
                :value (inspect seen (.get field obj))}))})

(def ^:private not-reflectable #{java.lang.Boolean
                                 java.lang.Byte
                                 java.lang.Character
                                 java.lang.Short
                                 java.lang.Integer
                                 java.lang.Long
                                 java.lang.Float
                                 java.lang.Double})

(defn inspect
  ([obj]
     (inspect #{} obj))
  ([seen ^Object obj]
     (if obj
       (let [class     (.getClass obj)
             id        (System/identityHashCode obj)
             classes   (reverse (class-hierarchy class))
             fields    classes
             new-seen  (conj seen id)
             inspected {:class    class
                        :identity id}]
         (cond
          (not-reflectable class) (assoc inspected :value (str obj))
          (seen id)               (assoc inspected :fields :already-seen)
          (.isArray class)        (assoc inspected
                                    :fields [{:class class
                                               :fields (map-indexed (fn [idx ^Object o]
                                                                      {:type (when o (.getClass o))
                                                                       :name idx
                                                                       :value (inspect new-seen o)})
                                                                    obj)}])
          :else                   (assoc inspected
                                    :fields (map (partial inspect-fields new-seen obj)
                                                 classes))))
       {:class :nil})))

(defn pp-inspect [obj]
  (pprint (inspect obj)))

(defn- gap [depth]
  (s/join (repeat depth " ")))

(defn- class-name [^Class class]
  (if (keyword? class)
    class
    (when class
      (.getName class))))

(defn inspect-and-report
  ([obj]
     (s/join "\n" (inspect-and-report (inspect obj) 0)))
  ([inspection depth]
     (flatten
      (cons
       (let [line (format "%s+ [%s] (%d)"
                          (gap depth)
                          (class-name (:class inspection))
                          (:identity inspection))]
         (if-let [value (:value inspection)]
           (str line " = " value)
           line))
       (for [field-class (:fields inspection)
             field       (:fields field-class)]
         (cons (format "%s  \"%s\" (on: %s) (type: %s)"
                       (gap (+ depth 2))
                       (:name field)
                       (class-name (:class field-class))
                       (class-name (:type field)))
               (inspect-and-report (:value field) (+ depth 5))))))))
