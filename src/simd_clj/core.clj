(ns simd-clj.core
  (:require [clojure.string :as s])
  (:import [jdk.incubator.vector Vector]
           [jdk.incubator.vector VectorMask]
           ;; This is was the JDK uses on all operators to make sure results
           ;; stay in a register and don't get moved to memory.
           [jdk.internal.vm.annotation ForceInline]))

(set! *warn-on-reflection* true)

(def ^:const vector-types '("Byte" "Short" "Int" "Long" "Float" "Double"))

(def ^:const bit-sizes [64 128 256 512])

(defn create-species-kw
  "Returns a keyword in the form of `:int-256` when  given
one of the `vector-types` and `bit-sizes`."
  [class-name size]
  (keyword (str (s/lower-case class-name) "-" size)))

(defn create-species-enum
  "Returns a namespaced symbol of a Vector species."
  [class-name size]
  (symbol (str "jdk.incubator.vector." class-name "Vector")
          (str "SPECIES_" size)))

(defn create-species-fn
  "Returns a namespaced symbol of a function for a given Vector species."
  [class-name fn-name]
  (symbol (str "jdk.incubator.vector." class-name "Vector")
          (name fn-name)))

(defn call-defop [name args vector-type size]
  (let [species-kw   (create-species-kw   vector-type size)
        species-enum (create-species-enum vector-type size)
        body         (create-species-fn   vector-type name)]
    `(defmethod ~name ~species-kw [~@args ~'type]
       (~body ~species-enum ~@args))))

(defn call-defop-multi [name args]
  `(defmulti ~name
     (fn [~@args ~'vec-type] ~'vec-type)))

(defmacro defop
  {:clj-kondo/ignore [:unresolved-symbol]
   :clj-kondo/lint-as 'clojure.core/def}
  [name args]
  `(do
     ~(call-defop-multi name args)
     ~@(for [vector-type vector-types
             size        bit-sizes]
         (call-defop name args vector-type size))))

(defop broadcast [scalar])

(defn add
  {ForceInline true}
  ([^Vector vector scalar-or-vector ^VectorMask mask]
   (.add vector scalar-or-vector mask))
  ([^Vector vector scalar-or-vector]
   (.add vector scalar-or-vector)))

(defn abs
  {ForceInline true}
  [^Vector vector]
  (.abs vector))
