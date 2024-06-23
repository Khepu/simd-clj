(ns simd-clj.core
  {:clj-kondo/config '{:lint-as {simd-clj.core/defop :defn}
                       :indent-as {simd-clj.core/defop :defn}}}
  (:require [clojure.string :as s]))

(set! *warn-on-reflection* true)

(def ^:const vector-types '("Byte" "Short" "Int" "Long" "Float" "Double"))

(def ^:const bit-sizes [64 128 256 512])

(defn create-species-kw [class-name size]
  (let [name (s/lower-case class-name)]
    (keyword (str name "-" size))))

(defn create-species-enum [class-name size]
  (symbol (str "jdk.incubator.vector." (str (name class-name) "Vector"))
          (str "SPECIES_" size)))

(defn create-species-fn [class-name fn-name]
  (symbol (str "jdk.incubator.vector." (str (name class-name) "Vector"))
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

(defmacro defop [name args]
  {:clj-kondo/ignore [:unresolved-symbol]}
  `(do
     ~(call-defop-multi name args)
     ~@(for [vector-type vector-types
             size        bit-sizes]
         (call-defop name args vector-type size))))

(defop broadcast [value])
