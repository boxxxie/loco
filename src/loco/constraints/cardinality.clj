(ns loco.constraints.cardinality
  (:use loco.utils)
  (:use loco.constraints.utils)
  (:require
   [loco.vars :as vars]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'cardinality)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of int-var?)
                       (s/tuple (s/coll-of int?) (s/coll-of int-var?))
                       (s/tuple #{'closed} boolean?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [values occurrences] [_ closed?]]}
           (.globalCardinality model
                               (into-array IntVar vars)
                               (int-array values)
                               (into-array IntVar occurrences)
                               closed?)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn cardinality
  "Takes a list of variables, and a frequency map (from numbers to
  frequencies), constrains that the frequency map is accurate. If
  the :closed flag is set to true, any keys that aren't in the
  frequency map can't appear at all in the list of variables.

  cardinality will generate vars in the model/compile phase

  Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos})
  => {:a 1, :b 1, :c 2, :d 2, :e 2 :ones 2, :twos 3}"
  {:choco "globalCardinality(IntVar[] vars, int[] values, IntVar[] occurrences, boolean closed)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html"}
  ([variables frequencies]
   (cardinality variables frequencies false))

  ([variables frequencies closed?]
   {:pre [
          (map? frequencies)
          (sequential? variables)
          (#{:closed true false} closed?)
          (every? integer? (keys frequencies))
          (distinct? (keys frequencies))
          (distinct? (vals frequencies))
          ]
    }
   (let [closed (get {:closed true} closed? closed?)
         values (preserve-consts (vec (keys frequencies)))
         occurences (vec (vals frequencies))
         cardinality-constraint (constraint constraint-name
                                            [(vec variables)
                                             [values occurences]
                                             ['closed closed]]
                                            compiler)]
     (-> (concat
          (mapv (p vars/proto cardinality-constraint (vec variables)) occurences)
          [cardinality-constraint])
         (with-meta {:generated-vars true})))))
