(ns loco.constraints.at-least-n-values ;; FIXME: WIP
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'at-least-n-values)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/cat
                       :ints     ::utils/coll-coerce-intvar?
                       :n-values (s/tuple #{'n-values} ::utils/coerce-intvar?)
                       :ac       (s/tuple #{'ac} boolean?)))))

(compile-function
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args {:ints ?vars :n-values [_ ?n-values] :ac [_ ?ac]} }
     (.atLeastNValues *model
                      (->> ?vars (map coerce-int-var) (into-array IntVar))
                      (coerce-int-var ?n-values)
                      ?ac))))

(defn $at-least-n-values
  "Creates an atLeastNValue constraint.
  Let N be the number of distinct values assigned to the variables of the vars collection.
  Enforce condition N >= nValues to hold.
  AC is false by default."
  {:choco "atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)"}
  ([vars n-values]
   ($at-least-n-values vars n-values false))

  ([vars n-values ac?]
   {:pre [(sequential? vars) (boolean? ac?)]}
   (constraint constraint-name
               [(vec vars)
                ['n-values n-values]
                ['ac ac?]]
               compiler)))
