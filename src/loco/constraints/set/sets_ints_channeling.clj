(ns loco.constraints.set.sets-ints-channeling
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set-ints-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'sets} (s/coll-of set-var?) #{'offset} nat-int?)
                       (s/tuple #{'ints} (s/coll-of int-var?) #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ sets _ offset-set] [_ ints _ offset-ints]]}
           (.setsIntsChanneling model
                                (into-array SetVar sets)
                                (into-array IntVar ints)
                                offset-set
                                offset-ints)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn sets-ints-channeling
  "Creates a constraint channeling set variables and integer variables :
  x in sets[y] <=> ints[x] = y

  Creates a constraint channeling set variables and integer variables :
  x in sets[y-offset1] <=> ints[x-offset2] = y"
  {:choco ["setsIntsChanneling(SetVar[] sets, IntVar[] ints)"
           "setsIntsChanneling(SetVar[] sets, IntVar[] ints, int offset1, int offset2)"]}
  ([sets ints] (sets-ints-channeling sets 0 ints 0))
  ([sets offset-set ints offset-int]
   {:pre [(nat-int? offset-int) (nat-int? offset-set) (sequential? ints) (sequential? sets)]}
   (constraint constraint-name
               [['sets (vec sets) 'offset (preserve-consts offset-set)]
                ['ints (vec ints) 'offset (preserve-consts offset-int)]]
               compiler)))
