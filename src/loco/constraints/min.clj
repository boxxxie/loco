;; FIXME: WIP

(ns loco.constraints.min
  (:use loco.constraints.utils)
  (:require
   [loco.utils :refer [p c]]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'min)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :bools       (s/tuple
                                     bool-var?
                                     (s/tuple #{'of}         (s/coll-of bool-var?)))

                       :ints        (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int-var?)))

                       :set         (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         set-var?)
                                     (s/tuple #{'not-empty?} boolean?))

                       :set-indices (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int?))
                                     (s/tuple #{'indices}    set-var?)
                                     (s/tuple #{'offset}     nat-int?)
                                     (s/tuple #{'not-empty?} boolean?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [(:or :ints :bools) [min [_ vars]]]}
           (.min model min (into-array vars))

           ;;{:args [:bools [min [_ vars]]]}

           {:args [:set [min [_ set] [_ not-empty?]]]}
           (.min model set min not-every?) ;;fugly API! bad choco!

           {:args [:set-indices [min [_ weights] [_ indices] [_ offset] [_ not-empty?]]]}
           (.min model indices (int-array weights) offset min not-every?)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (->> (interpose "_" (map name body))
              (apply str (name partial-name) "_"))))

(declare $min)
(defn- constraint-fn [var-name [op args]]
  ($min var-name args))

(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb d-lb :ub d-ub} {:lb (min lb d-lb)
                                                   :ub (min ub d-ub)}))
           ;;{:lb 0 :ub 0}
           body)
          (assoc :int true)
          (update :lb int)
          (update :ub int))))

(defn- min-partial
  "handles syntax like ($= :v ($min :a :b :c))"
  [vars]
  {:pre [(sequential? vars)]}
  (partial-constraint constraint-name (vec vars) name-fn constraint-fn domain-fn))

;;TODO: rearrange the arguments in $min away from choco, and into
;;something more consistent, also may be good to use spec instead of
;;defun
(defloco $min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers
  Creates a constraint over the minimum element in a set: min{i | i in set} = minElementValue
  Creates a constraint over the minimum element induces by a set: min{weights[i-offset] | i in indices} = minElementValue"
  {:choco
   ["min(IntVar min, IntVar[] vars)"
    "min(BoolVar max, BoolVar[] vars)"
    "min(SetVar set, IntVar minElementValue, boolean notEmpty)"
    "min(SetVar indices, int[] weights, int offset, IntVar minElementValue, boolean notEmpty)"]
   :arglists '([min-list]
               [min-var vars]
               [set-var min-var not-empty?]
               [set-indices weights offset min-var not-empty?]
               [& int-vars])}

  [& more]
  (match
   (vec more)
   [(min-list :guard sequential?)] (min-partial min-list)

   [min (vars :guard sequential?)]
   (constraint constraint-name
               [min
                ['of (vec vars)]] compiler)

   [set-var min (not-empty? :guard boolean?)]
   (constraint constraint-name
               [min
                ['of set-var]
                ['not-empty? not-empty?]]
               compiler)

   [set-indices,
    (weights :guard [sequential? (p every? int?)])
    (offset :guard integer?)
    min,
    (not-empty? :guard boolean?)]
   (constraint constraint-name
               [min
                ['of          (vec weights)]
                ['indices    set-indices]
                ['offset      offset]
                ['not-empty? not-empty?]]
               compiler)

   [& int-vars] (min-partial int-vars)))
