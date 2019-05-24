(ns loco.constraints.scalar
  (:use loco.constraints
        loco.utils)
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.match :refer [match+]])
  (:import
   [org.chocosolver.solver.variables
    SetVar
    IntVar
    BoolVar
    Task]))

(def ^:private constraint-name 'scalar)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       int-var?
                       comparison-symbol?
                       (s/coll-of (s/tuple int? int-var?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [result op vars-coeffs]}
           (let [coeffs (map first vars-coeffs)
                 vars (map second vars-coeffs)]
             (.scalar model
                      (into-array IntVar vars)
                      (int-array coeffs)
                      (name op)
                      result))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(declare $scalar)

(defn- constraint-fn
  "handle syntax like ($= :v ($scalar [[100 :a] [10 :b] [1 :c]]))"
  [var-name [op vars-coeffs]]
  (pp ["constraint-fn" [var-name [op vars-coeffs]]])
  (constraint constraint-name
              [var-name '=  (vec vars-coeffs)]
              compiler))

(defn- name-fn [partial]
  (match partial
         [partial-name tuples]
         (->> tuples
              (map (fn [[coef var-name]] (str coef (name var-name))))
              (interpose "+")
              (apply str (name partial-name) "_" ))))

(defn- domain-fn [partial]
  (match partial
         [partial-name vars-coeffs]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} var-coeff]
             (match var-coeff
                    ;;TODO: handle enumerated domains
                    [coeff {:int true :lb cur-lb :ub cur-ub}]
                    {:lb (+ (* coeff cur-lb) lb)
                     :ub (+ (* coeff cur-ub) ub)}))
           {:lb 0 :ub 0}
           vars-coeffs)
          (assoc :int true)
          (update :lb int)
          (update :ub int))))

(defn- scalar-partial [body]
  (partial-constraint constraint-name body
                      :name-fn name-fn
                      :constraint-fn constraint-fn
                      :domain-fn domain-fn))

(defloco $scalar
  "Creates a scalar constraint which ensures that Sum(vars[i]*coeffs[i]) operator scalar"
  {:choco "scalar(IntVar[] vars, int[] coeffs, String operator, IntVar scalar)"
   :partial true}
  ([int-var-coeff-pairs]
   {:pre [(sequential? int-var-coeff-pairs)
          (every? #(and
                    (int? (first %))
                    (= 2 (count %))) int-var-coeff-pairs)]}
   (scalar-partial (vec int-var-coeff-pairs)))

  ([int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)]}
   (scalar-partial (mapv vector coeffs int-vars)))

  ([eq operator int-var-coeff-pairs]
   {:pre [(sequential? int-var-coeff-pairs)
          ;; test that every coeff is an int
          ;; test that we are dealing with pairs
          (every? #(and
                    (int? (first %))
                    (= 2 (count %))) int-var-coeff-pairs)]}
   (constraint constraint-name
               [eq '=  (vec int-var-coeff-pairs)]
               compiler))

  ([eq operator int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)
          (= (count int-vars) (count coeffs))]}
   (constraint constraint-name
               [eq '=  (mapv vector coeffs int-vars)]
               compiler)))
