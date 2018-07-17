(ns loco.constraints.mod
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk]))

(def ^:private constraint-name 'mod)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'%} int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq-var _ operand1 _ operand2]}
           (.mod model operand1 operand2 eq-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $mod
  "Creates a modulo constraint.

  eq = operand1 % operand2

  eq         = IntVar
  operand1   = IntVar
  operand2   = IntVar"
  {:choco "mod(IntVar X, IntVar Y, IntVar Z)"
   :partial true}
  ([eq = operand1 _% operand2] ($mod eq operand1 operand2))
  ([eq operand1 operand2]
   (constraint constraint-name
               [eq '= operand1 '% operand2]
               compiler))
  ([operand1 operand2]
   (partial-constraint ['% [operand1 operand2]])))

;; -------------------- partial --------------------

(def ^:private partial-name '%)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [var-name [op [operand1 operand2]]]
  ($mod var-name = operand1 '% operand2))

;; TODO: delete this when $% tests are passing
;; (defn modulo-domains [[lb1 ub1] [lb2 ub2]]
;;   [0 ub2])

(defn domain-fn [partial [_ {ub2 :ub}]]
  (-> {:lb 0 :ub ub2}
      (assoc :int true)
      (update :lb int)
      (update :ub int)))

(defloco $%
  "partial of $mod

  e.g.:
  ($= :eq ($% :n1 :n2)) => ($mod :eq '% :n1 :n2)
  "
  {:partial true}
  ([operand1 operand2]
   (partial-constraint
    partial-name
    [operand1 operand2] ;; body
    name-fn
    constraint-fn
    domain-fn)))
