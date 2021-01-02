(ns loco.constraints.times
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.math.combinatorics :as combo]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints :refer [$=]]
   [loco.constraints.views.scale :refer [$scale]]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [c p split]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'times)

(s/def ::compile-spec
  (s/cat :constraint #{'times}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'*} int-var?))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?eq-var _ ?operand1 _ ?operand2]} (.times *model ?operand1 ?operand2 ?eq-var)))

(declare $*)

(defn $times
  "Creates a multiplication constraint:

  eq = operand1 * operand2

  eq         = IntVar
  operand1   = IntVar
  operand2   = IntVar"

  {:choco "times(IntVar X, IntVar Y, IntVar Z)"}
  ([eq = operand1 * operand2] ($times eq operand1 operand2))
  ([eq operand1 operand2]
   (if (some number? [eq operand1 operand2])
     ($= eq ($* operand1 operand2))
     (constraint constraint-name
                 [eq '= operand1 '* operand2]
                 compiler))))

;; -------------------- partial --------------------

(def ^:private partial-name '*)

(defn- constraint-fn [& partial]
  (let [[var-name [op operands]] partial
        [numbers vars] (split int? operands)]
    (match [vars numbers]
      [[] []]                     [nil]
      [[] ?nums]                  [(apply * ?nums)]
      [[_?only-var] []]           [nil]
      [[_?only-var] [0]]          [0]
      [[?only-var] [1]]           [?only-var]
      [[?only-var] ?nums]         [($scale ?only-var (apply * ?nums))]
      [[?operand1 ?operand2] []]  [($times var-name = ?operand1 * ?operand2)]
      )))

(defn- domain-fn [& partial]
  (let [[[partial-name domains]] partial
        [{lb1 :lb ub1 :ub} {lb2 :lb ub2 :ub}] (map domainize domains)
        possible-bounds (->>  [[lb1 ub1] [lb2 ub2]]
                              (apply combo/cartesian-product)
                              (map #(apply * %)))]
    {:int true
     :lb (apply min possible-bounds)
     :ub (apply max possible-bounds)}))

(defn $*
  "partial of $times

  allows for unlimited args (will create recursive constraint to support args)

  e.g.:
  ($= :eq ($* :n1 :n2)) => ($times :eq = :n1 * :n2)"
  {:partial true}
  [& args]
  (let [[numbers vars] (split int? args)]
    (match (vec (concat vars numbers))
      [] nil
      [?one] ?one
      [& ?more] (partial-constraint
                 partial-name [(first ?more) (apply $* (rest ?more))]
                 :constraint-fn constraint-fn
                 :domain-fn domain-fn))))
