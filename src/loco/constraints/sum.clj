(ns loco.constraints.sum
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer [constraint partial-constraint with-compiler] :as utils]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name :sum)

(s/def ::compile-spec
  (s/cat :constraint #{:con/sum}
         :args (s/spec
                (s/or
                 :set   (s/cat :eq-var utils/int-var?
                               :op #{:op/=}
                               :var utils/set-var?)
                 :bools (s/cat :eq-var utils/int-var?
                               :op utils/qualified-comparison-operator?
                               :vars (s/spec ::utils/bool-vars))
                 :ints  (s/cat :eq-var utils/int-var?
                               :op utils/qualified-comparison-operator?
                               :vars (s/spec ::utils/int-vars))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:constraint :con/sum,:args [:ints {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array IntVar vars) (name op) eq-var)

           {:constraint :con/sum,:args [:bools {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array BoolVar vars) (name op) eq-var)

           {:constraint :con/sum,:args [:set {:eq-var eq-var :op :op/=, :var set-var}]}
           (.sum model set-var eq-var)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn sum
  "Creates a sum constraint. Enforces that ∑i in |vars|varsi operator sum
  Creates a constraint summing elements of set sum{i | i in set} = sum"
  {:choco ["sum(BoolVar[] vars, String operator, IntVar sum)"
           "sum(IntVar[]  vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]
   :partial true}
  ([vars]
   {:pre [(sequential? vars)]}
   ;; this is named differently because it creates nice var
   ;; names. gets converted into a :sum at compile step
   (partial-constraint [:+ vars]))

  ([summation set-var]
   (let [op (:= utils/qualified-operator-map)]
     (-> (constraint [:con/sum [summation op set-var]])
         (with-compiler compiler))))

  ([summation operator vars]
   {:pre [(sequential? vars)
          (utils/comparison-operator? operator)]}
   (let [op (operator utils/qualified-operator-map)]
     (-> (constraint [:con/sum [summation op vars]])
         (with-compiler compiler)))))
