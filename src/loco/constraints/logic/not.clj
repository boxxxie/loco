(ns loco.constraints.logic.not
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   org.chocosolver.solver.constraints.Constraint
   ))

(def ^:private constraint-name 'not)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         ;;FIXME: change this
         :args (s/spec
                constraint?)))

;;TODO: in compiler.clj we handle logical constraints (like this) with a recursive function
;; example:
;; [:and (constraints :guard (p every? constraint?))]
;; (.and model (realize-nested-constraints constraints))
(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq _= dividend _ divisor]}
           (.not model constraint)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  {:choco "not(Constraint cstr)"}
  [constraint]
  (constraint constraint-name constraint compiler))
