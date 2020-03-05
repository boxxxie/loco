(ns loco.constraints.lex-chain-less-equal
  (:require
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer :all :as utils]
   [clojure.spec.alpha :as s]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'lex-chain-less-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of (s/coll-of int-var?)))))

(compile-function
 (match *conformed
   {:args ?vars-vectors} (.lexChainLessEq
                          *model
                          (->> ?vars-vectors
                               (map (p into-array IntVar))
                               into-array))))

;;TODO: lex-chain-less-equal is sort?
(defn $lex-chain-less-equal
  "Creates a lexChainLessEq constraint.
  For each pair of consecutive vectors varsi and varsi+1 of the vars collection
  varsi is lexicographically less or equal than than varsi+1"
  {:choco "lexChainLessEq(IntVar[]... vars)"
   :arglists '([int-var-vectors] [int-var-vector ...])}
  [& more]
  (match (vec more)
    [(m/pred (every-pred sequential? (p every? sequential?)) ?int-vars-vectors)]
    (constraint constraint-name
                (mapv vec ?int-vars-vectors)
                compiler)

    ?int-vars-vectors
    (constraint constraint-name
                (mapv vec ?int-vars-vectors)
                compiler)))
