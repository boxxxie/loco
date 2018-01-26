(ns loco.constraints.utils
  (:use [loco.utils])
  (:require [clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(defn ^:dynamic preserve-consts [val]
  (match [val (meta val)]
         [_ {:preserve-const true}] val
         [_ {:preserve-consts true}] val
         [(val :guard number?) _] (with-meta [val] {:preserve-const true})
         [(val :guard vector?) _] (with-meta val {:preserve-consts true})
         :else val))

(defn- compiler [compiler]
  {:compiler compiler})

(defn with-compiler [obj compiler]
  (with-meta obj {:compiler compiler}))

(defn constraint
  ([name input compiler]
   {:pre [(vector? input)]}
   (-> [:constraint [name input]]
       (with-meta {:compiler compiler})))
  ([name input]
   {:pre [(vector? input)]}
   [:constraint [name input]])
  ([input]
   {:pre [(vector? input)]}
   [:constraint input]))

(defn partial-constraint [input]
  {:pre [(vector? input)]}
  [:constraint :partial input])


(def comparison-operator? #{'= '> '< '!=  '>= '<=
                            := :> :< :!= :not=  :>= :<=
                            =  >  <  not= >=  <=})

(def comparison-symbol? (->> comparison-operator?
                             (filter symbol?)
                             (into #{})))

(def arithmetic-operator? #{'+ '* '/ '-
                            :+ :* :/ :-
                            +  *  /  -})

(def arithmetic-symbol? (->> arithmetic-operator?
                             (filter symbol?)
                             (into #{})))

(def op-map
  {
   + '+
   - '-
   * '*
   / '/
   = '=
   not= '!=
   > '>
   < '<
   >= '>=
   <= '<=
   :+ '+
   :- '-
   :* '*
   :/ '/
   := '=
   :not= '!=
   :!= '!=
   :> '>
   :< '<
   :>= '>=
   :<= '<=
   })

(defn to-operator [op]
  (get op-map op
       ;;TODO: remove this after converting all constraints to spec
       ((clojure.set/union arithmetic-operator? comparison-operator?) op)))

(def int-var?  (p instance? IntVar))
(def int-or-intvar? #(or (int? %) (int-var? %)))
(def bool-var? (p instance? BoolVar))
(def set-var?  (p instance? SetVar))
;;(def int-or-bool? #(or (bool-var? %) (int-var? %)))



(s/def ::int-vars (s/coll-of int-var?))
(s/def ::bool-vars (s/coll-of bool-var?))
;;(s/def ::mixed-ints-bools (s/coll-of int-or-bool?))

(s/def ::list-type
  (s/or
   :bools ::bool-vars
   :ints  ::int-vars))

(defn convert-vars-to-strings
  "turn var objects into strings for easier reading/debugging"
  [obj]
  (->> obj
       (walk/prewalk
        #(if (every? false?
                     ((juxt int-var? bool-var? set-var?) %))
           %
           (str %)))))

(defn report-spec-error [constraint-name, spec-name, statement]
  (throw (ex-info
          (str "There is an error in the input to constraint [" constraint-name "]"
               "\n"
               (->> statement (s/explain-str spec-name))
               "\n"
               (s/describe spec-name))
          (->> statement
               (s/explain-data spec-name)
               convert-vars-to-strings))))
