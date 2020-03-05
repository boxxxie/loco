(ns loco.constraints.vars
  (:require
   [loco.constraints.utils :refer :all]
   [meander.epsilon :as m :refer [match]]
   )
  (:import org.chocosolver.solver.variables.IntVar))

;;TODO: implement real vars
;;realVar, realVar, realVar, realVar, realVar,
;;realVarArray, realVarArray, realVarMatrix, realVarMatrix,

;; -------------------- Utils --------------------

(def ^:private valid-variable-name? (some-fn vector? string? keyword?))

(defn- hidden-name? [keyword-name]
  (when (keyword? keyword-name)
    (.startsWith (name keyword-name) "_")))

(defn- make-hidden [var]
  (-> var
      (assoc ,, 2 :hidden)
      (vary-meta ,, assoc :hidden true)))

(defn- hidden-conversion
  "converts a var, based on it's name, to be hidden. this is for backwards compatibility
  to be converted, the name should have an underscore at the beginning

  e.g. [:var :_var-name :public [:int 0 2]]
  e.g. [:var [:_var-name 1] :public [:int 0 2]]"
  [var]
  (match var
    [_  [(m/pred hidden-name? ?var-name) & _] & _]   (make-hidden var)
    [_  (m/pred hidden-name? ?var-name) & _]         (make-hidden var)
    _  var))

;; -------------------- Tuples --------------------

(defn $tuples
  "tuples act like a mask to be used with the table constraint.
  tuples can define the explicit arbitrary values a collection of
  variables can assume.
  tuples can define the explicit arbitrary
  values a collection of variables can not assume.

  e.g. ($tuples [[1][2][3][4][5][6][7][8][9][0]])
  a mask for 1 variable that enforces it must be one of 0-9.
  when used with $table is equivalent to ($int 0 9)

  e.g. ($tuples [[1][2][3][4][5][6][7][8][9][0]] false)
  a mask for 1 variable that enforces it must not be one of 0-9.
  when used with $table is equivalent to ($not ($int 0 9))

  useful when you know the permutations the variables can take on
  ahead of time, or require optimizations, or when you don't want to
  declare the domain of your vars ahead of time, and use a table
  instead.

  Choco:
  Create a list of tuples which represents all allowed tuples if
  feasible=true or a set of forbidden tuples if feasible=false."
  {:choco "Tuples(int[][] values, boolean feasible)"}
  ([var-name ints-lists] ($tuples var-name ints-lists :allowed))
  ([var-name ints-lists feasible?]
   {:pre [(valid-variable-name? var-name)
          (sequential? ints-lists)
          (every? (partial every? int?) ints-lists)
          (apply = (map count ints-lists))
          (some? (#{:forbidden :allowed true false} feasible?))
          (or (vector? var-name)
              (keyword? var-name)) ;;not supporting crazy var names yet
          ]}
   ;;TODO: maybe tuples should have a different structure than a typical var
   ^:var ^:tuples ^:hidden
   [:var var-name :hidden [:tuples (get {true :allowed false :forbidden} feasible? feasible?)
                           (mapv vec ints-lists)]]
   ))

(defn $tuples-forbidden [var-name ints-lists] ($tuples var-name ints-lists :forbidden))
(defn $tuples-allowed   [var-name ints-lists] ($tuples var-name ints-lists :allowed))
(alter-meta! (var $tuples-forbidden) merge (dissoc (meta (var $tuples)) :name))
(alter-meta! (var $tuples-allowed)   merge (dissoc (meta (var $tuples)) :name))

;; -------------------- Sets --------------------

;;TODO: setVarArray, setVarArray, setVarMatrix, setVarMatrix,

;; yet to be implemented
;; default SetVar[] setVarArray(int size, int[] lb, int[] ub)
;; Creates an array of size set variables, taking their domain in [lb, ub]
;; default SetVar[] setVarArray(String name, int size, int[] lb, int[] ub)
;; Creates an array of size set variables, taking their domain in [lb, ub]
;; default SetVar[][] setVarMatrix(int dim1, int dim2, int[] lb, int[] ub)
;; Creates a matrix of dim1*dim2 set variables, taking their domain in [lb, ub]
;; default SetVar[][] setVarMatrix(String name, int dim1, int dim2, int[] lb, int[] ub)
;; Creates a matrix of dim1*dim2 set variables, taking their domain in [lb, ub]

(defn- upper-bound-contains-lower-bound?
  "when a set is instantiated, it will error if the upper bound does not contain the lower bound"
  [lb ub]
  (every? (set ub) (set lb)))

(defn $set
  "Creates a set variable taking its domain in [lb, ub], For
   instance [#{0,3}, #{-2,0,2,3}] means the variable must include both
   0 and 3 and can additionnaly include -2, and 2

  if only a single int set is given, then Creates a constant set variable"
  {:choco ["setVar(String name, int[] lb, int[] ub)"
           "setVar(String name, int... value)"]}
  ([var-name lb ub]
   ;;TODO: possible that lb should be subset of ub
   {:pre [(valid-variable-name? var-name)
          (or (set? lb) (sequential? lb))
          (or (set? ub) (sequential? ub))
          ;;all elements from lb must be in ub
          (upper-bound-contains-lower-bound? lb ub)
          (every? int? lb)
          (every? int? ub)]}

   ;;^{:domain {:lb (into (sorted-set) lb) :ub (into (sorted-set) ub)}}
   (->
    ^:var ^:set
    [:var var-name :public [:set (into (sorted-set) lb) (into (sorted-set) ub)]]
    hidden-conversion))

  ([var-name ints]
   {:pre [(or (set? ints) (sequential? ints))
          (every? int? ints)]}

   ;;^{:domain {:lb #{} :ub (into (sorted-set) ints)}}
   (->
    ^:var ^:set
    [:var var-name :public [:set (into (sorted-set) ints)]]
    hidden-conversion)))

(defn $set- [& more]
  (->
   (apply $set more)
   make-hidden))

(alter-meta! (var $set-) merge (dissoc (meta (var $set)) :name))

;; -------------------- Constants --------------------

;;TODO: deprecate $const ... (;_;)
(defn $const
  "Declares that a variable must be a specific value (integer)"
  [var-name value]
  {:pre [(integer? value)
         (valid-variable-name? var-name)]}
  (-> ^:var ^:int ^:const ^{:domain {:lb (int value) :ub (int value)}}
      [:var var-name :public [:const value]]
      hidden-conversion))

(defn $const- [& more] (-> (apply $const more) make-hidden))
(alter-meta! (var $const-) merge (dissoc (meta (var $const)) :name))

;; -------------------- Booleans --------------------

;;TODO: implement const bool?  	boolVar(String name, boolean value)
;;TODO: boolVarArray, boolVarArray, boolVarMatrix, boolVarMatrix,

(defn $bool
  "Declares that a variable must be a boolean (true/false or [0 1])
  some constraints have optimizations for booleans/boolean-lists (e.g. Model.sum|and|or)"
  {:choco "boolVar(String name)"}
  [var-name]
  {:pre [(valid-variable-name? var-name)]}
  (->>  ^:var ^:bool ^{:domain {:lb 0 :ub 1}}
        [:var var-name :public [:bool 0 1]]
        hidden-conversion))

(defn $bool- [& more] (-> (apply $bool more) make-hidden))
(alter-meta! (var $bool-) merge (dissoc (meta (var $bool)) :name))

(defn $bools
  "Declares a list of booleans with given var-names.
  this is not a equivalent to a BoolVar[]"
  [& var-names]
  (->
   (mapv $bool var-names)
   (with-meta {:generated-vars true})))

;; -------------------- Integers --------------------
;;TODO: change name to something else $?
;;can use (load file) to import public functions, not sure if it is good solution

;;TODO: intVarArray, intVarArray, intVarArray, intVarArray,
;;intVarArray, intVarArray, intVarMatrix, intVarMatrix, intVarMatrix,
;;intVarMatrix, intVarMatrix, intVarMatrix

(defn $int
  "Declares that a variable must be in a certain domain.
   Possible arglist examples:
   (in :x)   => (in :x IntVar/MIN_INT_BOUND IntVar/MAX_INT_BOUND)
   (in :x 1) => const
   (in :x 1 5)
   (in :x [1 2 3 4 5])
   (in :x 1 5 :bounded)"
  {:choco ["intVar(String name, int value)"
           "intVar(String name, int[] values)"
           "intVar(String name, int lb, int ub)"
           "intVar(String name, int lb, int ub, boolean boundedDomain)"]}
  ([var-name]
   ($int var-name IntVar/MIN_INT_BOUND IntVar/MAX_INT_BOUND))

  ([var-name lb ub bounded?]
   {:pre [(valid-variable-name? var-name)
          (< lb ub)
          (int? lb)
          (int? ub)
          (some? (#{true false :bounded} bounded?))]}
   (->>
    (if bounded?
      ^:var ^:int ^{:domain {:int true :bounded true :ub (int ub) :lb (int lb)}}
      [:var var-name :public [:int lb ub :bounded]]
      ($int var-name lb ub))
    hidden-conversion))

  ([var-name lb ub]
   {:pre [(valid-variable-name? var-name) (int? lb) (int? ub)]}
   (->>
    (match (vec (sort [(int lb) (int ub)]))
      [0 1] ($bool var-name)
      _ ^:var ^:int ^{:domain {:int true :ub ub :lb lb}}
      [:var var-name :public [:int lb ub]])
    hidden-conversion))

  ([var-name values]
   {:pre [(valid-variable-name? var-name)
          (or
           (int? values)
           (and
            (sequential? values)
            (every? int? values)))]}

   (->> (if (int? values)
          ^:var ^:int ^{:domain {:int true
                                 :const true
                                 :ub (int values)
                                 :lb (int values)}}
          [:var var-name :public [:int values values]]
          (match (vec (dedupe (sort (flatten [values]))))
            [0 1] ($bool var-name)
            ?domain-list
            ^:var ^:int ^{:domain {:int true
                                   :ub (int (peek ?domain-list))
                                   :lb (int (first ?domain-list))
                                   :enumeration ?domain-list}}
            [:var var-name :public [:int ?domain-list]]))
        hidden-conversion)))

(defn $int- [& more] (-> (apply $int more) make-hidden))

;; -------------------- Tasks -------------------
;;TODO: taskVarArray,taskVarMatrix,

;;TODO: really need to make this more robust
(defn $task
  "Container representing a task: It ensures that: start + duration = end"
  {:choco "Task(IntVar s, IntVar d, IntVar e)"}
  ([task-name _start start _duration duration _end end]
   {:pre [(not-any? nil? [start duration end])]}
   (let [{:keys [start duration end]} {_start start _duration duration _end end}]
     ($task task-name start duration end)))
  ([var-name start duration end]
   {:pre [(valid-variable-name? var-name)]}
   (->
    ^:var ^:task
    [:var var-name :public [:task start duration end]]
    hidden-conversion)))

(defn $task- [& more] (-> (apply $task more) make-hidden))
(alter-meta! (var $task-) merge (dissoc (meta (var $task)) :name))

;;-------------------- meta --------------------
(defn- upgrade-proto [statement {:keys [int lb ub] :as domain}]
  {:pre [(true? int) (int? lb) (int? ub)]}
  (-> statement
      (conj [:int lb ub])
      (vary-meta assoc :domain domain)))

(defn $proto
  "this is not meant to be used like $bool or $int, more for internal usage. var-name must be a string"
  [var-name partial]
  {:pre [(string? var-name)]}
  (-> ^{:from partial :upgrade-fn upgrade-proto} ^:proto ^:var [:var var-name :proto]
      hidden-conversion))
