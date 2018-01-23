;;TODO: implement below constraint factory methods
;; -------------------- AUTOMATA --------------------
;; costRegular(IntVar[] vars, IntVar cost, ICostAutomaton costAutomaton)
;; multiCostRegular(IntVar[] vars, IntVar[] costVars, ICostAutomaton costAutomaton)
;; -------------------- MDD --------------------
;; mddc(IntVar[] vars, MultivaluedDecisionDiagram MDD)
;; requires building a complex object of ints and tuples
;;http://www.choco-solver.org/apidocs/org/chocosolver/util/objects/graphs/MultivaluedDecisionDiagram.html

(ns loco.constraints
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            [loco.match :refer [match+]]
            loco.automata
            loco.constraints.arithmetic
            loco.constraints.set
            loco.vars)
  (:import org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

(defn- inherit-def [prefix sym var-to-inherit]
  (let [sym (symbol (str prefix (name sym)))]
    (println "creating def" (str *ns* "/" (name sym)))
    (->
     (intern *ns* sym var-to-inherit)
     (reset-meta! (meta var-to-inherit)))))

(def ^:private to-inherit
  (->> [
        'loco.vars
        'loco.constraints.arithmetic
        'loco.constraints.set
        'loco.constraints.sum]
       (map ns-publics)
       (into {})))

(doseq [[sym var] to-inherit]
  (inherit-def "$" sym var))


;;;;; LOGIC

(defn $true
  "Always true."
  []
  [:constraint :true])

(defn $false
  "Always false."
  []
  [:constraint :false])

;;TODO: there is also a boolean list form that can be useful to implement
;;needs to be done at the compile phase, not here
(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if
  and only if every subconstraint is true."
  {:choco ["and(BoolVar... bools)"
           "and(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(sequential? constraints-or-bools) (not (empty? constraints-or-bools))]}
  [:constraint [:and (vec constraints-or-bools)]])

;;TODO: there is also a boolean list form that can be useful to implement
(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and
  only if at least one subconstraint is true."
  {:choco ["or(BoolVar... bools)"
           "or(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(sequential? constraints-or-bools) (not (empty? constraints-or-bools))]}
  [:constraint [:or (vec constraints-or-bools)]])

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  {:choco "not(Constraint cstr)"}
  [constraint]
  [:constraint [:not constraint]])

(defn $when
  [if-this then-this]
  [:constraint [:when [if-this then-this]]])

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole
  statement is false).  An optional \"else\" field can be specified,
  which must be true if P is false."
  [if-this then-this else-this]
  [:constraint [:if-else [if-this then-this else-this]]])

(defn $iff
  "Posts an equivalence constraint stating that cstr1 is satisfied <=>
  cstr2 is satisfied, BEWARE : it is automatically posted (it cannot
  be reified)"
  [if-this then-this]
  [:constraint [:iff [if-this then-this]]])

(defn $reify
  "Given a constraint C, will generate a bool-var V such that (V = 1) iff C."
  {:choco "reification(BoolVar var, Constraint cstr)"}
  [var-label, constraint]
  (-> [($bool- var-label)
       [:reify var-label constraint]]
      (with-meta {:generated-vars true})))

(defn ^:dynamic *cond-name-gen*
  "useful to change bindings for tests and if you want to use the
  internal bools generated by $cond"
  [prefix] (gensym prefix))

;;this is really complicated... very skeptical of it's use, or even correctly working
(defn $cond
  "A convenience function for constructing a \"cond\"-like statement out of $if/$reify statements.
  The final \"else\" can be specified using the :else keyword.

  the constraints try to behave like a clojure cond would, making sure
  that order is enforced, so each statement requires that all of the
  previous clauses are false.

  uses *cond-name-gen* dynamic function to generate names for the reify variables

  roughly translates to the below code:
  partition clauses by 2
  for each pair, produce:
  --------------------------
  ($reify :if_cond_1 ($false))
  ($reify :if_1 ($and :if_cond_1 ... :not_if_bool_0 ... :not_if_bool_n))
  ($reify :not_if_1 ($not ($false)))

  ($if :if_1
       ($true)
       ($and :not_if_1))
  ---------------------------
  "
  [& clauses]
  {:pre [(even? (count clauses))]}
  (let [global-name (*cond-name-gen* "_")
        last-index (dec (count (partition 2 clauses)))]
    (->>
     (partition 2 clauses)
     (map-indexed vector)
     (reduce
      (fn [acc [idx [clause action]]]
        (let [[previous-not-clauses statements] acc
              if-cond-bool (keyword (str global-name '_if_cond_ idx))
              if-bool (keyword (str global-name '_if_ idx))
              not-if-bool (keyword (str global-name '_not_if_ idx))

              new-statements
              (if (= clause :else)
                [($when (apply $and previous-not-clauses)
                         action)]
                [
                 ($reify if-cond-bool clause)
                 ($reify if-bool
                         (apply $and if-cond-bool previous-not-clauses))
                 ($reify not-if-bool ($not clause)) ;;option to be boolNotView
                 ($if if-bool
                      action
                      ($and not-if-bool))
                 ])
              ]
          [(conj previous-not-clauses not-if-bool), (into statements new-statements)]))
      [[] []])
     second)))

;;TODO: organize functions better
;;;;; GLOBAL

(defn $distinct
  "Given a bunch of int-vars, ensures that all of them have different
  values, i.e. no two of them are equal.

  Creates a constraint stating that sets should all be different (not
  necessarily disjoint) Note that there cannot be more than one empty
  set."
  {:choco ["allDifferent(IntVar... vars)"
           "allDifferent(SetVar... sets)"
           "allDifferent(IntVar[] vars, String CONSISTENCY)"]}
  [& vars]
  (match+ (vec vars)
          [int-vars, consistency]
          :guard [int-vars sequential?, consistency #{:default :bc :ac}]
          [:constraint [:distinct (vec int-vars) [:consistency consistency]]]

          [var-list :guard sequential?]
          [:constraint [:distinct (vec var-list)]]

          [& var-list]
          [:constraint [:distinct (vec var-list)]]))

(def $all-different $distinct)
(reset-meta! (var $all-different) (meta (var $distinct)))

(defn $distinct-except-0
  "Creates an allDifferent constraint for variables that are not equal
  to 0. There can be multiple variables equal to 0."
  {:choco "allDifferentExcept0(IntVar[] vars)"}
  [vars]
  {:pre [(sequential? vars)]}
  [:constraint [:distinct-except-0 (vec vars)]])

(def $all-different-except-0 $distinct-except-0)
(reset-meta! (var $all-different-except-0) (meta (var $distinct-except-0)))

;;TODO: implement allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)
;;TODO: figure out how to convert a function into a Condition object
;;http://www.choco-solver.org/apidocs/org/chocosolver/solver/constraints/nary/alldifferent/conditions/Condition.html
;;possibly need to use reify
#_(defn $distinct-under-condidiont
  "Creates an allDifferent constraint subject to the given
  condition. More precisely: IF singleCondition for all X,Y in vars,
  condition(X) => X != Y ELSE for all X,Y in vars, condition(X) AND
  condition(Y) => X != Y"
  {:choco "allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)"}
  [int-vars condition single-condition?]
  {:pre [(sequential? int-vars)
         (boolean? single-condition?)
         (fn? condition)]}
  )

(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default
  0), the elements of L define a circuit, where (L[i] = j + offset)
  means that j is the successor of i.  Hint: make the offset 1 when
  using a 1-based list.


  Filtering algorithms: (circuit-conf :all, :first, :light, :rd)
  - subtour elimination : Caseau & Laburthe (ICLP'97)
  - allDifferent GAC algorithm: Régin (AAAI'94)
  - dominator-based filtering: Fages & Lorca (CP'11)
  - Strongly Connected Components based filtering (Cambazard & Bourreau JFPC'06 and Fages and Lorca TechReport'12)

  See Fages PhD Thesis (2014) for more information"
  {:choco ["circuit(IntVar[] vars)"
           "circuit(IntVar[] vars, int offset)"
           "circuit(IntVar[] vars, int offset, CircuitConf conf)"]}
  ([vars]
    ($circuit vars 0))
  ([vars offset]
   {:pre [(integer? offset) (sequential? vars)]}
   [:constraint [:circuit [(vec vars) [:offset (preserve-consts offset)]]]])
  ([vars offset circuit-conf]
   {:pre [(integer? offset) (sequential? vars) (#{:all :first :light :rd} circuit-conf)]}
   [:constraint [:circuit [(vec vars)
                           [:offset (preserve-consts offset)]
                           [:conf circuit-conf]]]]))

(defn $nth
  "partial for $element"
  {:choco "element(IntVar value, IntVar[] table, IntVar index, int offset)"}
  ([vars index]
   ($nth vars index 0))

  ([vars index offset]
   {:pre [(integer? offset) (sequential? vars)]}
   (let [table (if (every? integer? vars)
                 (preserve-consts (vec vars))
                 vars)]

     [:constraint :partial [:$nth [table
                                   [:at index]
                                   [:offset (preserve-consts offset)]]]])))

(defn $element
  "Given a list of int-vars L, an int-var i, and an optional offset
  number (default 0), returns a new int-var constrained to equal L[i],
  or L[i - offset].

  value - an integer variable taking its value in table
  var-list - an array of integer values or variables
  index - an integer variable representing the value of value in table
  offset - offset matching index.lb and table[0] (Generally 0)

  Creates a constraint enabling to retrieve an element set in sets: sets[index-offset] = set
  Creates a constraint enabling to retrieve an element set in sets: sets[index] = set"
  {:choco ["element(IntVar value, int[] table, IntVar index, int offset)"
           "element(IntVar value, IntVar[] table, IntVar index, int offset)"
           "element(IntVar index, SetVar[] sets, SetVar set)"
           "element(IntVar index, SetVar[] sets, int offset, SetVar set)"]}
  ([value var-list index]
   ($element value var-list index 0))

  ([value var-list index offset]
   {:pre [(integer? offset) (sequential? var-list)]}
   (let [table (if (every? integer? var-list)
                 (preserve-consts var-list)
                 var-list)]
     [:constraint [:element [value
                             [:in table]
                             [:at index]
                             [:offset (preserve-consts offset)]]]])))

(def $elem $element)
(reset-meta! (var $elem) (meta (var $element)))

;;TODO: figure this out when we get to automata solution tests
(defn $regular
  "Takes a Choco automaton object constructed by the loco.automata
  namespace, and constrains that a list of variables represents an
  input string accepted by the automaton."
  {:choco "regular(IntVar[] vars, IAutomaton automaton)"}
  [^FiniteAutomaton automaton vars]
  {:pre [(sequential? vars)]}
  [:constraint [:regular [(vec vars) [:automation automaton]]]])

(defn $cardinality
  "Takes a list of variables, and a frequency map (from numbers to
  frequencies), constrains that the frequency map is accurate. If
  the :closed flag is set to true, any keys that aren't in the
  frequency map can't appear at all in the list of variables.

  Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed true)
  => {:a 1, :b 1, :c 2, :d 2, :e 2 :ones 2, :twos 3}"
  {:choco "globalCardinality(IntVar[] vars, int[] values, IntVar[] occurrences, boolean closed)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html"}
  ([variables frequencies]
   ($cardinality variables frequencies false))

  ([variables frequencies closed?]
   {:pre [
          (map? frequencies)
          (sequential? variables)
          (or (= closed? :closed) (boolean? closed?))
          (every? integer? (keys frequencies))
          (distinct? (keys frequencies))
          (distinct? (vals frequencies))
          ]
    }
   (let [closed (case closed?
                  :closed true
                  closed?)
         values (preserve-consts (vec (keys frequencies)))
         occurences (vec (vals frequencies))]
     [:constraint
      [:cardinality
       [(vec variables) [values occurences] [:closed closed]]]])))

(defn $knapsack
  "Creates a knapsack constraint. Ensures that :
  - occurrences[i] * weight[i] = weightSum
  - occurrences[i] * energy[i] = energySum
  and maximizing the value of energySum.

  Example: ($knapsack [3 1 2]    ; weight of item
                      [5 6 7]    ; energy of item
                      [:x :y :z] ; occurrences
                      :W         ; total weight
                      :V)        ; total energy"
  {:choco "knapsack(IntVar[] occurrences, IntVar weightSum, IntVar energySum, int[] weight, int[] energy)"}
  [weight energy occurrences weight-sum energy-sum]
  {:pre [
         (every? integer? weight)
         (every? integer? energy)
         (sequential? occurrences)
         (every? (p <= 0) weight)
         (every? (p <= 0) energy)
         ]}
  [:constraint
   [:knapsack
    [
     [:weight (preserve-consts (vec weight))]
     [:energy (preserve-consts (vec energy))]
     [:occurrences (vec occurrences)]
     [:weight-sum weight-sum]
     [:energy-sum energy-sum]
     ]]])

(defn $member
  "Creates a member constraint. Ensures var takes its values in [LB, UB]
   Creates a member constraint. Ensures var takes its values in table
   Creates a member constraint stating that the constant cst is in set
   Creates a member constraint stating that the value of intVar is in set
   Creates a member constraint stating that set belongs to sets"
  {:choco ["member(IntVar var, int[] table)"
           "member(IntVar var, int lb, int ub)"
           "member(int cst, SetVar set)"
           "member(IntVar intVar, SetVar set)"
           "member(SetVar[] sets, SetVar set)"
]}
  ([member-of collection]
   (match [member-of collection]
          [int-var (table :guard sequential?)]
          [:constraint [:member [int-var :of (preserve-consts (vec table))]]]

          [int-var (set :guard keyword?)]
          [:constraint [:member [int-var :of set]]]))

  ([var lb ub]
   {:pre [(integer? lb) (integer? ub) (< lb ub)]}
   [:constraint [:member [var
                          [:lower-bound (preserve-consts lb)]
                          [:upper-bound (preserve-consts ub)]]]]))

(defn $not-member
  "Creates a member constraint. Ensures var does not take its values in [LB, UB]
   Creates a member constraint. Ensures var does not take its values in table
   Creates a member constraint stating that the constant cst is not in set
   Creates a member constraint stating that the value of intVar is not in set"
  {:choco ["notMember(IntVar var, int[] table)"
           "notMember(IntVar var, int lb, int ub)"
           "notMember(int cst, SetVar set)"
           "notMember(IntVar var, SetVar set)"]}
  ([var-or-cst ints-or-set]
   (match [var-or-cst ints-or-set]
          [int-var (table :guard #(and (sequential? %) (every? integer? %)))]
          [:constraint [:not-member [int-var :of (preserve-consts (vec table))]]]

          [int-var (set :guard keyword?)]
          [:constraint [:not-member [int-var :of set]]]
          ))

  ([var lb ub]
   {:pre [(integer? lb) (integer? ub) (< lb ub)]}
   [:constraint [:not-member [var
                              [:lower-bound (preserve-consts lb)]
                              [:upper-bound (preserve-consts ub)]]]]))

(defn $n-values
  "Creates an nValue constraint. Let N be the number of distinct values
  assigned to the variables of the vars collection. Enforce condition
  N = nValues to hold."
  {:choco "nValues(IntVar[] vars, IntVar nValues)"}
  [vars n-values]
  {:pre [(sequential? vars)]}
  [:constraint [:n-values [(vec vars) n-values]]])

(defn $sort
  "Creates a sort constraint which ensures that the variables of
  sorted-vars correspond to the variables of vars according to a
  permutation. The variables of sortedVars are also sorted in increasing
  order.

  For example:
  - X= (4,2,1,3)
  - Y= (1,2,3,4)"
  {:choco "sort(IntVar[] vars, IntVar[] sortedVars)"}
  [vars sorted-vars]
  {:pre [(sequential? vars) (sequential? sorted-vars)]}
  [:constraint [:sort [(vec vars) (vec sorted-vars)]]])

(defn $count
  "Creates a count constraint. Let N be the number of variables of the
  vars collection assigned to value value; Enforce condition N = limit
  to hold. "
  {:choco ["count(int value, IntVar[] vars, IntVar limit) "
           "count(IntVar value, IntVar[] vars, IntVar limit)"]}
  [value vars limit]
  {:pre [(sequential? vars)]}
  [:constraint [:count [(vec vars) [:value (preserve-consts value)] [:limit limit]]]])

(defn $among
  "Creates an among constraint. nbVar is the number of variables of the
  collection vars that take their value in values."
  {:choco "among(IntVar nbVar, IntVar[] vars, int[] values)"
   :gccat "http://www.emn.fr/x-info/sdemasse/gccat/Camong.html"}
  [nb-var vars values]
  {:pre [(sequential? vars) (sequential? values) (every? integer? values)]}
  [:constraint [:among [(vec vars) [:nb-var nb-var] [:values (preserve-consts (vec values))]]]])

(defn $at-least-n-values
  "Creates an atLeastNValue constraint. Let N be the number of distinct
  values assigned to the variables of the vars collection. Enforce
  condition N >= nValues to hold."
  {:choco "atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)"}
  ([vars n-values] ($at-least-n-values vars n-values false))
  ([vars n-values ac]
   {:pre [(sequential? vars) (boolean? ac)]}
   [:constraint [:at-least-n-values [(vec vars) [:n-values n-values] [:ac ac]]]]))

(defn $at-most-n-values
  "Creates an atMostNValue constraint. Let N be the number of distinct
  values assigned to the variables of the vars collection. Enforce
  condition N <= nValues to hold."
  {:choco "atMostNValues(IntVar[] vars, IntVar nValues, boolean STRONG)"}
  ([vars n-values] ($at-most-n-values vars n-values false))
  ([vars n-values strong]
   {:pre [(sequential? vars) (boolean? strong)]}
   [:constraint [:at-most-n-values [(vec vars) [:n-values n-values] [:strong strong]]]]))

(defn $bin-packing
  "Creates a BinPacking constraint. Bin Packing formulation:
  forall b in [0, binLoad.length - 1],
  binLoad[b] = sum(itemSize[i] |
  i in [0, itemSize.length - 1],
  itemBin[i] = b + offset forall i in [0, itemSize.length - 1],
  itemBin is in [offset, binLoad.length-1 + offset]

  Parameters:
  itemBin  - IntVar representing the bin of each item
  itemSize - int representing the size of each item
  binLoad  - IntVar representing the load of each bin (i.e. the sum of the size of the items in it)
  offset   - 0 by default but typically 1 if used within MiniZinc (which counts from 1 to n instead of from 0 to n-1)

  GCCAT:
  Given several items of the collection ITEMS (each of them
  having a specific weight), and different bins described the the
  items of collection BINS (each of them having a specific capacity
  capa), assign each item to a bin so that the total weight of the
  items in each bin does not exceed the capacity of the bin."
  {:choco "binPacking(IntVar[] itemBin, int[] itemSize, IntVar[] binLoad, int offset)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cbin_packing_capa.html"
   :constraint-type [:resource-constraint]}
  ([item-map bin-load] {:pre [(map? item-map)]}
   ($bin-packing (keys item-map) (vals item-map) bin-load))
  ([item-bin, item-size, bin-load] ($bin-packing item-bin item-size bin-load 0))
  ([item-bin, item-size, bin-load, offset]
   {:pre [(sequential? item-bin)
          (< 0 (count item-bin))
          (distinct? item-bin)
          (sequential? item-size)
          (every? integer? item-size)
          (every? pos? item-size)
          (= (count item-size) (count item-bin))
          (sequential? bin-load)
          (integer? offset)
          (<= 0 offset)]}
   [:constraint [:bin-packing
                 [:item-bin (vec item-bin)]
                 [:item-size (preserve-consts (vec item-size))]
                 [:bin-load (vec bin-load)]
                 [:offset (preserve-consts offset)]]]))

(defn $diff-n
  "Creates a diffN constraint. Constrains each rectangle[i], given by
  their origins X[i],Y[i] and sizes width[i], height[i], to be non-overlapping.

  there is a good visualization of this at:
  http://sofdem.github.io/gccat/gccat/Cdiffn.html"
  {:choco "diffN(IntVar[] X, IntVar[] Y, IntVar[] width, IntVar[] height, boolean addCumulativeReasoning)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cdiffn.html"}
  [xs ys widths heights add-cumulative-reasoning?]
  {:pre [(every? sequential? [xs ys widths heights]) (boolean? add-cumulative-reasoning?)]}
  [:constraint [:diff-n
                [:xs xs]
                [:ys ys]
                [:widths widths]
                [:heights heights]
                [:add-cumulative-reasoning add-cumulative-reasoning?]]])

(defn $bits-channeling
  "Creates an channeling constraint between an integer variable and a set of bit variables.
  Ensures that var = 20*BIT_1 + 21*BIT_2 + ... 2n-1*BIT_n.

  BIT_1 is related to the first bit of OCTET (2^0), BIT_2 is related
  to the first bit of OCTET (2^1), etc.  The upper bound of var is
  given by 2n, where n is the size of the array bits."
  {:choco "bitsIntChanneling(BoolVar[] bits, IntVar var)"}
  [bits int-var]
  {:pre [(sequential? bits)]}
  (-> []
      (into (mapv $bool bits))
      (into [[:constraint [:bit-channeling [:bool-vars (vec bits)] [:int-var int-var]]]])
      (with-meta {:generated-vars true})))

(defn $bools-int-channeling
  "Creates an channeling constraint between an integer variable and a
  set of boolean variables. Maps the boolean assignments variables
  bVars with the standard assignment variable var.
  var = i <-> bVars[i-offset] = 1"
  {:choco "boolsIntChanneling(BoolVar[] bVars, IntVar var, int offset)"}
  ([bool-vars, int-var] ($bools-int-channeling bool-vars int-var 0))
  ([bool-vars, int-var, offset]
   {:pre [(integer? offset) (sequential? bool-vars)]}
   [:constraint
    [:bools-int-channeling
     [:bool-vars (vec bool-vars)]
     [:int-var int-var]
     [:offset (preserve-consts offset)]]]))

(defn $clauses-int-channeling
  "Creates an channeling constraint between an integer variable and a
  set of clauses. Link each value from the domain of var to two
  boolean variable: one reifies the equality to the i^th value of the
  variable domain, the other reifies the less-or-equality to the i^th
  value of the variable domain.

  Contract: eVars.lenght == lVars.length == var.getUB() - var.getLB() + 1
  Contract: var is not a boolean variable"
  {:choco "clausesIntChanneling(IntVar var, BoolVar[] eVars, BoolVar[] lVars)"}
  [int-var e-vars l-vars]
  {:pre [(every? sequential? [e-vars l-vars]) (= (count e-vars) (count l-vars))]}
  [:constraint
   [:clauses-int-channeling
    [:int-var int-var]
    [:e-vars (vec e-vars)]
    [:l-vars (vec l-vars)]]])

(defn $sub-circuit
  "Creates a subCircuit constraint which ensures that
  the elements of vars define a single circuit of subcircuitSize nodes where
  vars[i] = offset+j means that j is the successor of i.
  and vars[i] = offset+i means that i is not part of the circuit
  the constraint ensures that |{vars[i] =/= offset+i}| = subCircuitLength

  Filtering algorithms:
  subtour elimination : Caseau & Laburthe (ICLP'97)
  allDifferent GAC algorithm: Régin (AAAI'94)
  dominator-based filtering: Fages & Lorca (CP'11) (adaptive scheme by default, see implementation)"
  {:choco "subCircuit(IntVar[] vars, int offset, IntVar subCircuitLength)"}
  ([int-vars sub-circuit-length] ($sub-circuit int-vars sub-circuit-length 0))
  ([int-vars sub-circuit-length offset]
   {:pre [(integer? offset) (sequential? int-vars)]}
   [:constraint
    [:sub-circuit [(vec int-vars)
                   [:sub-circuit-length sub-circuit-length]
                   [:offset (preserve-consts offset)]]]]))

(defn $int-value-precede-chain
  "Creates an intValuePrecedeChain constraint.
  Ensure that, for each pair of V[k] and V[l] of values in V,
  such that k < l, if there exists j such that X[j] = V[l], then,
  there must exist i < j such that X[i] = V[k].

  Creates an intValuePrecedeChain constraint.
  Ensure that if there exists j such that X[j] = T, then,
  there must exist i < j such that X[i] = S."
  {:choco ["intValuePrecedeChain(IntVar[] X, int[] V)"
           "intValuePrecedeChain(IntVar[] X, int S, int T)"]}
  ([xs vs]
   {:pre [(every? integer? vs) (sequential? xs)]}
   [:constraint [:int-value-precede-chain [(vec xs) (preserve-consts vs)]]])
  ([xs s t]
   {:pre [(integer? s) (integer? t) (sequential? xs)]}
   [:constraint [:int-value-precede-chain [(vec xs) (preserve-consts s) (preserve-consts t)]]])
  )

(defn $lex-less
  "Creates a lexLessEq constraint.
  Ensures that vars1 is lexicographically less or equal than vars2."
  {:choco "lexLess(IntVar[] vars1, IntVar[] vars2)"}
  [vars, lex-less-or-equal-vars]
  {:pre [(sequential? vars) (sequential? lex-less-or-equal-vars)]}
  [:constraint [:lex-less [(vec vars) :lex-of lex-less-or-equal-vars]]])

(defn $lex-less-equal
  "Creates a lexLessEq constraint.
  Ensures that vars1 is lexicographically less or equal than vars2."
  {:choco "lexLessEq(IntVar[] vars1, IntVar[] vars2)"
   ;;TODO: create more arglists as ->    :arglists '(["vars <[int-var ...]>", "lex-less-or-equal-vars <[int-var ...]>"])
   ;;:arglists '(["vars [int-var ...]", "lex-less-or-equal-vars [int-var ...]"])
   }
  [vars, lex-less-or-equal-vars]
  {:pre [(sequential? vars) (sequential? lex-less-or-equal-vars)]}
  [:constraint [:lex-less-equal [(vec vars) :lex-of lex-less-or-equal-vars]]])

;;TODO: lex-chain-less is sort? make alias if so
(defn $lex-chain-less
  "Creates a lexChainLess constraint.
  For each pair of consecutive vectors varsi and varsi+1 of the vars collection
  varsi is lexicographically strictly less than than varsi+1"
  {:choco "lexChainLess(IntVar[]... vars)"
   :arglists '([int-var-vectors] [int-var-vector...])}
  [& more]
  (match+
   (vec more)
   [int-vars-vectors]
   :guard [int-vars-vectors [sequential? (p every? sequential?)]]
   [:constraint [:lex-chain-less (mapv vec int-vars-vectors)]]

   int-vars-vectors
   [:constraint [:lex-chain-less (mapv vec int-vars-vectors)]]))

;;TODO: lex-chain-less-equal is sort?
(defn $lex-chain-less-equal
  "Creates a lexChainLessEq constraint.
  For each pair of consecutive vectors varsi and varsi+1 of the vars collection
  varsi is lexicographically less or equal than than varsi+1"
  {:choco "lexChainLessEq(IntVar[]... vars)"
   :arglists '([int-var-vectors] [int-var-vector...])}
  [& more]
  (match+
   (vec more)
   [int-vars-vectors]
   :guard [int-vars-vectors [sequential? (p every? sequential?)]]
   [:constraint [:lex-chain-less-equal (mapv vec int-vars-vectors)]]

   int-vars-vectors
   [:constraint [:lex-chain-less-equal (mapv vec int-vars-vectors)]]))

(defn $path
  "Creates a path constraint which ensures that
  the elements of vars define a covering path from start to end
  where vars[i] = j means that j is the successor of i.
  Moreover, vars[end] = |vars|
  Requires : |vars|>0

  Filtering algorithms: see circuit constraint"
  {:choco ["path(IntVar[] vars, IntVar start, IntVar end)"
           "path(IntVar[] vars, IntVar start, IntVar end, int offset)"]}
  ([vars start end] ($path vars start end 0))
  ([vars start end offset]
   {:pre [(sequential? vars) (integer? offset) (pos? (count vars))]}
   [:constraint [:path [(vec vars)
                        [:start start]
                        [:end end]
                        [:offset (preserve-consts offset)]]]]))

(defn $sub-path
  "Creates a subPath constraint which ensures that
  the elements of vars define a path of SIZE vertices, leading from start to end
  where vars[i] = offset+j means that j is the successor of i.
  where vars[i] = offset+i means that vertex i is excluded from the path.
  Moreover, vars[end-offset] = |vars|+offset
  Requires : |vars|>0

  Filtering algorithms: see subCircuit constraint"
  {:choco "subPath(IntVar[] vars, IntVar start, IntVar end, int offset, IntVar SIZE)"}
  ([vars start end size] ($sub-path vars start end size 0))
  ([vars start end size offset]
   {:pre [(sequential? vars) (integer? offset) (pos? (count vars))]}
   [:constraint
    [:sub-path [(vec vars)
                [:start start]
                [:end end]
                [:offset (preserve-consts offset)]
                [:size size]]]]))

(defn $inverse-channeling
  "Creates an inverse channeling between vars1 and vars2:
  vars1[i-offset2] = j <=> vars2[j-offset1] = i Performs AC if domains are enumerated.
  If not, then it works on bounds without guaranteeing BC
  *(enumerated domains are strongly recommended)

  Beware you should have |vars1| = |vars2|"
  {:choco ["inverseChanneling(IntVar[] vars1, IntVar[] vars2)"
           "inverseChanneling(IntVar[] vars1, IntVar[] vars2, int offset1, int offset2)"]}
  ([vars1 vars2] ($inverse-channeling vars1 0 vars2 0))
  ([vars1 offset1 vars2 offset2]
   {:pre [(= (count vars1) (count vars2))
          (every? sequential? [vars1 vars2])
          (every? integer? [offset1 offset2])]}
   [:constraint [:inverse-channeling
                 [(vec vars1) [:offset (preserve-consts offset1)]]
                 [(vec vars2) [:offset (preserve-consts offset2)]]]]))


;;TODO: key-sort implementation requires int-var[][]
#_(defn $key-sort
  "Creates a keySort constraint which ensures that the variables of
  SORTEDvars correspond to the variables of vars according to a
  permutation stored in PERMvars (optional, can be null). The variables
  of SORTEDvars are also sorted in increasing order wrt to K-size
  tuples. The sort is stable, that is, ties are broken using the
  position of the tuple in vars.


For example:
- vars= (<4,2,2>,<2,3,1>,<4,2,1><1,3,0>)
- SORTEDvars= (<1,3,0>,<2,3,1>,<4,2,2>,<4,2,1>)
- PERMvars= (2,1,3,0)
- K = 2"
  {:choco "keySort(IntVar[][] vars, IntVar[] PERMvars, IntVar[][] SORTEDvars, int K)"}
  [])

(defn $tree
  "Creates a tree constraint.
  Partition succs variables into nbTrees (anti) arborescences
  succs[i] = j means that j is the successor of i.
  and succs[i] = i means that i is a root
  dominator-based filtering: Fages & Lorca (CP'11)
  However, the filtering over nbTrees is quite light here"
  {:choco ["tree(IntVar[] succs, IntVar nbTrees)"
           "tree(IntVar[] succs, IntVar nbTrees, int offset)"]
   :gccat "http://sofdem.github.io/gccat/gccat/Ctree.html"}
  ([succs, nb-trees] ($tree succs nb-trees 0))
  ([succs, nb-trees, offset]
   {:pre [(integer? offset) (sequential? succs)]}
   [:constraint [:tree [(vec succs)
                        [:nb-trees nb-trees]
                        [:offset (preserve-consts offset)]]]]))

;; -------------------- TASK --------------------


(defn $cumulative
  "Creates a cumulative constraint: Enforces that at each point in
  time, the cumulated height of the set of tasks that overlap that
  point does not exceed a given limit. Task duration and height should
  be >= 0 Discards tasks whose duration or height is equal to zero

  Cumulative Filters:
  - DEFAULT Combines above filters as a black-box not idempotent
  - DISJUNCTIVE_TASK_INTERVAL energetic reasoning to filter disjunctive constraint Only propagated on variable subsets of size < 30 not idempotent not enough to ensure correctness (only an additional filtering)
  - HEIGHTS filters height variables only (sweep-based algorithm) idempotent (on the given set of variables only)
  - NRJ energetic reasoning to filter not idempotent not enough to ensure correctness (only an additional filtering)
  - SWEEP time-table algorithm based on a sweep line idempotent (on the given set of variables only)
  - SWEEP_HEI_SORT time-table algorithm based on a sweep line idempotent (on the given set of variables only)
  - TIME time-table algorithm based on each point in time not idempotent
  "
  {:choco
   ["cumulative(Task[] tasks, IntVar[] heights, IntVar capacity)"
    "cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental)"
    "cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental, Cumulative.Filter... filters)"]}
  ([tasks heights capacity]
   ($cumulative tasks heights capacity false))
  ([tasks heights capacity incremental?]
   ($cumulative tasks heights capacity incremental? [:default]))
  ([tasks heights capacity incremental? filters]
   {:pre
    [(sequential? filters)
     (every?
      #{:default :disjunctive-task-interval :heights :nrj :sweep :sweep-hei-sort :time} filters)
     (sequential? tasks)
     (sequential? heights)
     (boolean? incremental?)]}
   [:constraint [:cumulative [(vec tasks)
                              [:heights (vec heights)]
                              [:capacity capacity]
                              [:incremental incremental?]
                              [:filters (vec filters)]]]]))

;; -------------------- TUPLE --------------------

(defn $table
  "
  ;; table(IntVar[] vars, Tuples tuples)
  ;; table(IntVar[] vars, Tuples tuples, String algo)

  Creates a table constraint specifying that the sequence of variables
  vars must belong to the list of tuples (or must NOT belong in case
  of infeasible tuples) Default configuration with GACSTR+ algorithm
  for feasible tuples and GAC3rm otherwise

  Creates a table constraint, with the specified algorithm defined algo

  - CT+: Compact-Table algorithm (AC),
  - GAC2001: Arc Consistency version 2001 for tuples,
  - GAC2001+: Arc Consistency version 2001 for allowed tuples,
  - GAC3rm: Arc Consistency version AC3 rm for tuples,
  - GAC3rm+ (default): Arc Consistency version 3rm for allowed tuples,
  - GACSTR+: Arc Consistency version STR for allowed tuples,
  - STR2+: Arc Consistency version STR2 for allowed tuples,
  - FC: Forward Checking.
  - MDD+: uses a multi-valued decision diagram for allowed tuples (see mddc constraint),

  ----------------------------------------

  ;; table(IntVar var1, IntVar var2, Tuples tuples)
  ;; table(IntVar var1, IntVar var2, Tuples tuples, String algo)

  Creates a table constraint over a couple of variables var1 and var2:
  - AC2001: table constraint which applies the AC2001 algorithm,
  - AC3: table constraint which applies the AC3 algorithm,
  - AC3rm: table constraint which applies the AC3 rm algorithm,
  - AC3bit+rm (default): table constraint which applies the AC3 bit+rm algorithm,
  - FC: table constraint which applies forward checking algorithm."

  {:choco ["table(IntVar[] vars, Tuples tuples)"
           "table(IntVar[] vars, Tuples tuples, String algo)"
           "table(IntVar var1, IntVar var2, Tuples tuples)"
           "table(IntVar var1, IntVar var2, Tuples tuples, String algo)"
           ]
   :arglists '([int-vars tuples]
               [int-vars tuples algo]
               [int-var1 int-var2 tuples]
               [int-var1 int-var2 tuples algo])
   }
  [& more]
  (match+
   (vec more)
   [int-vars tuples] :guard [int-vars sequential?]
   [:constraint [:table [:vars (vec int-vars)] [:tuples tuples]]]

   [int-vars tuples algo]
   :guard [int-vars sequential?
           algo #{:CT+ :GAC2001 :GAC2001+ :GAC3rm :GAC3rm+ :GACSTR+ :STR2+ :FC :MDD+}]
   [:constraint [:table [:vars (vec int-vars)] [:tuples tuples] [:algo algo]]]

   [int-var1 int-var2 tuples] :guard [[int-var1 int-var2] keyword?]
   [:constraint [:table [:pair int-var1 int-var2] [:tuples tuples]]]

   [int-var1 int-var2 tuples algo]
   :guard [[int-var1 int-var2] keyword?
           algo #{:AC2001 :AC3 :AC3rm :AC3bit+rm :FC}]
   [:constraint [:table [:pair int-var1 int-var2] [:tuples tuples] [:algo algo]]]
   )
  )
