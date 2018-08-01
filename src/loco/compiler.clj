(ns loco.compiler
  (:refer-clojure :exclude [compile ints var?])
  (:require
   [loco.model :as model]
   [loco.match :refer [match+]]
   [loco.constraints :refer []]
   [loco.utils :refer [p c view? var? constraint? reify? public-var?]]
   [clojure.core.match :refer [match]]
   [clojure.pprint :refer [pprint]])
  (:import org.chocosolver.solver.Model
           org.chocosolver.solver.variables.SetVar
           org.chocosolver.solver.variables.BoolVar
           org.chocosolver.solver.variables.IntVar
           org.chocosolver.solver.variables.Task
           org.chocosolver.solver.constraints.extension.Tuples
           org.chocosolver.solver.constraints.Constraint
           ))

(defn- lookup-var [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (if (number? name)
      name
      (throw (Exception. (str "Could not find variable: " name))))))

(defn- lookup-var-unchecked [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (when (number? name)
      name)))

;;TODO: use prewalk-replace?
(defn compile-constraint-statement [vars-index model statement]
  (let [
        lookup-var (partial lookup-var vars-index)
        lookup-var-unchecked (partial lookup-var-unchecked vars-index)
        realize-nested-constraints (fn [constraints]
                                     (->> constraints
                                          (map (p compile-constraint-statement vars-index model))
                                          (into-array Constraint)
                                          ))
        realize-nested-constraint (p compile-constraint-statement vars-index model)
        int-var? (p instance? IntVar)
        set-var? (p instance? SetVar)
        bool-var? (p instance? BoolVar)
        task-var? (p instance? Task)
        tuples-var? (p instance? Tuples)
        lookup-set-var? (c set-var? lookup-var-unchecked)
        lookup-int-var? (c int-var? lookup-var-unchecked)
        lookup-bool-var? (c bool-var? lookup-var-unchecked)
        lookup-task-var? (c task-var? lookup-var-unchecked)
        lookup-tuples-var? (c tuples-var? lookup-var-unchecked)
        all-lookup-int-vars? (p every? lookup-int-var?)
        all-lookup-set-vars? (p every? lookup-set-var?)
        all-lookup-bool-vars? (p every? lookup-bool-var?)
        all-lookup-task-vars? (p every? lookup-task-var?)
        all-int-vars? (p every? int-var?)
        all-set-vars? (p every? set-var?)
        all-bool-vars? (p every? bool-var?)
        ]

    (match
     [statement (meta statement)]
     [constraint {:compiler compiler}] (compiler model vars-index constraint)
     [[?constraint] _]
     (match+
      ?constraint


      ;; -------------------- LOGIC --------------------
      ;; handle boolean lists
      [:and (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.and model (->> bools (map lookup-var) (into-array BoolVar)))

      [:and (constraints :guard (p every? constraint?))]
      (.and model (realize-nested-constraints constraints))

      ;; handle boolean lists
      [:or (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.or model (->> bools (map lookup-var) (into-array BoolVar)))

      [:or (constraints :guard (p every? constraint?))]
      (.or model (realize-nested-constraints constraints))

      [:when [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked)) then-constraint]]
      (.ifThen model
               (lookup-var bool)
               (realize-nested-constraint then-constraint))

      [:when [if-constraint then-constraint]]
      (.ifThen model
               (realize-nested-constraint if-constraint)
               (realize-nested-constraint then-constraint))

      [:if-else [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked))
                 then-constraint else-constraint]]
      (.ifThenElse model
                   (lookup-var bool)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:if-else [if-constraint then-constraint else-constraint]]
      (.ifThenElse model
                   (realize-nested-constraint if-constraint)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:iff [if-constraint then-constraint]]
      (.ifOnlyIf model
                 (realize-nested-constraint if-constraint)
                 (realize-nested-constraint then-constraint))

      [:not constraint]
      (.not model (realize-nested-constraint constraint))

      :true
      (.trueConstraint model)

      :false
      (.falseConstraint model)

      ))))

(defn compile-reify-statement [vars-index model statement]
  (match statement
         [:reify (var-name :guard (c (p instance? BoolVar)
                                     (p lookup-var-unchecked vars-index))) constraint]
         (.reification model
                       (lookup-var vars-index var-name)
                       (compile-constraint-statement vars-index model constraint))))

(defn compile-reifies [model vars-index ast]
  (->>
   ast
   (map (partial compile-reify-statement vars-index model))
   doall))

;;this is a bit annoying, maybe ok to move it into the
;;compile-var-statement, but currently only used by Task
(defn- anon-int-var
  ([model domain]
   {:pre [(or (and (sequential? domain) (every? integer? domain))
              (integer? domain))]}
   (if (integer? domain)
     (.intVar model domain)
     (.intVar model (int-array domain))))
  ([model lb ub]
   {:pre [(integer? lb) (integer? ub)]}
   (.intVar model (min lb ub) (max lb ub)))
  ([model lb ub bounded?]
   {:pre [(integer? lb) (integer? ub) (boolean? bounded?)]}
   (.intVar model (min lb ub) (max lb ub) bounded?)))

(defn- compile-var-statement-helper
  "these should be replaced by compile functions that are assigned where the vars are created"
  {:deprecated true}
  [vars-index vars model statement]
  (let [lookup-var (partial lookup-var vars-index)]
    (match+
     [statement (meta statement)]

     [[:var var-name _ _] {:neg dep-name}]
     (.intMinusView model (lookup-var dep-name))

     [[:var var-name _ [:bool _ _]] _]
     (.boolVar model (name var-name))

     [[:var var-name _ [:int lb ub]] _] :guard [[lb ub] integer?]
     (.intVar model (name var-name) lb ub)

     [[:var var-name _ [:int lb ub :bounded]] _] :guard [[lb ub] integer?]
     (.intVar model (name var-name) lb ub true)

     [[:var var-name _ [:int enumeration]] _] :guard [enumeration vector?]
     (.intVar model (name var-name) (int-array enumeration))

     [[:var var-name _ [:const value]] _] :guard [value integer?]
     (.intVar model (name var-name) value)

     [[:var var-name _ [:set constants]] _] :guard [constants set?]
     (.setVar model (name var-name) (into-array Integer/TYPE constants))

     [[:var var-name _ [:set lb ub]] _] :guard [[lb ub] [set? (p every? integer?)]]
     (.setVar model (name var-name)
              (into-array Integer/TYPE lb)
              (into-array Integer/TYPE ub))

     [[:var var-name _ [:task start duration end]] _]
     (let [name? (some-fn string? keyword?)
           task (.taskVar
                 model
                 (if (name? start)
                   (lookup-var start)
                   (apply anon-int-var model start))
                 (if (name? duration)
                   (lookup-var duration)
                   (apply anon-int-var model duration))
                 (if (name? end)
                   (lookup-var end)
                   (apply anon-int-var model end)))]
       (.ensureBoundConsistency task)
       task)

     [[:var var-name _ [:tuples feasible? ints-lists]] _]
     :guard [feasible? #{:allowed :forbidden}, ints-lists [sequential? (p every? (p every? int?))]]
     (Tuples. (->> ints-lists (map int-array) into-array) ({:allowed true :forbidden false} feasible?))
     )))

(defn- compile-var-statement [[vars-index vars model] statement]
  (let [var-name (second statement)
        var (if-let [compile-fn (-> statement meta :compiler)]
              (compile-fn model vars-index statement)
              (compile-var-statement-helper vars-index vars model statement)
              )]
    [(-> vars-index
         (with-meta {:ast-statement statement})
         (assoc var-name var))
     (conj vars var)
     model]))

(defn compile-vars [model ast]
  ;;(println 'compile-vars (filter (some-fn view? var?) ast))
  (->>
   ast
   (filter (some-fn view? var?))
   (reduce compile-var-statement [{} [] model])))

(defn compile-constraints [model vars-index ast]
  (->>
   ast
   (map (partial compile-constraint-statement vars-index model))
   doall))

(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [
         uncompiled-constraints (->> ast (filter constraint?))
         uncompiled-reifies     (->> ast (filter reify?))
         [vars-index vars _]    (compile-vars model ast)
         public-var-names       (->> ast (filter public-var?) (map second))
         public-vars-index      (select-keys vars-index public-var-names)
         _reifies               (compile-reifies model vars-index uncompiled-reifies)
         constraints (->>
                      (compile-constraints model vars-index uncompiled-constraints)
                      ;;the conditional constraints return void, and are posted automatically
                      ;;the (when %) prevents NULL.post()
                      (map (juxt identity #(when % (.post %))))
                      (map first)
                      doall)]
     ;;(println 'vars-map (map vector vars-index vars))
     (with-meta
       {
        :ast ast
        :var-name-mapping (:var-name-mapping (meta ast))
        :constraints constraints
        :model model
        :vars vars
        :vars-map (map vector vars-index vars)
        :public-vars-index public-vars-index
        :vars-index vars-index
        }
       {:compiled? true}))))
