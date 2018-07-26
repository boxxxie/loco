;;TODO: move this into different (lower) namespace
(ns loco.constraints.test-utils
  (:require
   [clojure.test :refer :all :as t]
   [loco.compiler :as compiler]
   [loco.model :as model]
   [loco.solver :as solver]
   [loco.utils :refer [p c]]
   ))

;;deprecated
(defn compiled-constraints-strings [input]
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))

;;deprecated
(defn compiled-vars-strings [input]
  (->> input
       model/compile
       compiler/compile
       :vars
       (map str)))

;;deprecated
(defn multi-test [input]
  [input
   (model/compile input)
   (compiled-constraints-strings input)
   (compiled-vars-strings input)
   (solver/solutions input)])

;;deprecated
(defmacro choco-vars-string-assert
  "used for testing compile chain model/compile -> compiler/compile
  tests properties of vars in built Model"
  ([expected actual-input] `(choco-vars-string-assert ~expected ~actual-input nil))
  ([expected actual-input msg]
   `(is
     (=
      ~expected
      (->>
       ~actual-input
       model/compile
       compiler/compile
       :vars
       (map str)
       ))
     ~msg)))

(defmethod t/assert-expr 'loco? [msg form]
  (let [expr (nth form 1)
        {:keys [identity
                model
                compiled
                solutions] :as expected} (first (drop 2 form))
        expected-keys #{:identity :model :compiled :solutions}
        expected (if (every? nil? [identity model compiled solutions])
                   {:identity [] :model [] :compiled [] :solutions []}
                   expected)
        {:keys [identity
                model
                compiled
                solutions]} expected
        ]
    (assert (every? expected-keys (keys expected)))
    `(let [model-fn# (memoize ~model/compile)
           compile-fn# (memoize ~compiler/compile)
           solutions-fn# ~solver/solutions
           compiled-strings# (juxt
                              (c (p mapv str) :vars)
                              (c (p mapv str) (memfn getCstrs) :model))
           actual#
           (->
            {}
            (cond-> ,,
                ~identity  (assoc ,, :identity ~expr)
                ~model     (assoc ,, :model (model-fn# ~expr))
                ~compiled  (assoc ,, :compiled (->>
                                                ~expr
                                                model-fn#
                                                compile-fn#
                                                compiled-strings#))
                ~solutions (assoc ,, :solutions (->>
                                                 ~expr
                                                 model-fn#
                                                 compile-fn#
                                                 solutions-fn#
                                                 set))))]
       ;; yay! code reuse!
       (is (= ~expected actual#)))))

