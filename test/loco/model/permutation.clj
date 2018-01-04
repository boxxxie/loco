(ns ^:model loco.model.permutation
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

;;http://sofdem.github.io/gccat/gccat/Kpermutation.html#uid8618

(deftest distinct-test
  (is (=
       [[:var :a :public [:int 0 9]]
        [:var :b :public [:int 0 9]]
        [:var :b :public [:int 0 9]]
        [:constraint [:distinct [:a :b :c]]]]
       (->>
        [($in :a 0 9)
         ($in :b 0 9)
         ($in :b 0 9)
         ($distinct [:a :b :c])]
        model/compile)))

  (is (=
       [[:var :a :public [:int 0 9]]
        [:var :b :public [:int 0 9]]
        [:var :b :public [:int 0 9]]
        [:var :4 :hidden [:const 4]]
        [:var :a+b :proto [:int 0 18]]
        [:constraint [:sum [:a+b := [:a :b]]]]
        [:constraint [:distinct [:a :b :c :4 :a+b]]]]
       (->>
        [($in :a 0 9)
         ($in :b 0 9)
         ($in :b 0 9)
         ($distinct [:a :b :c 4 ($+ :a :b)])]
        model/compile))))

;;TODO: need to research circuit to understand if this representation is good or not
;; http://sofdem.github.io/gccat/gccat/Ccircuit.html#uid16305
(deftest circuit-test
  (is
   (=
    [[:var :a :public [:int 10 99]]
     [:var :b :public [:int 0 9]]
     [:var :b :public [:int 100 1000]]
     [:var :0 :hidden [:const 0]]
     [:constraint [:circuit [[:a :b :c] :0]]]]
    (->>
     [($in :a 10 99)
      ($in :b 0 9)
      ($in :b 100 1000)
      ($circuit [:a :b :c])]
     model/compile)))

  (is
   (=
    [[:var :a :public [:int 10 99]]
     [:var :b :public [:int 0 9]]
     [:var :b :public [:int 100 1000]]
     [:var :index :public [:int 0 2]]
     [:constraint [:circuit [[:a :b :c] :index]]]]
    (->>
     [($in :a 10 99)
      ($in :b 0 9)
      ($in :b 100 1000)
      ($in :index 0 2)
      ($circuit [:a :b :c] :index)]
     model/compile))))


(deftest cardinality-test
  (is
   (=
    [[:var :a :public [:int 2 3]]
     [:var :b :public [:int 1 3]]
     [:var :c :public [:int 1 3]]
     [:var :d :public [:int 2 3]]
     [:var :e :public [:int 1 3]]
     [:var :ones :proto [:int 0 3]]
     [:var :twos :proto [:int 0 5]]
     [:constraint
      [:cardinality
       [[:a :b :c :d :e] [[1 2] [:ones :twos]] [:closed true]]]]]
    (->>
     [($in :a 2 3)
      ($in :b 1 3)
      ($in :c 1 3)
      ($in :d 2 3)
      ($in :e 1 3)
      ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed)]
     model/compile))
   )

  (is
   (=
    [[:var :a :public [:int 1 3]]
     [:var :b :public [:int 1 3]]
     [:var :c :public [:int 2 3]]
     [:var :d :public [:int 1 3]]
     [:var :e :public [:int 1 3]]
     [:var :ones :proto [:int 0 4]]
     [:var :twos :proto [:int 0 5]]
     [:constraint
      [:cardinality
       [[:a :b :c :d :e] [[1 2] [:ones :twos]] [:closed false]]]]]
    (->>
     [($in :a 1 3)
      ($in :b 1 3)
      ($in :c 2 3)
      ($in :d 1 3)
      ($in :e 1 3)
      ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos})]
     model/compile)))
  )
