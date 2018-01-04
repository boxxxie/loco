(ns ^:model loco.model.arithmetic
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest arithmetic-test
  (is
   (=
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :-x :proto [:int -5 0]]
     [:var :y-x :proto [:int -5 5]]
     [:var :-y-x :proto [:int -5 5]]
     [:var :x-y-x :proto [:int -5 10]]
     [:constraint [:sum [:y-x := [:y :-x]]]]
     [:constraint [:sum [:x-y-x := [:x :-y-x]]]]
     [:constraint [:all-equal [:5 :x-y-x]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($= 5 ($- :x ($- :y :x)))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :3 :hidden [:const 3]]
     [:var :7 :hidden [:const 7]]
     [:var :-x :proto [:int -5 0]]
     [:var :-7 :proto [:const -7]]
     [:var :y-x-7 :proto [:int -12 -2]]
     [:var :-3 :proto [:const -3]]
     [:var :-y-x-7 :proto [:int 2 12]]
     [:var :x-3-y-x-7 :proto [:int -1 14]]
     [:constraint [:sum [:y-x-7 := [:y :-x :-7]]]]
     [:constraint [:sum [:x-3-y-x-7 := [:x :-3 :-y-x-7]]]]
     [:constraint [:all-equal [:5 :x-3-y-x-7]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($= 5 ($- :x 3 ($- :y :x 7)))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :10 :hidden [:const 10]]
     [:var :5 :hidden [:const 5]]
     [:var :x+y+5 :proto [:int 5 15]]
     [:constraint [:sum [:x+y+5 := [:x :y :5]]]]
     [:constraint [:all-equal [:10 :x+y+5]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($= 10 ($+ :x :y 5))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :10 :hidden [:const 10]]
     [:var :5 :hidden [:const 5]]
     [:constraint [:sum [:10 := [:x :y :5]]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($sum 10 := [:x :y 5])]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int 0 25]]
     [:constraint [:times [:x*y := :x :* :y]]]
     [:constraint [:all-equal [:0 :x*y]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($= 0 ($* :x :y))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 5]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int -25 25]]
     [:constraint [:times [:x*y := :x :* :y]]]
     [:constraint [:all-equal [:0 :x*y]]]]
    (->> [($in :x -5 5)
          ($in :y 0 5)
          ($= 0 ($* :x :y))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int -25 10]]
     [:var :-x*y :proto [:int -10 25]]
     [:constraint [:times [:x*y := :x :* :y]]]
     [:constraint [:all-equal [:0 :-x*y]]]]
    (->> [($in :x -5 5)
          ($in :y 0 2)
          ($= 0 ($neg ($* :x :y)))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 5 5]]
     [:var :y :public [:int 0 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x/y :proto [:int 5 3]]
     [:constraint [:div [:x/y := :x :/ :y]]]
     [:constraint [:all-equal [:0 :x/y]]]]
    (->> [($in :x 5 5)
          ($in :y 0 2)
          ($= 0 ($div :x :y))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 2 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x/y :proto [:int -3 3]]
     [:constraint [:div [:x/y := :x :/ :y]]]
     [:constraint [:all-equal [:0 :x/y]]]]
    (->> [($in :x -5 5)
          ($in :y 2 2)
          ($= 0 ($div :x :y))]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 10]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 10]]
     [:constraint [:div [:z := :x :/ :y]]]]
    (->> [($in :x 0 10)
          ($in :y 0 10)
          ($in :z 0 10)
          ($div :x :y :z)]
         model/compile)))

  (is
   (=
    [[:var :_a :hidden [:const 1]]
     [:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :-y :proto [:int -5 0]]
     [:var :x-y :proto [:int -5 5]]
     [:var :-x :proto [:int -5 0]]
     [:var :y-x :proto [:int -5 5]]
     [:constraint [:sum [:x-y := [:x :-y]]]]
     [:constraint [:sum [:y-x := [:y :-x]]]]
     [:constraint [:all-equal [:5 :x-y :y-x]]]]
    (->> [($in :_a 1)
          ($in :x 0 5)
          ($in :y 0 5)
          ($= 5 ($- :x :y) ($- :y :x))]
         model/compile)))

  )

(deftest $arithm-test
  (is
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :10 :hidden [:const 10]]
     [:constraint [:arithm [:y := :x :/ :10]]]]
    (->> [($in :x 0 100)
          ($in :y 0 10)
          ($arithm :y := :x :/ 10)]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint [:arithm [:y := :x :/ :z]]]]
    (->> [($in :x 0 100)
          ($in :y 0 10)
          ($in :z 0 5)
          ($arithm :y := :x :/ :z)]
         model/compile)))
  )

(deftest $times-test
  (is
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint [:times [:z := :x :* :y]]]]
    (->> [($in :x 0 100)
          ($in :y 0 10)
          ($in :z 0 5)
          ($times :x :y :z)]
         model/compile)))
  )

(deftest $*-test
  (is
   (=
    [[:var :a :public [:int 0 362880]]
     [:var :1 :hidden [:const 1]]
     [:var :2 :hidden [:const 2]]
     [:var :3 :hidden [:const 3]]
     [:var :4 :hidden [:const 4]]
     [:var :5 :hidden [:const 5]]
     [:var :6 :hidden [:const 6]]
     [:var :7 :hidden [:const 7]]
     [:var :8 :hidden [:const 8]]
     [:var :9 :hidden [:const 9]]
     [:var :8*9 :proto [:int 64 72]]
     [:var :7*8*9 :proto [:int 49 504]]
     [:var :6*7*8*9 :proto [:int 36 3024]]
     [:var :5*6*7*8*9 :proto [:int 25 15120]]
     [:var :4*5*6*7*8*9 :proto [:int 16 60480]]
     [:var :3*4*5*6*7*8*9 :proto [:int 9 181440]]
     [:var :2*3*4*5*6*7*8*9 :proto [:int 4 362880]]
     [:var :1*2*3*4*5*6*7*8*9 :proto [:int 1 362880]]
     [:constraint [:times [:8*9 := :8 :* :9]]]
     [:constraint [:times [:7*8*9 := :7 :* :8*9]]]
     [:constraint [:times [:6*7*8*9 := :6 :* :7*8*9]]]
     [:constraint [:times [:5*6*7*8*9 := :5 :* :6*7*8*9]]]
     [:constraint [:times [:4*5*6*7*8*9 := :4 :* :5*6*7*8*9]]]
     [:constraint [:times [:3*4*5*6*7*8*9 := :3 :* :4*5*6*7*8*9]]]
     [:constraint [:times [:2*3*4*5*6*7*8*9 := :2 :* :3*4*5*6*7*8*9]]]
     [:constraint
      [:times [:1*2*3*4*5*6*7*8*9 := :1 :* :2*3*4*5*6*7*8*9]]]
     [:constraint [:all-equal [:a :1*2*3*4*5*6*7*8*9]]]]
    (->>
     [($in :a 0 (* 1 2 3 4 5 6 7 8 9))
      ($= :a ($* 1 2 3 4 5 6 7 8 9))]
     model/compile
     ))
   "should be able to handle clojure-like (* ...) syntax"
   ))

(deftest $mod-test
  (is
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint [:mod [:z := :x :% :y]]]]
    (->> [($in :x 0 100)
          ($in :y 0 10)
          ($in :z 0 5)
          ($mod :x :y :z)]
         model/compile)))

  (is
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:var :x%y :proto [:int 0 10]]
     [:constraint [:mod [:x%y := :x :% :y]]]
     [:constraint [:all-equal [:z :x%y]]]]
    (->> [($in :x 0 100)
          ($in :y 0 10)
          ($in :z 0 5)
          ($= :z ($mod :x :y))]
         model/compile)))

  (is
   ;;TODO: need to test this for negative numbers as well
   (=
    [[:var :x :public [:int 0 100]]
     [:var :y :hidden [:const 10]]
     [:var :z :public [:int 0 5]]
     [:var :x%y :proto [:int 0 10]]
     [:constraint [:mod [:x%y := :x :% :y]]]
     [:constraint [:all-equal [:z :x%y]]]]
    (->> [($in :x 0 100)
          ($const :y 10)
          ($in :z 0 5)
          ($= :z ($% :x :y))]
         model/compile)))

  )

(deftest abs-test
  (is
   (=
    [[:var :y :public [:int 0 10]]
     [:var :z :public [:int -5 0]]
     [:constraint [:abs [:y := :z]]]]
    (->> [($in :y 0 10)
          ($in :z -5 0)
          ($abs :y :z)]
         model/compile)))

  (is
   (=
    [[:var :y :public [:int 0 10]]
     [:var :z :public [:int -5 0]]
     [:var :|z| :proto [:int 0 5]]
     [:constraint [:abs [:|z| := :z]]]
     [:constraint [:all-equal [:y :|z|]]]]
    (->> [($in :y 0 10)
          ($in :z -5 0)
          ($= :y ($abs :z))]
         model/compile)))

  )
