(ns loco.constraints.partials-test
  (:require [loco.model :as model]
            [loco.compiler :as compiler]
            [loco.solver :as solver])
  (:use
   loco.constraints
   clojure.test))

(deftest ^:model partials-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :-x :proto [:int -5 0]]
     [:var :y-x :proto [:int -5 5]]
     [:var :-y-x :proto [:int -5 5]]
     [:var :x-y-x :proto [:int -5 10]]
     [:constraint ['sum [:y-x '= [:y :-x]]]]
     [:constraint ['sum [:x-y-x '= [:x :-y-x]]]]
     [:constraint ['arithm [:5 '= :x-y-x]]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x ($- :y :x)))]

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
     [:constraint ['sum [:y-x-7 '= [:y :-x :-7]]]]
     [:constraint ['sum [:x-3-y-x-7 '= [:x :-3 :-y-x-7]]]]
     [:constraint ['arithm [:5 '= :x-3-y-x-7]]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x 3 ($- :y :x 7)))]

    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :10 :hidden [:const 10]]
     [:var :5 :hidden [:const 5]]
     [:var :x+y+5 :proto [:int 5 15]]
     [:constraint ['sum [:x+y+5 '= [:x :y :5]]]]
     [:constraint ['arithm [:10 '= :x+y+5]]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 10 ($+ :x :y 5))]

    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int 0 25]]
     [:constraint ['times [:x*y '= :x '* :y]]]
     [:constraint ['arithm [:0 '= :x*y]]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 0 ($* :x :y))]

    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 5]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int -25 25]]
     [:constraint ['times [:x*y '= :x '* :y]]]
     [:constraint ['arithm [:0 '= :x*y]]]]
    [($in :x -5 5)
     ($in :y 0 5)
     ($= 0 ($* :x :y))]

    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int -25 10]]
     [:var :-x*y :proto [:int -10 25]]
     [:constraint ['times [:x*y '= :x '* :y]]]
     [:constraint ['arithm [:0 '= :-x*y]]]]
    [($in :x -5 5)
     ($in :y 0 2)
     ($= 0 ($neg ($* :x :y)))]

    [[:var :x :public [:int 5 5]]
     [:var :y :public [:int 0 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x/y :proto [:int 2 5]]
     [:constraint [:div [:x/y := :x :/ :y]]]
     [:constraint ['arithm [:0 '= :x/y]]]]
    [($in :x 5 5)
     ($in :y 0 2)
     ($= 0 ($div :x :y))]

    [[:var :x :public [:int 5 5]]
     [:var :y :public [:int -2 0]]
     [:var :0 :hidden [:const 0]]
     [:var :x/y :proto [:int -2 5]]
     [:constraint [:div [:x/y := :x :/ :y]]]
     [:constraint ['arithm [:0 '= :x/y]]]]
    [($in :x  5 5)
     ($in :y -2 0)
     ($= 0 ($div :x :y))]

    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 2 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x/y :proto [:int -2 2]]
     [:constraint [:div [:x/y := :x :/ :y]]]
     [:constraint ['arithm [:0 '= :x/y]]]]
    [($in :x -5 5)
     ($in :y 2 2)
     ($= 0 ($div :x :y))]

    [[:var :_a :hidden [:const 1]]
     [:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :-y :proto [:int -5 0]]
     [:var :x-y :proto [:int -5 5]]
     [:var :-x :proto [:int -5 0]]
     [:var :y-x :proto [:int -5 5]]
     [:constraint ['sum [:x-y '= [:x :-y]]]]
     [:constraint ['sum [:y-x '= [:y :-x]]]]
     [:constraint [:all-equal [:5 :x-y :y-x]]]]
    [($in :_a 1)
     ($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x :y) ($- :y :x))]

    [[:var :x :public [:int 0 5]]
     [:var :5 :hidden [:const 5]]
     [:var :-5 :proto [:const -5]]
     [:var :x-5 :proto [:int -5 0]]
     [:constraint ['sum [:x-5 '= [:x :-5]]]]
     [:constraint ['arithm [:x-5 '= 0]]]]
    [($in :x 0 5)
     ($= ($- :x 5) 0)]

    [[:var :x :public [:bool 0 1]]
     [:var :y :public [:bool 0 1]]
     [:var :2 :hidden [:const 2]]
     [:var :x+y :proto [:int 0 2]]
     [:constraint ['sum [:x+y '= [:x :y]]]]
     [:constraint ['arithm [:2 '= :x+y]]]]
    [($bool :x)
     ($bool :y)
     ($= 2 ($+ :x :y))]

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
     [:constraint ['times [:8*9 '= :8 '* :9]]]
     [:constraint ['times [:7*8*9 '= :7 '* :8*9]]]
     [:constraint ['times [:6*7*8*9 '= :6 '* :7*8*9]]]
     [:constraint ['times [:5*6*7*8*9 '= :5 '* :6*7*8*9]]]
     [:constraint ['times [:4*5*6*7*8*9 '= :4 '* :5*6*7*8*9]]]
     [:constraint ['times [:3*4*5*6*7*8*9 '= :3 '* :4*5*6*7*8*9]]]
     [:constraint ['times [:2*3*4*5*6*7*8*9 '= :2 '* :3*4*5*6*7*8*9]]]
     [:constraint ['times [:1*2*3*4*5*6*7*8*9 '= :1 '* :2*3*4*5*6*7*8*9]]]
     [:constraint ['arithm [:a '= :1*2*3*4*5*6*7*8*9]]]]
    [($in :a 0 (* 1 2 3 4 5 6 7 8 9))
     ($= :a ($* 1 2 3 4 5 6 7 8 9))]

    [[:var :x :public [:int 0 100]]
     [:var :y :hidden [:const 10]]
     [:var :z :public [:int 0 5]]
     [:var :x%y :proto [:int 0 10]]
     [:constraint ['mod [:x%y '= :x '% :y]]]
     [:constraint ['arithm [:z '= :x%y]]]]
    [($in :x 0 100)
     ($const- :y 10)
     ($in :z 0 5)
     ($= :z ($% :x :y))]
    ))
