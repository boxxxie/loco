(ns loco.constraints.arithmetic.division-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest ^:loco division-partial-test
  (test-loco
   [($in :x 0 2)
    ($in :y 0 2)
    ($in :z 0 2)
    ($= :z ($div :x :y))]
   {:model
    '[[:var :x :public [:int 0 2]]
      [:var :y :public [:int 0 2]]
      [:var :z :public [:int 0 2]]
      [:var "x/y" :proto [:int 0 2]]
      [div ["x/y" = :x / :y]]
      [arithm [:z = "x/y"]]],
    :compiled
    [["x = {0..2}" "y = {0..2}" "z = {0..2}" "x/y = {0..2}"]
     ["DIVISION ([PropDivXYZ(x, y, x/y, ..., x/y)])"
      "ARITHM ([prop(z.EQ.x/y)])"]],
    :solutions
    #{{:x 1, :y 1, :z 1} {:x 2, :y 2, :z 1} {:x 1, :y 2, :z 0}
      {:x 0, :y 1, :z 0} {:x 0, :y 2, :z 0} {:x 2, :y 1, :z 2}}}
   )
  )
