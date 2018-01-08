(ns ^:model loco.model.compiler
  (:require [loco.compiler :as compiler]
            [loco.model :as model])
  (:use clojure.test
        loco.model.test
        loco.constraints)
  (:import org.chocosolver.solver.Model)
  )

(deftest compiling-vars-test

  (testing "consts vars"
    (is
     (=
      '([:7 ["7" 7 7]]
        [:a ["a" 7 7]]
        [:b ["b" 4 4]])
      (->>
       [($const :7 7)
        ($const :a 7)
        ($const :b 4)]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB))
                         second)))))))

  (testing "boolVars"
    (is
     (=
      '([:bool ["bool" 0 1]])
      (->>
       [($in :bool 0 1)]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB))
                         second))))))

    (is
     (=
      '([:bool ["bool" 0 1 true]])
      (->>
       [($bool :bool)]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB)
                          (memfn hasEnumeratedDomain))
                         second))))))
    )

  (testing "intVars"
    (is
     (=
      '([:7 ["7" 7 7]])
      (->>
       [($in :7 7 7)]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB))
                         second))))))

    (is
     (=
      '([:9 ["9" 1 13 true]])
      (->>
       [($in :9 [1 2 3 5 8 13])]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB)
                          (memfn hasEnumeratedDomain))
                         second))))))

    (is
     (=
      '([:7 ["7" 7 7 true]]
        [:8 ["8" -1000 1000 true]]
        [:9 ["9" 1 3 false]])
      (->>
       [($in :7 7 7)
        ($in :8 -1000 1000)
        ($in :9 1 3 :bounded)]
       model/compile
       compiler/compile
       :vars
       (map (juxt first (comp
                         (juxt
                          (memfn getName)
                          (memfn getLB)
                          (memfn getUB)
                          (memfn hasEnumeratedDomain))
                         second))))))

    )
  )

(deftest compiling-constraints-test
  (is
   (=
    '("SUM ([z + y + x = 1])")
      (->>
       [($bool :x)
        ($bool :y)
        ($bool :z)
        ($sum 1 := [:x :y :z])]
       model/compile
       compiler/compile
       :model
       (.getCstrs)
       (map (memfn toString))
       )))

  (is
   (=
    '("SUM ([z + y + x = 2])")
    (->>
     [($in :x 1 2)
      ($in :y 1 2)
      ($in :z 1 2)
      ($sum 2 := [:x :y :z])]
     model/compile
     compiler/compile
     :model
     (.getCstrs)
     (map (memfn toString))
     )))

  (is
   (=
    '("SUM ([z + y + x = 2])")
    (->>
     [($in :x 1 2)
      ($bool :y)
      ($in :z 1 2)
      ($sum 2 := [:x :y :z])]
     model/compile
     compiler/compile
     :model
     (.getCstrs)
     (map (memfn toString)))))
  )
