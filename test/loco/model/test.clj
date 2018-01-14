(ns loco.model.test
  (:require [loco.model :as model]
            [loco.compiler :as compiler])
  (:use clojure.test
        loco.utils))

;;order of ast statements is very important... in the ast building process, not really after
(defn compiled-assert
  "used for testing model/compile"
  ([expected model-input msg]
   (is
    (=
     expected
     (model/compile model-input))
    msg))

  ([expected model-input]
   (is
    (=
     expected
     (model/compile model-input)))))

(defn constraints-assert
  "used for testing compile chain model/compile -> compiler/compile
  tests the constraints toStrings in built Model"
  ([expected actual-input] (constraints-assert expected actual-input nil))
  ([expected actual-input msg]
   (is
    (=
     expected
     (->> actual-input
          model/compile
          compiler/compile
          :model
          (.getCstrs)
          (map (memfn toString))
          ))
    msg)))

(defn vars-assert
  "used for testing compile chain model/compile -> compiler/compile
  tests properties of vars in built Model"
  [expected actual-input]
  (is
   (=
    expected
    (->>
     actual-input
     model/compile
     compiler/compile
     :vars
     (map (juxt
           (memfn getName)
           (memfn getLB)
           (memfn getUB)
           (memfn hasEnumeratedDomain)
           (memfn toString)))))))
