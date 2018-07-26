(ns loco.constraints.bin-packing
    (:require
     [clojure.core.match :refer [match]]
     [clojure.spec.alpha :as s]
     [clojure.walk :as walk]
     [loco.constraints.utils :refer :all :as utils]
     [loco.utils :refer [p]]
     )
    (:import
     [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'bin-packing)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'item-bin}  ::utils/coll-coerce-intvar?)
                       (s/tuple #{'item-size} ::utils/coll-int?)
                       (s/tuple #{'bin-load}  ::utils/coll-coerce-intvar?)
                       (s/tuple #{'offset}    int?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (p utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ item-bin] [_ item-size] [_ bin-load] [_ offset]]}
           (.binPacking model
                        (->> item-bin (map coerce-int-var) (into-array IntVar))
                        (int-array item-size)
                        (->> bin-load (map coerce-int-var) (into-array IntVar))
                        offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $bin-packing
  "Creates a BinPacking constraint.

Bin Packing formulation:
  forall b in [0, binLoad.length - 1],
  binLoad[b] = sum(itemSize[i] | i in [0, itemSize.length - 1])
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
  ([item-bin->size-map bin-load] {:pre [(map? item-bin->size-map)]}
   ($bin-packing (keys item-bin->size-map) (vals item-bin->size-map) bin-load))

  ([item-bin, item-size, bin-load]
   ($bin-packing item-bin item-size bin-load 0))

  ([item-bin, item-size, bin-load, offset]
   {:pre [(sequential? item-bin)
          (not-empty item-bin)
          (distinct? item-bin)
          (sequential? item-size)
          (every? pos-int? item-size)
          (= (count item-size) (count item-bin))
          (sequential? bin-load)
          (nat-int? offset)]}
   (constraint constraint-name
               [['item-bin  (vec item-bin)]
                ['item-size (vec item-size)]
                ['bin-load  (vec bin-load)]
                ['offset    offset]]
               compiler)))
