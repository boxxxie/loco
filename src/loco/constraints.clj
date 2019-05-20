(ns loco.constraints)

(def imports
  [
   "constraints/vars"
   "constraints/views/minus"
   "constraints/views/offset"
   "constraints/views/scale"
   "constraints/views/abs"
   "constraints/views/affine"
   "constraints/arithm"
   "constraints/all_equal"
   "constraints/arithmetic/sum"

   "constraints/times"

   "constraints/arithmetic/subtraction"

   "constraints/mod"
   "constraints/abs"
   "constraints/arithmetic/div"

   "constraints/not_all_equal"

   "constraints/all_different"
   "constraints/all_different_except_0"
   "constraints/among"
   "constraints/at_least_n_values"
   "constraints/at_most_n_values"
   "constraints/bin_packing"
   "constraints/bits_int_channeling"
   "constraints/bools_int_channeling"
   "constraints/cardinality"
   "constraints/circuit"
   "constraints/clauses_int_channeling"
   "constraints/count"
   "constraints/cumulative"
   "constraints/diff_n"

   "constraints/distance"
   "constraints/element"
   "constraints/int_value_precede"
   "constraints/int_value_precede_chain"
   "constraints/inverse_channeling"
   "constraints/knapsack"
   "constraints/lex_chain_less"
   "constraints/lex_chain_less_equal"
   "constraints/lex_less"
   "constraints/lex_less_equal"
   "constraints/max"
   "constraints/member"
   "constraints/min"
   "constraints/n_values"
   "constraints/not_member"
   ;;"constraints/nth" ;;TODO: implement
   "constraints/path"
   "constraints/scalar"
   "constraints/sort"
   "constraints/square"
   "constraints/sub_circuit"
   "constraints/sub_path"
   "constraints/table"
   "constraints/tree"

   "constraints/set/intersection"
   "constraints/set/union"
   "constraints/set/nb_empty"
   "constraints/set/not_empty"
   "constraints/set/offset"
   "constraints/set/partition"
   "constraints/set/subset_eq"
   "constraints/set/sum_elements"
   "constraints/set/symetric"
   "constraints/set/all_disjoint"
   "constraints/set/disjoint"
   "constraints/set/set_bools_channeling"
   "constraints/set/sets_ints_channeling"
   "constraints/set/inverse"

   "constraints/logic/logic"
   "constraints/automata/regular"
   ])

(apply load imports)
