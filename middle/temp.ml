type temp = int
type label = Symbol.symbol

let next_temp = ref 0
let next_label = ref 0

let newtemp () =
  let temp = !next_temp in
  next_temp := !next_temp + 1 ; temp

let newlabel () =
  let label = !next_label in
  next_label := !next_label + 1 ;
  Symbol.make ("L" ^ (string_of_int label))

let named_label = Symbol.make
