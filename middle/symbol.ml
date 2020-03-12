type symbol = string * int

let next = ref 0
let symbol_table = Hashtbl.create 1024

let make s = match Hashtbl.find_opt symbol_table s with
  | Some i -> (s, i)
  | None ->
    let i = !next in
    Hashtbl.add symbol_table s i;
    next := !next + 1;
    (s, i)


let name (s,_) = match Hashtbl.find_opt symbol_table s with
  | Some _ -> Some s
  | None -> None
