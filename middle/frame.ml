type access = InFrame of int | InReg of Temp.temp

type frame = {
  name    : Temp.label ;
  formals : access list ;
  mutable stack_loc : int
}

let rec upd_frame_acc acc init = match acc with
  | [] -> []
  | (InFrame _) :: tl ->
      let upd_tail = upd_frame_acc tl (init+1) in
      InFrame init :: upd_tail
  | p :: tl ->
      let upd_tail = upd_frame_acc tl init in
      p :: upd_tail

let make_acc = function
  | true -> InFrame 0
  | false -> InReg (Temp.new_temp ())

let new_frame name formals =
  let accf = List.map make_acc formals in
  let formals = upd_frame_acc accf 0 in
  {name ; formals ; stack_loc = -1}

let name f = f.name

let formals f = f.formals

let alloc_local f esc = match esc with
  | true -> 
      let loc = f.stack_loc in
      (f.stack_loc <- (f.stack_loc - 1)) ; InFrame loc
  | false -> InReg (Temp.new_temp ())
