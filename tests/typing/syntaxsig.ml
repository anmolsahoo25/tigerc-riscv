open Result
module M = Front.Parser.Make (Grammar)

include M

type expr =
  | Prog of expr list
  | VarDec of expr * expr
  | ConstInt of int
  | ConstChar of char
  | Var of expr
  | VarUpd of expr * expr
  | AddOp of expr * expr
  | Id of string
  | Empty

and

syntax_tree = expr

type ty =
  | TyInt
  | TyChar
  | TyUnit

let semantic_action _ _ = Empty

let type_check tree =
  let env = Hashtbl.create 1024 in
  let rec type_check_aux = function
    | Empty -> assert false
    | Prog els ->
      let res = List.map type_check_aux els in
      let err = List.find_opt is_error res in
      begin match err with
      | None -> Ok (get_ok (List.nth res (List.length res - 1)))
      | Some _ -> Error ""
      end
    | VarDec (Id s, e) ->
      let res_type = type_check_aux e in
      if (is_ok res_type) then begin
        Hashtbl.add env s (get_ok res_type) ;
        Ok TyUnit
      end else begin
        Error ""
      end
    | VarDec (_ , _) ->
      Error ""
    | ConstInt _ ->
      Ok TyInt
    | ConstChar _ ->
      Ok TyChar
    | Var (Id s) ->
      let r = Hashtbl.find_opt env s in
      begin match r with
      | None -> Error ""
      | Some ty -> Ok ty
      end
    | Var _ ->
      Error ""
    | VarUpd (Id s , e) ->
      let r = Hashtbl.find_opt env s in
      let ty = type_check_aux e in
      begin match (r,ty) with
      | None, _ -> Error ""
      | _, Error _ -> Error ""
      | Some tl, Ok tr ->
          if (tl = tr) then
            Ok TyUnit
          else
            Error ""
      end
    | VarUpd (_, _) ->
      Error ""
    | AddOp (e1, e2) ->
      let ty1 = type_check_aux e1 in
      let ty2 = type_check_aux e2 in
      begin match (ty1, ty2) with
      | Ok TyInt, Ok TyInt -> Ok TyInt
      | _ , _ -> Error "" 
      end
    | Id s ->
      begin match (Hashtbl.find_opt env s) with
      | Some t -> Ok t
      | None -> Error ""
      end
  in
  type_check_aux tree
