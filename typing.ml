
(* ------------------------------------------------------------------------------------ *)
(* The Type System                                                                      *)
(*                                                                                      *)
(* Once the parsing is complete, we need to verify the soundness of the program, that   *)
(* is, if the the program is well typed. To do so, we exploit two notions: rules and    *)
(* environment. Rules are formal definitions of what needs to be satisfied in order to  *)
(* check the validity of the next target. Environment is a set of variables, functions  *)
(* and structures already typed, which can be useful in a future type check             *)
(* ------------------------------------------------------------------------------------ *)

open Ttree
open Ptree


(* ------------------------------------------------------------------------------------ *)
(* Error handling                                                                       *)
(* ------------------------------------------------------------------------------------ *)

exception Error of string

type error =
  | Incompatible of Ttree.typ * Ttree.typ
  | Redefinition of string * Ttree.ident
  | Undefinition of string * Ttree.ident
  | ArrowAccess  of Ttree.typ
  | NumberOfArgs
  | Inconsistency

let string_of_type = function
  | Ttree.Tint       -> "int"
  | Ttree.Tstructp x -> "struct " ^ x.str_name ^ " *"
  | Ttree.Tvoidstar  -> "void*"
  | Ttree.Ttypenull  -> "typenull"

let format_error = function
  | Incompatible (t1, t2) ->
     (string_of_type t1) ^ " is not compatible with " ^ (string_of_type t2)
  | Redefinition (def, id) -> def ^ " '" ^ id ^  "' is already defined in the scope"
  | Undefinition (def, id) -> def ^ " '" ^ id ^ "' is not defined in the scope"
  | ArrowAccess t ->
     "arrow access is allowed for struct types, type " ^ string_of_type t ^ " was found"
  | NumberOfArgs -> "incorrect number of arguments"
  | Inconsistency -> "inconsistent type"
  
let add_position s (l, _) =
  let line, column =
    string_of_int l.Lexing.pos_lnum, string_of_int (l.Lexing.pos_cnum - l.Lexing.pos_bol) in
  (s ^ "; line: " ^ line ^ ", column: " ^ column)

let error_msg s id_loc = raise (Error (add_position (format_error s) id_loc))


(* ------------------------------------------------------------------------------------ *)
(* The relation between types                                                           *)
(* ------------------------------------------------------------------------------------ *)
                
let related = function
  | Ttree.Tint, ( Ttree.Tstructp _ | Ttree.Tvoidstar ) 
  | Ttree.Tstructp _, Ttree.Tint                       
  | Ttree.Tvoidstar, ( Ttree.Tint | Ttree.Ttypenull )  
  | Ttree.Ttypenull, Ttree.Tvoidstar -> false
  | _ -> true


(* ------------------------------------------------------------------------------------ *)
(* Implementation of the environment                                                    *)
(* ------------------------------------------------------------------------------------ *)

module Env =
  struct
    type t = {
      struct_env: (Ttree.ident, Ttree.structure) Hashtbl.t;
      var_env   : (Ttree.ident, Ttree.typ) Hashtbl.t;
      fun_env   : (Ttree.ident, Ttree.typ * Ttree.decl_var list) Hashtbl.t;
    }
           
    let create n =
      let (env : t) = {
        struct_env = Hashtbl.create n;
        var_env    = Hashtbl.create n;
        fun_env    = Hashtbl.create n;
        } in
      Hashtbl.add env.fun_env "putchar" (Ttree.Tint, [Ttree.Tint, "c"]);
      Hashtbl.add env.fun_env "sbrk" (Ttree.Tvoidstar, [Ttree.Tint, "n"]);
      env

    let ttyp env = function
      | Ptree.Tint -> Ttree.Tint
      | Ptree.Tstructp { id; id_loc } ->
         Ttree.Tstructp (Hashtbl.find env.struct_env id)

    let tvars env decl_list =
      List.map (fun (t, dvar) -> (ttyp env t, dvar.id)) decl_list

    let well_typed env = function
      | Ptree.Tint -> true
      | Ptree.Tstructp { id; id_loc } -> Hashtbl.mem env.struct_env id

    let check_uniquiness _env def id id_loc =
      if Hashtbl.mem _env id then error_msg (Redefinition (def, id)) id_loc

    let check_type env (t, { id; id_loc }) =
      if not (well_typed env t) then error_msg Inconsistency id_loc

    let add_fun env fun_typ ({ id; id_loc } as fun_name) fun_formals =
      check_uniquiness env.fun_env "function" id id_loc;
      check_type env (fun_typ, fun_name);
      Hashtbl.add env.fun_env id (ttyp env fun_typ, tvars env fun_formals)
  
    let check_vars env decl_list =
      List.iter (check_type env) decl_list;
      let aux var_env (t, { id; id_loc }) =
        check_uniquiness var_env "variable" id id_loc;
        Hashtbl.add var_env id (ttyp env t);
      var_env in
      List.fold_left aux (Hashtbl.create 13) decl_list

    let add_struct env {id; id_loc } decl_list =
      check_uniquiness env.struct_env "struct" id id_loc;
      let fields = Hashtbl.create 13 in
      let size = 8 * (List.length decl_list) in
      Hashtbl.add env.struct_env id { str_name = id; str_fields = fields; str_size = size };
      let var_env = check_vars env decl_list in
      let build_field field_name field_typ field_pos = { field_name; field_typ; field_pos } in
      begin
        let pos = ref 0 in
        let aux id t = Hashtbl.add fields id (build_field id t (!pos * 8)); incr(pos) in
        Hashtbl.iter aux var_env
      end

    let add_vars env decl_list =
      let var_env = check_vars env decl_list in
      Hashtbl.iter (fun key el -> Hashtbl.add env.var_env key el) var_env

    let remove_vars env decl_list =
      let var_env = check_vars env decl_list in      
      Hashtbl.iter (fun key _ -> Hashtbl.remove env.var_env key) var_env

    let find_opt env id = Hashtbl.find_opt env id

    let find_opt_struct { struct_env; _ } id = find_opt struct_env id

    let find_opt_var { var_env; _ } id = find_opt var_env id

    let find_opt_fun { fun_env; _ } id = find_opt fun_env id

    let find env id = Hashtbl.find env id

    let find_struct { struct_env; _ } id = find struct_env id

    let find_var { var_env; _ } id = find var_env id

    let find_fun { fun_env; _ } id = find fun_env id
    
  end

      
(* ------------------------------------------------------------------------------------ *)
(* Typing expressions                                                                   *)
(* ------------------------------------------------------------------------------------ *)

let build_expr expr_node expr_typ =
  { expr_node; expr_typ }

let rec texpr env e =
  match e.expr_node with
  | Ptree.Econst i ->
     let expr_typ = if i = Int32.zero then Ttree.Ttypenull else Ttree.Tint in
     let expr_node = Ttree.Econst i in
     (build_expr expr_node expr_typ)
       
  | Ptree.Eright lval ->
     begin match lval with
     | Ptree.Lident { id; id_loc } ->
        let expr_typ =
          match (Env.find_opt_var env id) with
          | Some t -> t
          | None -> error_msg (Undefinition ("variable", id)) id_loc in
        let expr_node = Ttree.Eaccess_local id in
        (build_expr expr_node expr_typ)
     | Ptree.Larrow (e, { id; id_loc }) ->
        let e' = texpr env e in
        match e'.expr_typ with
        | Ttree.Tstructp { str_name; str_fields } ->
           let expr_node, expr_typ = match Hashtbl.find_opt str_fields id with
           | Some f -> Ttree.Eaccess_field (e', f), f.field_typ
           | None -> error_msg (Undefinition ("field", id)) e.expr_loc in
           (build_expr expr_node expr_typ)
        | _ -> error_msg (ArrowAccess e'.expr_typ) e.expr_loc end

  | Ptree.Eassign (lval, e) ->
     let e2' = texpr env e in
     let expr_node, expr_typ =
       match texpr env { expr_node = (Ptree.Eright lval); expr_loc = e.expr_loc } with
       | { expr_node = Ttree.Eaccess_local id; expr_typ; } ->
          Ttree.Eassign_local (id, e2'), expr_typ
       | { expr_node = Ttree.Eaccess_field (e1', f); expr_typ; } ->
          Ttree.Eassign_field (e1', f, e2'), expr_typ
       | _ -> assert false (* unreachable *) in
     if related (e2'.expr_typ, expr_typ) then (build_expr expr_node expr_typ)
     else error_msg (Incompatible (expr_typ, e2'.expr_typ)) e.expr_loc
       
  | Ptree.Eunop (op, e) ->
     let e' = texpr env e in
     let continue = match op with
       | Ptree.Unot -> true
       | Ptree.Uminus -> related (e'.expr_typ, Ttree.Tint) in
     if continue then (build_expr (Ttree.Eunop (op, e')) Ttree.Tint)
     else error_msg (Incompatible (e'.expr_typ, Ttree.Tint)) e.expr_loc
       
  | Ptree.Ebinop (op, e1, e2) ->
     let e1', e2' = texpr env e1, texpr env e2 in
     let continue = match op with
       | ( Ptree.Beq | Ptree.Bneq | Ptree.Blt |
           Ptree.Ble | Ptree.Bgt | Ptree.Bge ) -> related (e1'.expr_typ, e2'.expr_typ)
       | ( Ptree.Badd | Ptree.Bsub | Ptree.Bmul | Ptree.Bdiv ) ->
            (related (e1'.expr_typ, Ttree.Tint)) && (related (e2'.expr_typ, Ttree.Tint))
       | ( Ptree.Band | Ptree.Bor ) -> true in
     if continue then (build_expr (Ttree.Ebinop (op, e1', e2')) Ttree.Tint)
     else error_msg (Incompatible (e1'.expr_typ, e2'.expr_typ)) e1.expr_loc
       
  | Ptree.Ecall ({ id; id_loc }, expr_list) ->
     let expr_list' = List.map (texpr env) expr_list in
     begin match Env.find_opt_fun env id with
     | Some (fun_typ, fun_formals) ->
        let aux ((continue, _) as acc) (e, (f_typ, _)) =
          if not continue then acc
          else ((related (e.expr_typ, f_typ)), (e.expr_typ, f_typ)) in
        let continue, (t1, t2) =
          try List.fold_left aux (true, (Tint, Tint)) (List.combine expr_list' fun_formals)
          with Invalid_argument _ -> error_msg NumberOfArgs id_loc in
        if continue then (build_expr (Ttree.Ecall (id, expr_list')) fun_typ)
        else error_msg (Incompatible (t1, t2)) id_loc
     | None -> error_msg (Undefinition ("function", id)) id_loc end
       
  | Ptree.Esizeof { id; id_loc } ->
     match Env.find_opt_struct env id with
     | Some s -> (build_expr (Ttree.Esizeof s) Ttree.Tint)
     | None -> error_msg (Undefinition ("struct", id)) id_loc


(* ------------------------------------------------------------------------------------ *)
(* Typing statements                                                                    *)
(* ------------------------------------------------------------------------------------ *)

let tbody env fun_typ =
  let rec tstmt = function
    | Ptree.Sskip -> Ttree.Sskip
    | Ptree.Sexpr e ->
       Ttree.Sexpr (texpr env e)
    | Ptree.Sif (e, s1, s2) ->
       Ttree.Sif (texpr env e, tstmt s1.stmt_node, tstmt s2.stmt_node)
    | Ptree.Swhile (e, s) ->
       Ttree.Swhile (texpr env e, tstmt s.stmt_node)
    | Ptree.Sblock (decl_list, stmt_list) ->
       Env.add_vars env decl_list;
       let tvars_list = Env.tvars env decl_list in
       let stmt_node_list = List.map (fun s -> s.stmt_node) stmt_list in
       let tstmt_list = List.map tstmt stmt_node_list in
       Env.remove_vars env decl_list;
       Ttree.Sblock (tvars_list, tstmt_list)
    | Ptree.Sreturn e ->
       let { expr_node; expr_typ } as e' = texpr env e in
       if (related (expr_typ, fun_typ)) then Ttree.Sreturn e'
       else error_msg (Incompatible (expr_typ, fun_typ)) e.expr_loc in
  tstmt


(* ------------------------------------------------------------------------------------ *)
(* Typing function and structure declarations                                           *)
(* ------------------------------------------------------------------------------------ *)


let tdecl env = function
  | Ptree.Dfun { fun_typ; fun_name; fun_formals = decl_list; fun_body } ->
     Env.add_fun env fun_typ fun_name decl_list;
     Env.add_vars env decl_list;
     let fun_typ, fun_formals = Env.find_fun env fun_name.id in
     let fun_name = fun_name.id in
     let fun_body =  (* We consider the body of a function as a block statement *)
       let fun_block = Ptree.Sblock (fun_body) in
       match tbody env fun_typ fun_block with Ttree.Sblock b -> b | _ -> assert false in
     Env.remove_vars env decl_list;
     Some ({ fun_typ; fun_name; fun_formals; fun_body } : Ttree.decl_fun)
  | Ptree.Dstruct (ident, decl_list) ->
     (* Structure definitions only interfere with updating the environment *)
     Env.add_struct env ident decl_list;
     None


(* ------------------------------------------------------------------------------------ *)
(* Typing the file                                                                      *)
(* ------------------------------------------------------------------------------------ *)

let program p =
  let filter_map l ~f =
    List.map f l
    |> List.filter (fun x -> x <> None)
    |> List.map (function Some e -> e | None -> assert false) in
  { funs = filter_map p ~f:(tdecl (Env.create 13)) }

