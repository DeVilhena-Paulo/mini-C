
(* ------------------------------------------------------------------------------------ *)
(* Translation: Ops -> RTL                                                              *)
(*                                                                                      *)
(* In this step of the compilation process, we translate the output from the operation  *)
(* selection step to RTL (Register Transfer Langage), a sequential langague very        *)
(* similar to assembly in structure, but different in memory management. Indeed, it's   *)
(* in this step that the abstract syntax structure will be destroyed and will be        *)
(* replaced by a sequence of instructions operating over infinitely many registers.     *)
(* ------------------------------------------------------------------------------------ *)

open Ops
open Rtltree


(* ------------------------------------------------------------------------------------ *)
(* Global variables                                                                     *)
(* ------------------------------------------------------------------------------------ *)

(* Control flow graph                                                                   *)
(* It consists of a mapping between labels and instructions. A label identifies one and *)
(* only one instruction and one instruction stores zero, one or two next labels, which  *)
(* indentify in their turn the next instruction to be executed.                         *)
let (graph : Rtltree.cfg ref) = ref Label.M.empty

(* Mapping between variable idents and registers (pseudo-registers) *)
let (registers : (Ops.ident, Register.t) Hashtbl.t) = Hashtbl.create 13

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let add_vars decl_list =
  List.map (fun id -> let r = Register.fresh () in Hashtbl.add registers id r; r) decl_list

let var_register id = Hashtbl.find registers id


(* ------------------------------------------------------------------------------------ *)
(* Traslation of an expression                                                          *)
(*                                                                                      *)
(* Much of the work has already been done in the Ops module. Here we only orchestrate   *)
(* the flow by creating pseudo-registers, moving values between them and updating the   *)
(* control flow graph (CFG)                                                             *)
(* ------------------------------------------------------------------------------------ *)

let rec expr e destr destl =
  match e with
  | Ops.Mconst i ->
     generate (Rtltree.Econst (i, destr, destl))
  | Ops.Maccess id ->
     generate (Rtltree.Embinop (Ops.Mmov, var_register id, destr, destl))
  | Ops.Massign (id, e) ->
     expr e destr (generate (Rtltree.Embinop (Ops.Mmov, destr, var_register id, destl)))
  | Ops.Mload (d, e) ->
     let r = Register.fresh () in
     expr e r (generate (Rtltree.Eload (r, d, destr, destl)))
  | Ops.Mstore (d, e1, e2) ->
     let r1, r2 = Register.fresh (), Register.fresh () in
     let l = generate (Rtltree.Estore (r2, r1, d, destl)) in
     expr e1 r1 (expr e2 r2 (generate (Rtltree.Embinop (Ops.Mmov, r2, destr, l))))
  | Ops.Mbinop (binop, e1, e2) ->  (* e1 op e2 *)
     (* [destr <- (e1: destr) op (e2: r)] *)
     let r = Register.fresh () in
     let l = generate (Rtltree.Embinop (binop, r, destr, destl)) in
     expr e1 destr (expr e2 r l)
  | Ops.Munop (unop, e) ->
     let l1 = generate (Rtltree.Emunop (unop, destr, destl)) in
     expr e destr l1  (* [destr <- unop (e: destr)] *)
  | Ops.Mcall (id, expr_list) ->
     let r_list = List.rev_map (fun _ -> Register.fresh ()) expr_list in
     let l = generate (Rtltree.Ecall (destr, id, r_list, destl)) in
     List.fold_right2 expr expr_list r_list l


(* ------------------------------------------------------------------------------------ *)
(* Translation of an expression used as a condition                                     *)
(* ------------------------------------------------------------------------------------ *)

let general_case e lt lf =
  let r = Register.fresh () in
  expr e r (generate (Rtltree.Emubranch (Ops.Mjnz, r, lt, lf)))

  
let rec condition e lt lf =
  match e with
  | Ops.Mconst i -> if i <> Int32.zero then lt else lf
  | Ops.Mbinop (Ops.Mmul, e1, e2) -> condition e1 (condition e2 lt lf) lf
  | Ops.Mbinop (Ops.Msetl, Ops.Mconst i, e2) ->
     let r = Register.fresh () in
     expr e2 r (generate (Rtltree.Emubranch (Ops.Mjgi i, r, lt, lf)))
  | Ops.Mbinop (Ops.Msetl, e1, e2) ->
     let r1, r2 = Register.fresh (), Register.fresh () in
     expr e1 r1 (expr e2 r2 (generate (Rtltree.Embbranch (Mjl, r2, r1, lt, lf))))
  | Ops.Mbinop (Ops.Msetle, e1, Ops.Mconst i) ->
     let r = Register.fresh () in
     expr e1 r (generate (Rtltree.Emubranch (Ops.Mjlei i, r, lt, lf)))
  | Ops.Mbinop (Ops.Msetle, e1, e2) ->
     let r1, r2 = Register.fresh (), Register.fresh () in
     expr e1 r1 (expr e2 r2 (generate (Rtltree.Embbranch (Mjle, r2, r1, lt, lf))))
  | Ops.Munop (Ops.Maddi i, e2) ->
     condition (Ops.Munop (Ops.Msetnei (Int32.neg i), e2)) lt lf
  | Ops.Munop (Ops.Msetei i, e2) ->
     condition (Ops.Munop (Ops.Msetnei i, e2)) lf lt
  | Ops.Munop (Ops.Msetnei i, e2) ->
     if i = Int32.zero then condition e2 lt lf else general_case e lt lf
  | _ -> general_case e lt lf


(* ------------------------------------------------------------------------------------ *)
(* Translation of a statement                                                           *)
(* ------------------------------------------------------------------------------------ *)

let rec stmt destl retr exitl = function
  | Ops.Mskip -> destl
  | Ops.Mexpr e -> expr e (Register.fresh ()) destl
  | Ops.Mif (e, s1, s2) ->
     condition e (stmt destl retr exitl s1) (stmt destl retr exitl s2)
  | Ops.Mwhile (e, s) ->
     let goto = Label.fresh () in
     let l = condition e (stmt goto retr exitl s) destl in
     graph := Label.M.add goto (Rtltree.Egoto l) !graph;
     l
  | Ops.Mblock (id_list, stmt_list) ->
     begin List.iter (fun id -> Hashtbl.add registers id (Register.fresh ())) id_list end;
     let l = List.fold_right (fun s destl -> stmt destl retr exitl s) stmt_list destl in
     begin List.iter (fun id -> Hashtbl.remove registers id) id_list end;
     l
  | Ops.Mreturn e -> expr e retr exitl


(* ------------------------------------------------------------------------------------ *)
(* Translation of a function                                                            *)
(* ------------------------------------------------------------------------------------ *)

let deffun ({ fun_name; fun_formals; fun_locals; fun_body } : Ops.decl_fun) =
  let fun_formals = add_vars fun_formals in
  let fun_locals = Register.set_of_list (add_vars fun_locals) in
  let fun_exit = Label.fresh () in
  let fun_result = Register.fresh () in
  let fun_block = Ops.Mblock ([], fun_body) in
  let fun_entry = stmt fun_exit fun_result fun_exit fun_block in
  let fun_body = !graph in
  Hashtbl.reset registers;
  graph := Label.M.empty;
  {
    fun_name;
    fun_formals;
    fun_result;
    fun_locals;
    fun_entry;
    fun_exit;
    fun_body
  }


(* ------------------------------------------------------------------------------------ *)
(* Translation of a program                                                             *)
(* ------------------------------------------------------------------------------------ *)

let program { Ops.funs } = { funs = List.map deffun funs }
                                      
