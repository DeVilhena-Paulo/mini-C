
(* ------------------------------------------------------------------------------------ *)
(* Translation: RTL -> ERTL                                                             *)
(*                                                                                      *)
(* In this step, we add to our sequence of instructions a more realistic treatement of  *)
(* the memory at three phases of the program: at the beginning of a function, at the    *)
(* end of a function and at a function call. It's the step where we make the x86-64     *)
(* calling conventions explict, which explains the E in ERTL (Explicit Register         *)
(* Transfer Langage).                                                                   *)
(* ------------------------------------------------------------------------------------ *)

open Rtltree
open Ertltree


(* ------------------------------------------------------------------------------------ *)
(* Global variables                                                                     *)
(* ------------------------------------------------------------------------------------ *)

(* Control flow graph for each function *)
let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let split m l =
  let rec aux (acc, i) elt = if i >= m then (acc, i) else ((elt :: acc), (i + 1)) in
  let res, _ = List.fold_left aux ([], 0) l in 
  List.rev res


(* ------------------------------------------------------------------------------------ *)
(* Translation of an instuction                                                         *)
(* ------------------------------------------------------------------------------------ *)

(* All instructions remain unchanged, except for the function call and the division. *)
let instr = function
  | Rtltree.Econst (n, r, l)               -> Ertltree.Econst (n, r, l)
  | Rtltree.Eload (r1, d, r2, l)           -> Ertltree.Eload (r1, d, r2, l)
  | Rtltree.Estore (r1, r2, d, l)          -> Ertltree.Estore (r1, r2, d, l) 
  | Rtltree.Emunop (unop, r, l)            -> Ertltree.Emunop (unop, r, l)
  | Rtltree.Egoto l                        -> Ertltree.Egoto l
  | Rtltree.Emubranch (br, r, lt, lf)      -> Ertltree.Emubranch (br, r, lt, lf)
  | Rtltree.Embbranch (br, r1, r2, lt, lf) -> Ertltree.Embbranch (br, r1, r2, lt, lf)
  | Rtltree.Embinop (Ops.Mdiv, r1, r2, l)  ->
     let out_l = generate (Ertltree.Embinop (Ops.Mmov, Register.rax, r2, l)) in
     let div_l = generate (Ertltree.Embinop (Ops.Mdiv, r1, Register.rax, out_l)) in
     Ertltree.Embinop (Ops.Mmov, r2, Register.rax, div_l)
  | Rtltree.Embinop (binop, r1, r2, l)     -> Ertltree.Embinop (binop, r1, r2, l)
  | Rtltree.Ecall (r, id, r_list, l)       ->
     let n = List.length r_list in
     let unstack_l =
       if n > 6 then
         generate (Ertltree.Emunop (Ops.Maddi (Int32.of_int (8 * (n - 6))), Register.rsp, l))
       else l in
     let copyres_l = generate (Ertltree.Embinop (Ops.Mmov, Register.result, r, unstack_l)) in
     let funcall_l = generate (Ertltree.Ecall (id, min 6 n, copyres_l)) in
     let argpush_l =
       if n > 6 then
         let r_list' = split (n - 6) (List.rev r_list) in
         List.fold_left (fun l r -> generate (Ertltree.Epush_param (r, l))) funcall_l r_list'
       else funcall_l in
     let argpass_l =
       let r_list', p_list' =
         let p_list = Register.parameters in
         if n > 6 then (split 6 r_list), p_list
         else r_list, (split n p_list) in
       let aux r' p' l' = generate (Ertltree.Embinop (Ops.Mmov, r', p', l')) in
       List.fold_right2 aux r_list' p_list' argpush_l in
     let inst = Label.M.find argpass_l !graph in
     graph := Label.M.remove argpass_l !graph;
     inst


(* ------------------------------------------------------------------------------------ *)
(* Memory menagement at the beginning of a function                                     *)
(* ------------------------------------------------------------------------------------ *)

let fun_beginning (f : Rtltree.deffun) locals =
  let n = List.length f.fun_formals in
  let argpop_l =
    if n > 6 then
      let r_list' = split (n - 6) (List.rev f.fun_formals) in
      let aux (l', ofs) r = (generate (Ertltree.Eget_param (ofs, r, l')), ofs + 8) in
      let l, _ = List.fold_left aux (f.fun_entry, 16) r_list' in l
    else f.fun_entry in
  let argrec_l =
    let r_list', p_list' =
      if n > 6 then split 6 f.fun_formals, Register.parameters
      else f.fun_formals, split n Register.parameters in
    let aux r p l' = generate (Ertltree.Embinop (Ops.Mmov, p, r, l')) in
    List.fold_right2 aux r_list' p_list' argpop_l in
  let callee_rgts, calleesav_l =
    let aux (r_list', l') r =
      let r' = Register.fresh () in
      locals := Register.S.add r' !locals;
      (r' :: r_list', generate (Ertltree.Embinop (Ops.Mmov, r, r', l'))) in
    List.fold_left aux ([],  argrec_l) Register.callee_saved in
  generate (Ertltree.Ealloc_frame calleesav_l), List.rev callee_rgts


(* ------------------------------------------------------------------------------------ *)
(* Memory menagement at the end of a function                                           *)
(* ------------------------------------------------------------------------------------ *)

let fun_ending (f : Rtltree.deffun) r_list =
  let ret_l = generate (Ertltree.Ereturn) in
  let del_l = generate (Ertltree.Edelete_frame ret_l) in
  let aux l r c =
    generate (Ertltree.Embinop (Ops.Mmov, r, c, l)) in
  let call_l = List.fold_left2 aux del_l r_list Register.callee_saved in
  Ertltree.Embinop (Ops.Mmov, f.fun_result, Register.rax, call_l)


(* ------------------------------------------------------------------------------------ *)
(* Translation of a function                                                            *)
(* ------------------------------------------------------------------------------------ *)

(* Fill the graph with the translaton of each instruction *)
let fill_graph (f : Rtltree.deffun) =
  let aux l i =
    let i' = instr i in
    graph := Label.M.add l i' !graph; in
  begin Label.M.iter aux f.fun_body end


let deffun (f : Rtltree.deffun) =
  fill_graph f;
  let fun_name = f.fun_name in
  let fun_formals = List.length f.fun_formals in
  let locals = ref f.fun_locals in
  let fun_entry, callee_rgts = fun_beginning f locals in
  let fun_exit = fun_ending f callee_rgts in
  graph := Label.M.add f.fun_exit fun_exit !graph;
  let fun_locals = !locals in
  let fun_body = !graph in
  graph := Label.M.empty;
  {
    fun_name;
    fun_formals;
    fun_locals;
    fun_entry;
    fun_body
  }


(* ------------------------------------------------------------------------------------ *)
(* Translation of a program                                                             *)
(* ------------------------------------------------------------------------------------ *)

let program ({ funs } : Rtltree.file) = { funs = List.map deffun funs }

