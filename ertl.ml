
(* ------------------------------------------------------------------------------------ *)
(* Translation: RTL -> ERTL                                                             *)
(* ------------------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------------------ *)
(* Global variables                                                                     *)
(* ------------------------------------------------------------------------------------ *)

(* Control flow graph *)
let (graph : Ertltree.cfg ref) = ref Label.M.empty

(* Mapping between already processed functions and their entry label *)
let (functions : (string, Label.t) Hashtbl.t) = Hashtbl.create 13

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

let rec instr { Rtltree.fun_name; fun_entry; fun_exit; fun_formals; _ } = function
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

  (* Tail call *)
  | Rtltree.Ecall (r, id, r_list, l) when l = fun_exit && id <> "putchar" && id <> "sbrk" ->
     let n = List.length r_list in
     (* The gap between actual and next frames in number of bytes *)
     let gap = 8 * ((max 6 (List.length fun_formals)) - (max 6 n)) in
     let funcall_l =
       let tail =
         if id = fun_name then Ertltree.Egoto fun_entry
         else Ertltree.Etail_call (id, min 6 n, Hashtbl.find functions id) in
       let l1 = generate (Ertltree.Edelete_frame (generate tail)) in
       generate (Ertltree.Emunop (Ops.Maddi (Int32.of_int gap), Register.rbp, l1)) in
     let argpush_l =
       let previous_rbp, return_address = Register.fresh (), Register.fresh () in
       let r_list' = previous_rbp :: return_address :: (split (n - 6) (List.rev r_list)) in
       let aux (l', ofs) r = generate (Ertltree.Estore (r, Register.rbp, ofs, l')), ofs + 8 in
       let l', _ = List.fold_left aux (funcall_l, gap) r_list' in
       generate (Ertltree.Eload (Register.rbp, 0, previous_rbp,
         generate (Ertltree.Eload (Register.rbp, 8, return_address, l')))) in
     argpass r_list argpush_l

  (* Simple call *)
  | Rtltree.Ecall (r, id, r_list, l) ->
     let n = List.length r_list in
     let unstack_l =
       if n <= 6 then l else
         generate (Ertltree.Emunop (Ops.Maddi (Int32.of_int (8 * (n - 6))), Register.rsp, l)) in
     let copyres_l = generate (Ertltree.Embinop (Ops.Mmov, Register.result, r, unstack_l)) in
     let funcall_l = generate (Ertltree.Ecall (id, min 6 n, copyres_l)) in
     let argpush_l =
       (* If n <= 6, split returns the empty list and
          this whole expression evaluates to funcall_l *)
       let r_list' = split (n - 6) (List.rev r_list) in
       List.fold_left (fun l r -> generate (Ertltree.Epush_param (r, l))) funcall_l r_list' in
     argpass r_list argpush_l

and argpass r_list l =
  let m = min 6 (List.length r_list) in
  let aux reg par l' = generate (Ertltree.Embinop (Ops.Mmov, reg, par, l')) in
  Ertltree.Egoto (List.fold_right2 aux (split m r_list) (split m Register.parameters) l)


(* ------------------------------------------------------------------------------------ *)
(* Memory menagement at the beginning of a function                                     *)
(* ------------------------------------------------------------------------------------ *)

let fun_beginning { Rtltree.fun_formals; fun_entry; _ } locals =
  let n = List.length fun_formals in
  let argpop_l =
    let r_list' = split (n - 6) (List.rev fun_formals) in
    let aux (l', ofs) r = (generate (Ertltree.Eget_param (ofs, r, l')), ofs + 8) in
    let l, _ = List.fold_left aux (fun_entry, 16) r_list' in
    l in
  let argrec_l =
    let m = min 6 n in
    let aux reg par l' = generate (Ertltree.Embinop (Ops.Mmov, par, reg, l')) in
    List.fold_right2 aux (split m fun_formals) (split m Register.parameters) argpop_l in
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

let fun_ending f r_list =
  let ret_l = generate (Ertltree.Ereturn) in
  let del_l = generate (Ertltree.Edelete_frame ret_l) in
  let aux l r c =
    generate (Ertltree.Embinop (Ops.Mmov, r, c, l)) in
  let call_l = List.fold_left2 aux del_l r_list Register.callee_saved in
  Ertltree.Embinop (Ops.Mmov, f.Rtltree.fun_result, Register.rax, call_l)


(* ------------------------------------------------------------------------------------ *)
(* Translation of a function                                                            *)
(* ------------------------------------------------------------------------------------ *)

(* Fill the graph with the translaton of each instruction *)
let fill_graph f =
  let aux l i =
    let i' = instr f i in
    graph := Label.M.add l i' !graph; in
  begin Label.M.iter aux f.Rtltree.fun_body end


let deffun f =
  let fun_name = f.Rtltree.fun_name in
  let fun_formals = List.length f.Rtltree.fun_formals in
  let locals = ref f.Rtltree.fun_locals in
  let fun_entry, callee_rgts = fun_beginning f locals in
  fill_graph { f with Rtltree.fun_entry = fun_entry; };
  Hashtbl.add functions f.Rtltree.fun_name fun_entry;
  let fun_exit = fun_ending f callee_rgts in
  graph := Label.M.add f.Rtltree.fun_exit fun_exit !graph;
  let fun_locals = !locals in
  let fun_body = !graph in
  graph := Label.M.empty;
  { Ertltree.fun_name; fun_formals; fun_locals; fun_entry; fun_body }


(* ------------------------------------------------------------------------------------ *)
(* Translation of a program                                                             *)
(* ------------------------------------------------------------------------------------ *)

let program { Rtltree.funs } = { Ertltree.funs = List.map deffun funs }

