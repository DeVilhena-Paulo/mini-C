
(* ------------------------------------------------------------------------------------ *)
(* Translation: ERTL -> LTL (Location Transfer Language)                                *)
(* ------------------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------------------ *)
(* Global variables                                                                     *)
(* ------------------------------------------------------------------------------------ *)

(* Control flow graph *)
let (graph : Ltltree.cfg ref) = ref Label.M.empty


let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l


let lookup c r = Register.M.find r c


type frame = {
  f_params: int; (* taille pour les paramètres + adresse retour *)
  f_locals: int; (* taille pour les variables locales *)
  }


(* ------------------------------------------------------------------------------------ *)
(* Translation of an instuction                                                         *)
(* ------------------------------------------------------------------------------------ *)

let move a b l = Ltltree.Embinop (Ops.Mmov, a, b, l)


let to_tmp1 op l = move op (Ltltree.Reg Register.tmp1) l

let to_tmp2 op l = move op (Ltltree.Reg Register.tmp2) l


let from_tmp1_to op l = move (Ltltree.Reg Register.tmp1) op l

let from_tmp2_to op l = move (Ltltree.Reg Register.tmp2) op l

                                           
let instr c frame = function
  | Ertltree.Egoto l               -> Ltltree.Egoto l
  | Ertltree.Ereturn               -> Ltltree.Ereturn
  | Ertltree.Econst (n, r, l)      -> Ltltree.Econst (n, lookup c r, l)
  | Ertltree.Epush_param (r, l)    -> Ltltree.Epush (lookup c r, l)
  | Ertltree.Ecall (id, m, l)      -> Ltltree.Ecall (id, l)
  | Ertltree.Etail_call (id, m, l) -> Ltltree.Etail_call id
  | Ertltree.Emunop (unop, r, l)   -> Ltltree.Emunop (unop, lookup c r, l)
                                    
  (* r2 <- d(r1) *)
  | Ertltree.Eload (r1, d, r2, l) ->
     let op1, op2 = lookup c r1, lookup c r2 in
     begin match op1, op2 with
     | Ltltree.Reg r1   , Ltltree.Reg r2    -> Ltltree.Eload (r1, d, r2, l)
     | Ltltree.Reg r1   , Ltltree.Spilled _ ->
        Ltltree.Eload (r1, d, Register.tmp1, generate (from_tmp1_to op2 l))
     | Ltltree.Spilled _, Ltltree.Reg r2    ->
        to_tmp1 op1 (generate (Ltltree.Eload (Register.tmp1, d, r2, l)))
     | Ltltree.Spilled _, Ltltree.Spilled _ ->
        let l1 = generate (from_tmp2_to op2 l) in
        let l2 = generate (Ltltree.Eload (Register.tmp1, d, Register.tmp2, l1)) in
        to_tmp1 op1 l2 end
     
  (* d(r1) <- r2 *)                                         
  | Ertltree.Estore (r2, r1, d, l) ->
     let op1, op2 = lookup c r1, lookup c r2 in
     begin match op1, op2 with
     | Ltltree.Reg r1   , Ltltree.Reg r2     -> Ltltree.Estore (r2, r1, d, l)
     | Ltltree.Reg r1   , Ltltree.Spilled _ ->
        to_tmp2 op2 (generate (Ltltree.Estore (Register.tmp2, r1, d, l)))
     | Ltltree.Spilled _, Ltltree.Reg r2    ->
        to_tmp1 op1 (generate (Ltltree.Estore (r2, Register.tmp1, d, l)))
     | Ltltree.Spilled _, Ltltree.Spilled _ ->
        let l' = generate (Ltltree.Estore (Register.tmp2, Register.tmp1, d, l)) in
        to_tmp1 op1 (generate (to_tmp2 op2 l')) end
     
  | Ertltree.Emubranch (br, r, lt, lf)      ->
     let op = lookup c r in
     begin match op with
     | Ltltree.Reg r'    -> Ltltree.Emubranch (br, r', lt, lf)
     | Ltltree.Spilled _ ->
        to_tmp1 op (generate (Ltltree.Emubranch (br, Register.tmp1, lt, lf))) end
     
  | Ertltree.Embbranch (br, r1, r2, lt, lf) ->
     let op1, op2 = lookup c r1, lookup c r2 in
     begin match op1, op2 with
     | Ltltree.Reg r1   , Ltltree.Reg r2    -> Ltltree.Embbranch (br, r1, r2, lt, lf)
     | Ltltree.Reg r1   , Ltltree.Spilled _ ->
        to_tmp2 op2 (generate (Ltltree.Embbranch (br, r1, Register.tmp2, lt, lf)))
     | Ltltree.Spilled _, Ltltree.Reg r2    ->
        to_tmp1 op1 (generate (Ltltree.Embbranch (br, Register.tmp1, r2, lt, lf)))
     | Ltltree.Spilled _, Ltltree.Spilled _ ->
        let l' = generate (Ltltree.Embbranch (br, Register.tmp1, Register.tmp2, lt, lf)) in
        to_tmp1 op1 (generate (to_tmp2 op2 l')) end
     
  | Ertltree.Embinop (Ops.Mmov, r1, r2, l) ->
     let op1, op2 = lookup c r1, lookup c r2 in
     begin match op1, op2 with
     | _, _ when op1 = op2 -> Ltltree.Egoto l
     | Ltltree.Spilled _, Ltltree.Spilled _ -> to_tmp1 op1 (generate (from_tmp1_to op2 l))
     | _, _ -> move op1 op2 l end
                                              
  | Ertltree.Embinop (binop, r1, r2, l)     ->
     let op1, op2 = lookup c r1, lookup c r2 in
     begin match op1, op2 with
     | Ltltree.Reg _    , Ltltree.Reg _
     | Ltltree.Reg _    , Ltltree.Spilled _
     | Ltltree.Spilled _, Ltltree.Reg _     -> Ltltree.Embinop (binop, op1, op2, l)
     | Ltltree.Spilled _, Ltltree.Spilled _ ->
        let tmp1 = Ltltree.Reg Register.tmp1 in
        to_tmp1 op1 (generate (Ltltree.Embinop (binop, tmp1, op2, l))) end
     
  | Ertltree.Eget_param (ofs, r, l)         ->
     begin match lookup c r with
     | Ltltree.Reg r' as op    -> move (Ltltree.Spilled ofs) op l
     | Ltltree.Spilled _ as op ->
        let tmp1 = Ltltree.Reg Register.tmp1 in
        move (Ltltree.Spilled ofs) tmp1 (generate (move tmp1 op l)) end
    
  | Ertltree.Ealloc_frame l ->
     let rsp, rbp = Ltltree.Reg Register.rsp, Ltltree.Reg Register.rbp in
     if frame.f_locals = 0 then Ltltree.Epush (rbp, l)
     else
       let ofs = Int32.of_int ((-8) * frame.f_locals) in
       let l1 = generate (Ltltree.Emunop (Ops.Maddi ofs, rsp, l)) in
       Ltltree.Epush (rbp, generate (Ltltree.Embinop (Ops.Mmov, rsp, rbp, l1)))
       
  | Ertltree.Edelete_frame l ->
     if frame.f_locals = 0 then Ltltree.Epop (Register.rbp, l)
     else
       let rsp, rbp = Ltltree.Reg Register.rsp, Ltltree.Reg Register.rbp in
       Ltltree.Embinop (Ops.Mmov, rbp, rsp, generate (Ltltree.Epop (Register.rbp, l)))


(* ------------------------------------------------------------------------------------ *)
(* Translation of a function                                                            *)
(* ------------------------------------------------------------------------------------ *)

(* Fill the graph with the translaton of each instruction *)
let fill_graph f c frame =
  let aux l i =
    let i' = instr c frame i in
    graph := Label.M.add l i' !graph; in
  begin Label.M.iter aux f.Ertltree.fun_body end


let deffun f = 
  let c, n_spilled =
    Kildall.liveness f.Ertltree.fun_body |> Interference.make |> Coloring.color in
  let f_params, f_locals = (1 + (max 0 (f.Ertltree.fun_formals - 6))), n_spilled in
  fill_graph f c { f_params; f_locals };
  let fun_name = f.Ertltree.fun_name in
  let fun_entry = f.Ertltree.fun_entry in
  let fun_body = !graph in
  graph := Label.M.empty;
  { Ltltree.fun_name; fun_entry; fun_body }
             

(* ------------------------------------------------------------------------------------ *)
(* Translation of a program                                                             *)
(* ------------------------------------------------------------------------------------ *)

let program { Ertltree.funs } = { Ltltree.funs = List.map deffun funs }
