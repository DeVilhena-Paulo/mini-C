
open Ltltree

   
(* ------------------------------------------------------------------------------------ *)
(* Global variables                                                                     *)
(* ------------------------------------------------------------------------------------ *)

(* Control flow graph *)
let (graph : Ltltree.cfg ref) = ref Label.M.empty


let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l


let lookup c r =
  if Register.is_hw r then Ltltree.Reg r else Register.M.find r c


type frame = {
  f_params: int; (* taille pour les paramètres + adresse retour *)
  f_locals: int; (* taille pour les variables locales *)
  }


(* ------------------------------------------------------------------------------------ *)
(* Translation of an instuction                                                         *)
(* ------------------------------------------------------------------------------------ *)

let move a b l = Ltltree.Embinop (Ops.Mmov, a, b, l)


let to_stack r op l = move (Ltltree.Reg r) op l


let to_register op r l = move op (Ltltree.Reg r) l


let pattern_match ~rr ~rs ~sr ~ss c r1 r2 =
  let op1, op2 = lookup c r1, lookup c r2 in match op1, op2 with
  | Ltltree.Reg _    , Ltltree.Reg _     -> rr op1 op2
  | Ltltree.Reg _    , Ltltree.Spilled _ -> rs op1 op2
  | Ltltree.Spilled _, Ltltree.Reg _     -> sr op1 op2
  | Ltltree.Spilled _, Ltltree.Spilled _ -> ss op1 op2


let getr = function Ltltree.Reg r -> r | _ -> assert false

                                           
let instr c frame = function
  | Ertltree.Egoto l              -> Ltltree.Egoto l
  | Ertltree.Ereturn              -> Ltltree.Ereturn
  | Ertltree.Econst (n, r, l)     -> Ltltree.Econst (n, lookup c r, l)
  | Ertltree.Epush_param (r, l)   -> Ltltree.Epush (lookup c r, l)
  | Ertltree.Ecall (id, m, l)     -> Ltltree.Ecall (id, l)
  | Ertltree.Emunop (unop, r, l)  -> Ltltree.Emunop (unop, lookup c r, l)
  (* r2 <- d(r1) *)
  | Ertltree.Eload (r1, d, r2, l) ->
     let rr op1 op2 = Ltltree.Eload (getr op1, d, getr op2, l) in
     let rs op1 op2 =
       Ltltree.Eload (getr op1, d, Register.tmp1, generate (to_stack Register.tmp1 op2 l)) in
     let sr op1 op2 =
       to_register op1 Register.tmp1 (generate (Ltltree.Eload (Register.tmp1, d, getr op2, l))) in
     let ss op1 op2 =
       let l1 = generate (to_stack Register.tmp2 op2 l) in
       let l2 = generate (Ltltree.Eload (Register.tmp1, d, Register.tmp2, l1)) in
       to_register op1 Register.tmp1 l2 in
     pattern_match ~rr ~rs ~sr ~ss c r1 r2
  (* d(r1) <- r2 *)                                         
  | Ertltree.Estore (r2, r1, d, l) ->
     let rr op1 op2 = Ltltree.Estore (getr op2, getr op1, d, l) in
     let rs op1 op2 =
       to_register op2 Register.tmp1 (generate (Ltltree.Estore (Register.tmp1, getr op1, d, l))) in
     let sr op1 op2 =
       to_register op1 Register.tmp1 (generate (Ltltree.Estore (getr op2, Register.tmp1, d, l))) in
     let ss op1 op2 =
       let l' = generate (Ltltree.Estore (Register.tmp2, Register.tmp1, d, l)) in
       to_register op1 Register.tmp1 (generate (to_register op2 Register.tmp2 l')) in
     pattern_match ~rr ~rs ~sr ~ss c r1 r2
  | Ertltree.Emubranch (br, r, lt, lf)      ->
     let op = lookup c r in begin match op with
     | Ltltree.Reg r' -> Ltltree.Emubranch (br, r', lt, lf)
     | Ltltree.Spilled _ ->
        to_register op Register.tmp1 (generate (Ltltree.Emubranch (br, Register.tmp1, lt, lf)))
     end
  | Ertltree.Embbranch (br, r1, r2, lt, lf) ->
     let rr op1 op2 = Ltltree.Embbranch (br, getr op1, getr op2, lt, lf) in
     let rs op1 op2 =
       let l' = generate (Ltltree.Embbranch (br, getr op1, Register.tmp1, lt, lf)) in
       to_register op2 Register.tmp1 l' in
     let sr op1 op2 =
       let l' = generate (Ltltree.Embbranch (br, Register.tmp1, getr op2, lt, lf)) in
       to_register op1 Register.tmp1 l' in
     let ss op1 op2 =
        let l' = generate (Ltltree.Embbranch (br, Register.tmp1, Register.tmp2, lt, lf)) in
        to_register op1 Register.tmp1 (generate (to_register op2 Register.tmp2 l')) in
     pattern_match ~rr ~rs ~sr ~ss c r1 r2
  | Ertltree.Embinop (Ops.Mmov, r1, r2, l) ->
     let op1, op2 = lookup c r1, lookup c r2 in begin match op1, op2 with
     | _, _ when op1 = op2 -> Ltltree.Egoto l
     | Ltltree.Spilled _, Ltltree.Spilled _ ->
        to_register op1 Register.tmp1 (generate (to_stack Register.tmp1 op2 l))
     | _, _ -> move op1 op2 l end
  | Ertltree.Embinop (binop, r1, r2, l)     ->
     let rr op1 op2 = Ltltree.Embinop (binop, op1, op2, l) in
     let ss op1 op2 =
       let tmp1 = Register.tmp1 in
       to_register op1 tmp1 (generate (Ltltree.Embinop (binop, Ltltree.Reg tmp1, op2, l))) in
     pattern_match ~rr ~rs:rr ~sr:rr ~ss c r1 r2
  | Ertltree.Eget_param (ofs, r, l)         ->
     let op = lookup c r in begin match op with
     | Ltltree.Reg r'     -> move (Ltltree.Spilled ofs) op l
     | Ltltree.Spilled _ ->
        let tmp1 = Ltltree.Reg Register.tmp1 in
        move (Ltltree.Spilled ofs) tmp1 (generate (move tmp1 op l))
     end
  | Ertltree.Ealloc_frame l ->
     let rsp, rbp = Ltltree.Reg Register.rsp, Ltltree.Reg Register.rbp in
     let l2 =
       if frame.f_locals = 0 then l
       else
         let ofs = Int32.of_int ((-8) * frame.f_locals) in
         let l1 = generate (Ltltree.Emunop (Ops.Maddi ofs, rsp, l)) in
         generate (Ltltree.Embinop (Ops.Mmov, rsp, rbp, l1)) in
     Ltltree.Epush (rbp, l2)
  | Ertltree.Edelete_frame l ->
     let rsp, rbp = Ltltree.Reg Register.rsp, Ltltree.Reg Register.rbp in
     let pop_instr = Ltltree.Epop (Register.rbp, l) in
     if frame.f_locals = 0 then pop_instr
     else Ltltree.Embinop (Ops.Mmov, rbp, rsp, generate (pop_instr))


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
  { Ltltree.
    fun_name; fun_entry; fun_body }
             

(* ------------------------------------------------------------------------------------ *)
(* Translation of a program                                                             *)
(* ------------------------------------------------------------------------------------ *)

let program { Ertltree.funs } = { Ltltree.funs = List.map deffun funs }
