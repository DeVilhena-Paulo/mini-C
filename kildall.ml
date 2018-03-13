
(* ------------------------------------------------------------------------------------ *)
(* Life Time Analysis - Kildall's algorithm                                             *)
(*                                                                                      *)
(* In this module, we compute the living variables of each ERTL instruction following   *)
(* the definition bellow. It's the Kildall's algorithm.                                 *)
(*                                                                                      *)
(*   use(I) = { v | v is a variable used by the instruction I }                         *)
(*   def(I) = { v | v is a variable definied by the instruction I }                     *)
(*                                                                                      *)
(*   in(I)  = use(I) U (out(I) \ def(I))                                                *)
(*   out(I) = U { in(I') | I' is a successor of I }                                     *)
(*                                                                                      *)
(* ------------------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------------------ *)
(* Usefull information for each instruction                                             *)
(* ------------------------------------------------------------------------------------ *)

type live_info = {
           instr: Ertltree.instr;
            succ: Label.t list;    (* successors *)
    mutable pred: Label.set;       (* predecessors *)
            defs: Register.set;    (* definitions *)
            uses: Register.set;    (* utilizations *)
    mutable  ins: Register.set;    (* living variables in input *)
    mutable outs: Register.set;    (* living vatiables in output *)
  }

(* For each processed function, we store the output of Kildall's algorithm for a        *)
(* particular instruction: alloc_frame. By doing so, we allow tail calls to that        *)
(* function.                                                                            *)
let (functions : (Label.t, Register.set * Register.set) Hashtbl.t) = Hashtbl.create 13


let build_info instr =
  let succ = Ertltree.succ instr in
  let pred = Label.S.empty in
  let def_list, use_list = Ertltree.def_use instr in
  let defs, uses = Register.set_of_list def_list, Register.set_of_list use_list in
  let ins, outs =
    match instr with
    (* | Ertltree.Etail_call (_, _, l) -> Hashtbl.find functions l *)
    | _ -> Register.S.empty, Register.S.empty in
  { instr; succ; pred; defs; uses; ins; outs }


(* ------------------------------------------------------------------------------------ *)
(* Kildall's algorithm                                                                  *)
(* ------------------------------------------------------------------------------------ *)

let kildall table =
  let rec kildall' = function
    | [] -> ()
    | li :: ws ->
       let old_ins = li.ins in
       let aux outs l' =
         let li' = Hashtbl.find table l' in
         Register.S.union li'.ins outs in
       li.outs <- List.fold_left aux li.outs li.succ;
       li.ins <- Register.S.union li.uses (Register.S.diff li.outs li.defs);
       if Register.S.equal old_ins li.ins then kildall' ws
       else kildall' (Label.S.fold (fun l' ws' -> (Hashtbl.find table l') :: ws') li.pred ws) in
  kildall' (Hashtbl.fold (fun l li ws -> li :: ws) table [])
    

(* ------------------------------------------------------------------------------------ *)
(* Preprocessing of instructions before applying Kildall                                *)
(* ------------------------------------------------------------------------------------ *)

let liveness (cfg : Ertltree.cfg) =
  let table = Hashtbl.create 13 in
  Label.M.iter (fun l i -> Hashtbl.add table l (build_info i)) cfg;  (* Initialization *)
  let update_pred l li =
    let aux l_succ =
      let next_li = Hashtbl.find table l_succ in
      next_li.pred <- Label.S.add l next_li.pred in
    List.iter aux li.succ in
  Hashtbl.iter update_pred table;  (* Compute precedent instructions *)
  kildall table;
  Hashtbl.iter (fun l i ->
      match i.instr with
      | Ertltree.Ealloc_frame _ -> Hashtbl.add functions l (i.ins, i.ins)
      | _ -> ()) table;  (* Update `functions` for future tail cails *)
  Hashtbl.fold (fun l li set -> Label.M.add l li set) table (Label.M.empty)

