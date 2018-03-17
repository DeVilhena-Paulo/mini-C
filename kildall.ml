
(* ------------------------------------------------------------------------------------ *)
(* Life Time Analysis - Kildall's algorithm                                             *)
(*                                                                                      *)
(* In this module, we compute the living variables for ERTL instruction following the   *)
(* definition bellow.                                                                   *)
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


let build_info instr =
  let succ = Ertltree.succ instr in
  let pred = Label.S.empty in
  let def_list, use_list = Ertltree.def_use instr in
  let defs, uses = Register.set_of_list def_list, Register.set_of_list use_list in
  let ins, outs = Register.S.empty, Register.S.empty in
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


let liveness (cfg : Ertltree.cfg) =

  (* Initialization *)
  let table = Hashtbl.create 13 in
  Label.M.iter (fun l instr -> Hashtbl.add table l (build_info instr)) cfg;

  (* Compute precedent instructions *)
  let update_pred l li =
    let aux l_succ =
      let next_li = Hashtbl.find table l_succ in
      next_li.pred <- Label.S.add l next_li.pred in
    List.iter aux li.succ in
  Hashtbl.iter update_pred table;

  kildall table; Hashtbl.fold (fun l li set -> Label.M.add l li set) table (Label.M.empty)

