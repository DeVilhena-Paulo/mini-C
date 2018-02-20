
(* ------------------------------------------------------------------------------------ *)
(* Life Time Analysis - Kildall's algorithm                                             *)
(*                                                                                      *)
(* In this module, we compute the living variables of each ERTL instruction following   *)
(* the definition bellow. It's the Kildall's algorithm.                                 *)
(*                                                                                      *)
(*   use(I) = { variables used by the instruction I }                                   *)
(*   def(I) = { variables definied by the instruction I }                               *)
(*                                                                                      *)
(*   in(I)  = use(I) U (out(I) \ def(I))                                                *)
(*   out(I) = U { in(I') | I' is a successor of I }                                     *)
(*                                                                                      *)
(* ------------------------------------------------------------------------------------ *)

open Ertltree


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
    

(* ------------------------------------------------------------------------------------ *)
(* Preprocessing of instructions before applying Kildall                                *)
(* ------------------------------------------------------------------------------------ *)

let liveness (cfg : Ertltree.cfg) =
  let table = Hashtbl.create 13 in
  Label.M.iter (fun l i -> Hashtbl.add table l (build_info i)) cfg;
  let update_pred l li =
    let aux l_succ =
      let next_li = Hashtbl.find table l_succ in
      next_li.pred <- Label.S.add l next_li.pred; in
    List.iter aux li.succ in
  Hashtbl.iter update_pred table;
  kildall table;
  Hashtbl.fold (fun l li set -> Label.M.add l li set) table (Label.M.empty)


(* ------------------------------------------------------------------------------------ *)
(* Translation: Ertltree -> Kildall                                                     *)
(* ------------------------------------------------------------------------------------ *)
         
type deffun = {
    fun_name : Ertltree.ident;
    fun_formals : int;
    fun_locals : Register.set;
    fun_entry : Ertltree.label;
    fun_body : live_info Label.map;
  }

            
type file = {
    funs : deffun list;
  }

          
let deffun ({ fun_name; fun_formals; fun_locals; fun_entry; fun_body } : Ertltree.deffun ) =
  let fun_body = liveness fun_body in
  { fun_name; fun_formals; fun_locals; fun_entry; fun_body }

  
let program ({ funs } : Ertltree.file) = { funs = List.map deffun funs }


(* ------------------------------------------------------------------------------------ *)
(* Print functions                                                                      *)
(* ------------------------------------------------------------------------------------ *)

open Format
open Pp

let print_set = Register.print_set

let print_live_info fmt li =
  fprintf fmt "in = {%a}; out = {%a}" print_set li.ins print_set li.outs


let visit f g entry =
  let visited = Hashtbl.create 97 in
  let rec visit l =
    if not (Hashtbl.mem visited l) then
      begin
        Hashtbl.add visited l ();
        try
          let i = Label.M.find l g in
          f l i;
          List.iter visit (i.succ)
        with Not_found ->
          failwith ("Label " ^ (l :> string) ^ " not found")
      end
  in
  visit entry

let print_graph fmt =
  visit (fun l i -> fprintf fmt "%a: %a@\n" Label.print l print_live_info i)

let print_deffun fmt f =
  fprintf fmt "%s(%d)@\n" f.fun_name f.fun_formals;
  fprintf fmt "  @[";
  fprintf fmt "entry : %a@\n" Label.print f.fun_entry;
  fprintf fmt "locals: @[%a@]@\n" Register.print_set f.fun_locals;
  print_graph fmt f.fun_body f.fun_entry;
  fprintf fmt "@]@."

let print_file fmt p =
  fprintf fmt "=== Live Info ============================================@\n";
  List.iter (print_deffun fmt) p.funs
