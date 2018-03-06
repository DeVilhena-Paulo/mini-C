

type arc =
  | Pref of Register.t * Register.t
  | Intf of Register.t * Register.t


type arcs = {
    prefs: Register.set;
    intfs: Register.set
  }


type igraph = arcs Register.map


let build_arcs () =
  { prefs = Register.S.empty; intfs = Register.S.empty }

  
let find_arcs v g =
  if Register.M.mem v g then Register.M.find v g else build_arcs ()

    
let update_intfs v w { prefs; intfs } =
  let prefs = Register.S.remove w prefs in
  let intfs = Register.S.add    w intfs in
  { prefs; intfs }


let update_prefs v w v_arcs =
  if Register.S.mem w v_arcs.intfs then v_arcs
  else { v_arcs with prefs = Register.S.add w v_arcs.prefs; }


let update g = function
  | Pref (v, w) -> Register.M.add v (update_prefs v w (find_arcs v g)) g
  | Intf (v, w) -> Register.M.add v (update_intfs v w (find_arcs v g)) g


let add arc g =
  let invert = function Pref (v, w) -> Pref (w, v) | Intf (v, w) -> Intf (w, v) in
  update (update g arc) (invert arc)


let add_intfs defs outs g =
  let aux v g' =
    Register.S.fold (fun w g'' -> add (Intf (v, w)) g'') (Register.S.remove v outs) g' in
  Register.S.fold aux defs g


let increment g { Kildall.instr; defs; outs; _ } =
  match instr with
  | Ertltree.Embinop (Ops.Mmov, w, _, _) ->
     Register.S.fold (fun v g' -> add (Pref (v, w)) g') defs g
     |> add_intfs defs (Register.S.remove w outs)
  | _ -> add_intfs defs outs g


let make li_map =
  Label.M.fold (fun _ li g -> increment g li) li_map Register.M.empty


let print ig =
  Register.M.iter (fun r arcs ->
    Format.printf "%4s: prefs=@[%a@] intfs=@[%a@]@." (r :> string)
      Register.print_set arcs.prefs Register.print_set arcs.intfs) ig


let print_file fmt p =
  Format.fprintf fmt "=== Interference Graph ===================================@\n";
  List.iter (fun f -> print (make f.Kildall.fun_body)) p.Kildall.funs;
  print_newline ()
