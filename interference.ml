
(* --------------------------------------------------------------------------------------- *)
(* Interference Graph                                                                      *)
(*                                                                                         *) 
(* The purpose of this step is to build a graph for each function of our program where the *)
(* vertices are registers (pseudo or not) and the arcs are of two kinds: interference,     *)
(* expressing that the two ending vertices can't be allocated in the same real register,   *)
(* and preference, expresssing that it would be prefereable to allocate the two ending     *)
(* vertices in the same real register, but not required.                                   *)
(* --------------------------------------------------------------------------------------- *)

type arc =
  | Pref of Register.t * Register.t
  | Intf of Register.t * Register.t


type arcs = {
    prefs: Register.set;
    intfs: Register.set
  }


type igraph = arcs Register.map


let build_arcs () = { prefs = Register.S.empty; intfs = Register.S.empty }

  
let update_intfs g v w =
  let { prefs; intfs } = if Register.M.mem v g then Register.M.find v g else build_arcs () in
  let prefs = Register.S.remove w prefs in
  let intfs = Register.S.add    w intfs in
  { prefs; intfs }


let update_prefs g v w =
  let { prefs; intfs } as v_arcs =
    if Register.M.mem v g then Register.M.find v g else build_arcs () in
  if Register.S.mem w intfs then v_arcs else { intfs; prefs = Register.S.add w prefs; }


let add arc g =
  match arc with
  | Pref (v, w) -> Register.M.add v (update_prefs g v w) g |> Register.M.add w (update_prefs g w v)
  | Intf (v, w) -> Register.M.add v (update_intfs g v w) g |> Register.M.add w (update_intfs g w v)


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
