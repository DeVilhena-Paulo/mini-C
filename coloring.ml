
type color = Ltltree.operand

           
type coloring = color Register.map


let degree g v =
  let { Interference.prefs; intfs } = Interference.find_arcs v g in
  (Register.S.cardinal prefs) + (Register.S.cardinal intfs)


let minimal g =  (* g must be non empty *)
  let v, _ = Register.M.choose g in
  Register.M.fold (fun w _ v -> if degree g v < degree g w then v else w) g v


let neighbors v g =
  let { Interference.prefs; intfs } = Register.M.find v g in Register.S.union prefs intfs


let george_criteria k g v1 v2 =
  let verify condition =
    let v1_neighbors = neighbors v1 g in
    let aux w acc = acc && (not (condition w) || Register.S.mem w v1_neighbors) in
    Register.S.fold aux (neighbors v2 g) true in
  if Register.is_hw v1
  then (not (Register.is_hw v2)) && (verify (fun w -> not (Register.is_hw w) || degree g w >= k))
  else verify (fun w -> Register.is_hw w  || degree g w >= k)

  
let find_pref_arc k g =
  let aux v1 { Interference.prefs; _ } = function
    | Some _ as arc -> arc
    | None ->
       let candidates = Register.S.filter (fun v2 -> george_criteria k g v1 v2) prefs in
       if Register.S.is_empty candidates then None
       else Some (v1, Register.S.choose candidates) in
  Register.M.fold aux g None

  
let remove v neighbors g =
  let elim { Interference.prefs; intfs } =
    let prefs = Register.S.remove v prefs in
    let intfs = Register.S.remove v intfs in
    { Interference.prefs ; intfs } in
  Register.S.fold (fun w g' -> Register.M.add w (elim (Register.M.find w g)) g') neighbors g

  
let erase v g =
  let neighbors = neighbors v g in
  remove v neighbors g |> Register.M.remove v

  
let erase_prefs v g =
  let { Interference.intfs; prefs } = Register.M.find v g in
  remove v prefs g |> Register.M.add v { Interference.intfs; prefs = Register.S.empty }

  
let fusion g v1 v2 =
  let module I = Interference in
  let { I.prefs = prefs1; intfs = intfs1 }, { I.prefs = prefs2; intfs = intfs2 } =
    Register.M.find v1 g, Register.M.find v2 g in
  let prefs = Register.S.remove v2 (Register.S.union prefs1 prefs2) in
  let intfs = Register.S.remove v2 (Register.S.union intfs1 intfs2) in
  Register.S.fold (fun w g' -> I.add (I.Pref (v2, w)) g') prefs g
  |> Register.S.fold (fun w g' -> I.add (I.Intf (v2, w)) g') intfs
  |> erase v1

  
let available_regs v c g =
  let used_regs =
    let aux w acc =
      match Register.M.find_opt w c with
      | Some (Ltltree.Reg r) -> Register.S.add r acc
      | Some _ | None -> acc in
    Register.S.fold aux (neighbors v g) Register.S.empty in
  Register.S.filter (fun r -> not (Register.S.mem r used_regs)) Register.allocatable
                          

let min_cost g = let v, _ = Register.M.choose g in v

let color_to_string = function
  | Ltltree.Reg r     -> "Reg " ^ (r :> string)
  | Ltltree.Spilled n -> "Spilled " ^ (string_of_int n)
               
  
let iter_reg_coalescing n k igraph =
  
  let rec simplify g =
    let g' =
      Register.M.filter (fun v { Interference.prefs; _ } ->
          (not (Register.is_hw v)) && (Register.S.is_empty prefs) && (degree g v < k)) g in
    if not (Register.M.is_empty g') then select g (minimal g') else coalesce g
    
  and coalesce g =
    match find_pref_arc k g with
    | Some (v1, v2) ->
       let v1, v2 = if Register.is_hw v1 then v2, v1 else v1, v2 in
       let c = simplify (fusion g v1 v2) in
       Register.M.add v1 (Register.M.find v2 c) c
    | None -> freeze g
    
  and freeze g =
    let g' = Register.M.filter (fun v _ -> not (Register.is_hw v)) g in
    if not (Register.M.is_empty g') && degree g' (minimal g') < k
    then simplify (erase_prefs (minimal g') g) else spill g

  and spill g =
    let g' = Register.M.filter (fun v _ -> not (Register.is_hw v)) g in
    if Register.M.is_empty g'
    then Register.M.fold (fun r _ c -> Register.M.add r (Ltltree.Reg r) c) g (Register.M.empty)
    else select g (min_cost g')

  and select g v =
    let c = simplify (erase v g) in
    let regs =
      if Register.is_hw v then Register.S.singleton v else available_regs v c g in
    if Register.S.is_empty regs
    then begin incr n; Register.M.add v (Ltltree.Spilled (-8 * !n)) c end
    else Register.M.add v (Ltltree.Reg (Register.S.choose regs)) c in

  simplify igraph


let check c igraph =
  let aux v { Interference.intfs; _ } =
    let v_color = Register.M.find v c in
    let aux' w =
      let w_color = Register.M.find w c in
      if v_color = w_color
      then Format.printf "%s and %s have the same color\n" (v :> string) (w :> string); in
    Register.S.iter aux' intfs; in
  begin Register.M.iter aux igraph end
      

let color igraph =
  let n = ref 0 in
  let c = (iter_reg_coalescing n (Register.k) igraph) in
  begin check c igraph; (c, !n) end


let print_color fmt = function
  | Ltltree.Reg hr    -> Format.fprintf fmt "%a" Register.print hr
  | Ltltree.Spilled n -> Format.fprintf fmt "stack %d" n


let print (cm, _) =
  Register.M.iter
    (fun r cr -> Format.printf "%a -> %a@\n" Register.print r print_color cr) cm


let print_file fmt p =
  Format.fprintf fmt "=== Register Allocation ==================================@\n";
  List.iter (fun f -> print (color (Interference.make f.Kildall.fun_body))) p.Kildall.funs;
  print_newline ()
