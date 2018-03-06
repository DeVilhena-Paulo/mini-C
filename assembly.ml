
open X86_64

type instr = Code of text | Label of Label.t


let code = ref []

let visited = Hashtbl.create 17
         
let labels = Hashtbl.create 17


let emit l i = code := Code i :: Label l :: !code

let emit_wl i = code := Code i :: !code

let need_label l = Hashtbl.add labels l ()

let inject (r : Register.t) = register_of_string (r :> string)
       
let operand = function
  | Ltltree.Reg r     -> reg (inject r)
  | Ltltree.Spilled n -> ind ~ofs:n rbp


let linearize_function g fun_entry =
  let rec lin l =
    if not (Hashtbl.mem visited l)
    then begin Hashtbl.add visited l (); instr l (Label.M.find l g) end
    else begin need_label l; emit_wl (jmp (l :> string)) end

  and instr l i =
    let module Lt = Ltltree in
    
    let rec compare op1 op2 setcc =
      emit l (cmpq op1 op2);
      emit_wl (setcc (reg r11b));
      emit_wl (movsbq (reg r11b) (r11));
      emit_wl (movq (reg r11) op2)

    and branch r1 r2 (jcc, not_jcc) lt lf =
      let l1, l2, cond_jmp =
        begin match Hashtbl.mem visited lf, Hashtbl.mem visited lt with
        | true, false -> lt, lf, not_jcc (lf :> string)
        | _           -> lf, lt, jcc (lt :> string) end in
      need_label l2;
      emit l (cmpq r1 r2);
      emit_wl cond_jmp;
      lin l1;
      lin l2  (* or: if not (Hashtbl.mem visited l2) then lin l2 *)
 
    and instr' = function
      | Lt.Econst (n, op, l1) -> emit l (movq (imm32 n) (operand op)); lin l1
      | Lt.Ereturn -> emit l ret
      | Lt.Epush (op, l1) -> emit l (pushq (operand op)); lin l1
      | Lt.Epop (r, l1)   -> emit l (popq (inject r)); lin l1
      | Lt.Egoto l1 -> begin if not (Hashtbl.mem visited l1) then emit l nop end; lin l1

      (* Struct read and assign *)
      | Lt.Eload  (r1, ofs, r2, l1) ->
         emit l (movq (ind ~ofs (inject r1)) (reg (inject r2))); lin l1
      | Lt.Estore (r2, r1, ofs, l1) ->
         emit l (movq (reg (inject r2)) (ind ~ofs (inject r1))); lin l1
         
      (* Unary branch *)   
      | Lt.Emubranch (Ops.Mjz,     r, lt, lf) -> branch (imm 0)   (reg (inject r)) (jz, jnz) lt lf
      | Lt.Emubranch (Ops.Mjnz,    r, lt, lf) -> branch (imm 0)   (reg (inject r)) (jnz, jz) lt lf
      | Lt.Emubranch (Ops.Mjlei n, r, lt, lf) -> branch (imm32 n) (reg (inject r)) (jle, jg) lt lf
      | Lt.Emubranch (Ops.Mjgi n,  r, lt, lf) -> branch (imm32 n) (reg (inject r)) (jg, jle) lt lf
        
      (* Binary branch *)
      | Lt.Embbranch (Ops.Mjl, r1, r2, lt, lf) ->
         branch (reg (inject r1)) (reg (inject r2)) (jl, jge) lt lf
      | Lt.Embbranch (Ops.Mjle, r1, r2, lt, lf) ->
         branch (reg (inject r1)) (reg (inject r2)) (jle, jg) lt lf
        
      (* Unary operation *)
      | Lt.Emunop (Ops.Maddi n,   op, l1) -> emit l (addq (imm32 n) (operand op)); lin l1
      | Lt.Emunop (Ops.Msetei n,  op, l1) -> compare (imm32 n) (operand op) sete ; lin l1
      | Lt.Emunop (Ops.Msetnei n, op, l1) -> compare (imm32 n) (operand op) setne; lin l1

      (* Binary logical operation: [cmpq op1 op2; setcc] *)
      | Lt.Embinop (Ops.Msete , op1, op2, l1) -> compare (operand op1) (operand op2) sete ; lin l1
      | Lt.Embinop (Ops.Msetne, op1, op2, l1) -> compare (operand op1) (operand op2) setne; lin l1
      | Lt.Embinop (Ops.Msetl , op1, op2, l1) -> compare (operand op1) (operand op2) setl ; lin l1
      | Lt.Embinop (Ops.Msetle, op1, op2, l1) -> compare (operand op1) (operand op2) setle; lin l1
      | Lt.Embinop (Ops.Msetg , op1, op2, l1) -> compare (operand op1) (operand op2) setg ; lin l1
      | Lt.Embinop (Ops.Msetge, op1, op2, l1) -> compare (operand op1) (operand op2) setge; lin l1
                                          
      (* Binary operation [ op2 <-  op2 OP op1 ]*)
      | Lt.Embinop (Ops.Mmov, op1, op2, l1) -> emit l (movq  (operand op1) (operand op2)); lin l1
      | Lt.Embinop (Ops.Madd, op1, op2, l1) -> emit l (addq  (operand op1) (operand op2)); lin l1
      | Lt.Embinop (Ops.Msub, op1, op2, l1) -> emit l (subq  (operand op1) (operand op2)); lin l1
      | Lt.Embinop (Ops.Mmul, op1, Lt.Reg r, l1) ->
         emit l (imulq (operand op1) (reg (inject r))); lin l1
      | Lt.Embinop (Ops.Mmul, op1, op2, l1) ->  (* op2 is necessarilly Spilled _  *)
         emit l (movq (operand op2) (reg r11));
         emit_wl (imulq (operand op1) (reg r11));
         emit_wl (movq (reg r11) (operand op2)); lin l1
      | Lt.Embinop (Ops.Mdiv, op1, op2, l1) ->
         emit l (movq (reg rdx) (reg r11));
         emit_wl cqto;
         emit_wl (idivq (operand op1));
         emit_wl (movq (reg r11) (reg rdx)); lin l1

      (* Function call *)
      | Lt.Ecall (id, l1) ->
         emit l (call ((if id = "putchar" || id = "sbrk" then id ^ "@PLT" else id) :> string));
         lin l1 in

    instr' i in

  lin fun_entry


let concat text_list =
  List.fold_left (fun acc t -> t ++ acc) nop text_list


let reset () =
  begin code := []; Hashtbl.reset visited; Hashtbl.reset labels end

  
let deffun { Ltltree.fun_name; fun_entry; fun_body } =
  reset (); linearize_function fun_body fun_entry; !code
  |> List.filter (function Label l -> Hashtbl.mem labels l | _ -> true)
  |> List.map (function Label l -> label (l :> string) | Code i -> i)
  |> concat
  |> (fun c -> (inline ((fun_name :> string) ^ ":\n")) ++ c)
  
                     
let program { Ltltree.funs } =
  { text = (globl "main") ++ (concat (List.map deffun funs)); data = nop }

