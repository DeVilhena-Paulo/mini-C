
(* ------------------------------------------------------------------------------------ *)
(* Operation Selection x86-64                                                           *)
(* ------------------------------------------------------------------------------------ *)

open Ttree


(* ------------------------------------------------------------------------------------ *)
(* Inductive langage definition                                                         *)
(* ------------------------------------------------------------------------------------ *)

type ident = string
           
(* Expression *)
type expr =
  | Mconst  of int32
  | Maccess of ident
  | Massign of ident * expr
  | Mload   of int * expr
  | Mstore  of int * expr * expr
  | Mbinop  of mbinop * expr * expr
  | Munop   of munop * expr
  | Mcall   of ident * expr list

(* Unary operators *)
and munop =
  | Maddi   of int32
  | Msetei  of int32
  | Msetnei of int32

(* Binary operators *)
and mbinop =
  | Mmov
  | Madd
  | Msub
  | Mmul
  | Mdiv
  | Msete
  | Msetne
  | Msetl
  | Msetle
  | Msetg
  | Msetge


(* Unary branch operations *)
type mubranch =
  | Mjz
  | Mjnz
  | Mjlei of int32
  | Mjgi  of int32

(* Binary branch operations *)
type mbbranch =
  | Mjl
  | Mjle

(* Statement *)
type stmt =
  | Mskip
  | Mexpr   of expr
  | Mif     of expr * stmt * stmt
  | Mwhile  of expr * stmt
  | Mreturn of expr
  | Mblock  of ident list * stmt list  (* we remeber the variables with the same ident *)

(* Function definiton *)
type decl_fun = {
  fun_name   : ident;
  fun_formals: ident list;
  fun_locals : ident list;
  fun_body   : stmt list
}

(* Program *)
type file = {
  funs: decl_fun list;
}


(* ------------------------------------------------------------------------------------ *)
(* Translation: Ttree -> Ops                                                            *)
(* ------------------------------------------------------------------------------------ *)

module S = Set.Make(String)
         
let locals = ref S.empty

let zero = Int32.zero
let one = Int32.one


let rec pure = function
  | Mconst _  -> true
  | Maccess _ -> true
  | Massign _ -> false
  | Mload _   -> false
  | Mstore  _ -> false
  | Mbinop (_, e1, e2) -> (pure e1) && (pure e2)
  | Munop  (_, e)      -> pure e
  | Mcall _   -> false


(* ------------------------------------------------------------------------------------ *)
(* Smart constructors for unary operations                                              *)
(* ------------------------------------------------------------------------------------ *)

let mkNeg = function
  | Mconst i -> Mconst (Int32.neg i)
  | _ as e   -> Mbinop (Msub, Mconst zero, e)


let mkNot = function
  | Mconst i -> Mconst (if i = zero then one else zero)
  | _ as e   -> Munop (Msetei zero, e)


let mkUnop op e =
  match op with
  | Ptree.Uminus -> mkNeg e
  | Ptree.Unot   -> mkNot e
                  

(* ------------------------------------------------------------------------------------ *)
(* Smart constructors for binary operations                                             *)
(* ------------------------------------------------------------------------------------ *)
             
let rec mkAdd e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 -> Mconst (Int32.add i1 i2)
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     if i = zero then e else
       begin match e with
       | Munop ((Maddi i'), e') -> mkAdd (Mconst (Int32.add i i')) e'
       | _ -> Munop ((Maddi i), e)
       end
  | _ -> Mbinop (Madd, e1, e2)

          
let rec mkSub e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 -> Mconst (Int32.sub i1 i2)
  | _, Mconst i ->
     begin match e1 with
     | Munop ((Maddi i'), e') -> mkAdd e' (Mconst (Int32.sub i' i))
     | _ -> if i = zero then e1 else Munop ((Maddi (Int32.neg i)), e1)
     end
  | Mconst i, _ ->
     begin match e2 with
     | Munop ((Maddi i'), e') -> mkSub (Mconst (Int32.sub i i')) e'
     | _ -> if i = zero then mkNot e2 else Munop (Maddi i, mkNeg e2)
     end
  | _ -> Mbinop (Msub, e1, e2)

          
let rec mkMul e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 -> Mconst (Int32.mul i1 i2)
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     if i = zero && pure e then Mconst zero else
       if i = one then e else
         begin match e with
         | Munop ((Maddi i'), e') -> mkAdd (Mconst (Int32.mul i i')) (mkMul e' (Mconst i))
         | _ -> Mbinop (Mmul, e1, e2)
         end
  | _ -> Mbinop (Mmul, e1, e2)
 
          
let mkDiv e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 when i2 <> zero -> Mconst (Int32.div i1 i2)
  | _, Mconst i when i = one -> e1
  | _ -> Mbinop (Mdiv, e1, e2)


let mkAnd e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 ->
     let value = if i1 <> zero && i2 <> zero then one else zero in
     Mconst value
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     if i = zero then Mconst zero else Munop (Msetnei zero, e)
  | _ -> Munop (Msetnei zero, mkMul e1 e2)  (* Msetnei implies value equals to 0 or 1 *)


let mkOr e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 ->
     let value = if i1 <> zero || i2 <> zero then one else zero in
     Mconst value
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     if i <> zero then Mconst one else Munop (Msetnei zero, e)
  | _ -> mkNot (mkMul (mkNot e1) (mkNot e2))


let rec mkLt e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 ->
     let value = if i1 < i2 then one else zero in
     Mconst value
  (* With signed integers: i < e' + i' <=> i - i' < e' *)
  | Mconst i, Munop (Maddi i', e') -> mkLt (Mconst (Int32.sub i i')) e'
  | Munop (Maddi i', e'), Mconst i -> mkLt e' (Mconst (Int32.sub i i'))
  | _ -> Mbinop (Msetl, e1, e2)


let rec mkLe e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 ->
     let value = if i1 <= i2 then one else zero in
     Mconst value
  (* With signed integers: i <= e' + i' <=> i - i' <= e' *)
  | Mconst i, Munop (Maddi i', e') -> mkLe (Mconst (Int32.sub i i')) e'
  | Munop (Maddi i', e'), Mconst i -> mkLe e' (Mconst (Int32.sub i i'))
  | _ -> Mbinop (Msetle, e1, e2)


let rec mkSete e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 -> Mconst (if i1 = i2 then one else zero)
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     begin match e with
     | Munop (Maddi i', e') -> mkSete (Mconst (Int32.sub i i')) e'
     | _ -> Munop (Msetei i, e)
     end
  | _ -> Mbinop (Msete, e1, e2)


let rec mkSetne e1 e2 =
  match e1, e2 with
  | Mconst i1, Mconst i2 -> Mconst (if i1 <> i2 then one else zero)
  | Mconst i, (_ as e) | (_ as e), Mconst i ->
     begin match e with
     | Munop (Maddi i', e') -> mkSetne (Mconst (Int32.sub i i')) e'
     | _ -> Munop (Msetnei i, e)
     end
  | _ -> Mbinop (Msetne, e1, e2)


let rec mkBinop op e1 e2 =
  match op with
  | Ptree.Beq  -> mkSete  e1 e2
  | Ptree.Bneq -> mkSetne e1 e2

  | Ptree.Blt  -> mkLt    e1 e2
  | Ptree.Ble  -> mkLe    e1 e2
  | Ptree.Bgt  -> mkLt    e2 e1
  | Ptree.Bge  -> mkLe    e2 e1

  | Ptree.Badd -> mkAdd   e1 e2
  | Ptree.Bsub -> mkSub   e1 e2
  | Ptree.Bmul -> mkMul   e1 e2
  | Ptree.Bdiv -> mkDiv   e1 e2

  | Ptree.Band -> mkAnd   e1 e2
  | Ptree.Bor  -> mkOr    e1 e2


(* ------------------------------------------------------------------------------------ *)
(* Operation selection for expressions                                                  *)
(* ------------------------------------------------------------------------------------ *)

let rec op_expr { expr_node; expr_typ } =
  match expr_node with
  | Ttree.Econst i                  -> Mconst i
  | Ttree.Eaccess_local id          -> Maccess id
  | Ttree.Eaccess_field (e, f)      -> Mload (f.field_pos, op_expr e)
  | Ttree.Eassign_local (id, e)     -> Massign (id, op_expr e)
  | Ttree.Eassign_field (e1, f, e2) -> Mstore (f.field_pos, op_expr e1, op_expr e2)
  | Ttree.Eunop (op, e)             -> mkUnop op (op_expr e)
  | Ttree.Ebinop (op, e1, e2)       -> mkBinop op (op_expr e1) (op_expr e2)        
  | Ttree.Ecall (id, expr_list)     -> Mcall (id, List.map op_expr expr_list)
  | Ttree.Esizeof s                 -> Mconst (Int32.of_int s.str_size)


(* ------------------------------------------------------------------------------------ *)
(* Operation selection for statements                                                   *)
(* ------------------------------------------------------------------------------------ *)

let rec op_stmt = function
  | Ttree.Sskip           -> Mskip
  | Ttree.Sexpr e         -> Mexpr (op_expr e)
  | Ttree.Sif (e, s1, s2) -> Mif (op_expr e, op_stmt s1, op_stmt s2)
  | Ttree.Swhile (e, s)   -> Mwhile (op_expr e, op_stmt s)
  | Ttree.Sblock (decl_list, stmt_list) ->
     let repeated_ids =
       let aux acc (_, id) = if S.mem id !locals then (id :: acc) else acc in
       List.fold_left aux [] decl_list in
     locals := List.fold_left (fun set (_, id) -> S.add id set) !locals decl_list;
     Mblock (repeated_ids, List.map op_stmt stmt_list)
  | Ttree.Sreturn e -> Mreturn (op_expr e)


(* ------------------------------------------------------------------------------------ *)
(* Operation selection for functions                                                    *)
(* ------------------------------------------------------------------------------------ *)
                     
let op_fun { fun_typ; fun_name; fun_formals; fun_body } =
  let fun_formals = List.map (fun (_, id) -> id) fun_formals in
  let fun_body = match op_stmt (Ttree.Sblock fun_body) with
    | Mblock (_, s_list) -> s_list
    | _ -> assert false in
  let fun_locals = S.elements !locals in
  locals := S.empty;
  { fun_name; fun_formals; fun_locals; fun_body }


(* ------------------------------------------------------------------------------------ *)
(* Operation selection of a program                                                     *)
(* ------------------------------------------------------------------------------------ *)

let program ({ funs } : Ttree.file) = { funs = List.map op_fun funs }

                                    
(* ------------------------------------------------------------------------------------ *)
(* Print functions                                                                      *)
(* ------------------------------------------------------------------------------------ *)

open Format

   
let print_munop fmt = function
  | Maddi   i -> fprintf fmt "add $%ld"   i
  | Msetei  i -> fprintf fmt "sete $%ld"  i
  | Msetnei i -> fprintf fmt "setne $%ld" i

               
let print_mbinop fmt = function
  | Mmov   -> fprintf fmt "mov"
  | Madd   -> fprintf fmt "add"
  | Msub   -> fprintf fmt "sub"
  | Mmul   -> fprintf fmt "imul"
  | Mdiv   -> fprintf fmt "idiv"
  | Msete  -> fprintf fmt "sete"
  | Msetne -> fprintf fmt "setne"
  | Msetl  -> fprintf fmt "setl"
  | Msetle -> fprintf fmt "setle"
  | Msetg  -> fprintf fmt "setg"
  | Msetge -> fprintf fmt "setge"

            
let print_mubranch fmt = function
  | Mjz     -> fprintf fmt "jz"
  | Mjnz    -> fprintf fmt "jnz"
  | Mjlei n -> fprintf fmt "jle $%ld" n
  | Mjgi  n -> fprintf fmt "jg $%ld"  n

             
let print_mbbranch fmt = function
  | Mjl  -> fprintf fmt "jl"
  | Mjle -> fprintf fmt "jle"

