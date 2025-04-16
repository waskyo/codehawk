open CCHBasicTypes

let binop_to_str_ricardo (b: binop) =
  match b with
  | PlusA -> "+"
  | PlusPI -> "+"
  | IndexPI -> "index_pi"
  | MinusA -> "-"
  | MinusPI -> "-"
  | MinusPP -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Shiftlt -> "<<"
  | Shiftrt -> ">>"
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | BAnd -> "&"
  | BXor -> "^"
  | BOr -> "|"
  | LAnd -> "&&"
  | LOr -> "||"

let unop_to_str_ricardo (u: unop) =
  match u with
  | Neg -> "neg"
  | BNot -> "bnot"
  | LNot -> "lnot"

let type_to_str_ricardo (t: typ) =
  match t with
  | TVoid _ -> "(void)"
  | TInt (_, _) -> "(int)"
  | TFloat (_, _) -> "(float)"
  | TPtr (_, _) -> "(ptr)"
  | TArray (_, _, _) -> "(array)"
  | TFun (_, _, _, _) -> "(func)"
  | TNamed (_, _) -> "(named)"
  | TComp (_, _) -> "(comp)"
  | TEnum (_, _) -> "(enum)"
  | TBuiltin_va_list _ -> "(va_list)"

let const_to_str_ricardo (c: constant) =
  match c with
  | CInt (i_64, _, opt) -> (Int64.to_string i_64) ^ " opt: " ^ (match opt with | None -> "none" | Some s_opt -> s_opt)
  | CStr _ -> "str const"
  | CWStr _ -> "wstr const"
  | CChr _ -> "chr const"
  | CReal (_, _, _) -> "real const"
  | CEnum (_, _, _) -> "enum const"

let rec exp_to_str_ricardo (x: exp) =
  (match x with
  | Const c -> "Const(" ^ (const_to_str_ricardo c) ^ ")"
  | Lval l -> "Lval(" ^ (lval_to_str_ricardo l) ^ ")"
  | SizeOf t -> "SizeOf(" ^ (type_to_str_ricardo t) ^ ")"
  | SizeOfE s -> "SizeOfE(" ^ (exp_to_str_ricardo s) ^ ")"
  | SizeOfStr s -> "SizeofStr(" ^ s ^ ")"
  | AlignOf t -> "AlignOf(" ^ (type_to_str_ricardo t) ^ ")"
  | AlignOfE e -> "AlignOfE(" ^ (exp_to_str_ricardo e) ^ ")"
  | UnOp (u, two, t2) -> "UnOp(, " ^ (unop_to_str_ricardo u) ^ ", " ^ (exp_to_str_ricardo two) ^ (type_to_str_ricardo t2) ^ ")"
  | BinOp (a, b, c, d) -> "BinOp(" ^ (binop_to_str_ricardo a) ^ ", " ^ (exp_to_str_ricardo b) ^ ", " ^ (exp_to_str_ricardo c) ^ ", " ^ (type_to_str_ricardo d) ^ ")"
  | Question (_, _, _, _) -> "Question"
  | CastE (t, e) -> "CastE(" ^ (type_to_str_ricardo t) ^ ", " ^ (exp_to_str_ricardo e) ^ ")"
  | AddrOf _ -> "AddrOf"
  | AddrOfLabel _ -> "AddrOfLabel"
  | StartOf _ -> "StartOf"
  | FnApp (_, _, _) -> "FnApp"
  | CnApp (_, _, _) -> "CnApp")

and varuse_to_str_ricardo (v: varuse) = match v with | (vname, vid) -> vname ^ ", " ^ (Int.to_string vid)

and lhost_to_str_ricardo (host: lhost) =
  match host with
  | Var vu -> "Var Host: " ^ (varuse_to_str_ricardo vu)
  | Mem me -> "Mem Host: " ^ (exp_to_str_ricardo me)

and offset_to_str_ricardo (off: offset) =
  match off with
  | NoOffset -> "no offset"
  | Field (_, _) -> "field"
  | Index (_, _) -> "index"

and lval_to_str_ricardo (l: lval) =
  match l with | (host, offset) -> (lhost_to_str_ricardo host) ^ ", " ^ (offset_to_str_ricardo offset)

