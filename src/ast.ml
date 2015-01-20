type binop = In | Add | Sub | Mul | Div | Mod | Pow | Eq | Neq | Less | Leq | Greater | Geq
        | Or | And
type asnop = Sadd | Ssub | Smul | Sdiv | Smod | Asn
type resolve = Dot

type ptypes = Int | Char | Str | Graph | Node | Edge | Bool | Void | Err

type expr =
    BoolLit of bool
  | CharLit of char
  | NumLit of int
  | StrLit of string
  | Negof of expr
  | Notof of expr
  | Id of string
  | MultiId of expr * resolve * string
  | Array of string * expr list
  | Binop of expr * binop * expr
  | Assign of expr * asnop * expr
  | Call of string * expr list
  | GraphLit of edge list
  | Null
  | Noexpr

and var_type = {
  ptype: ptypes;
  dimension: expr list;
}

and var_decl = {
  vname: string;
  vtype: var_type;
  vinit: expr;
}

and node = string

and edge = {
    src: node;
    dest: node;
    weight: expr;
}

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Localvar of var_decl

type func_decl = {
  fname : string;
  formals : var_decl list;
  body : stmt;
  ret : var_type
}

type program = {
  gdecls : var_decl list;
  fdecls : func_decl list
}

exception Main_not_void


(* for printing the AST out *)

let string_of_binop = function
  In -> "in"
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| Pow -> "^"
| Eq -> "=="
| Neq -> "!="
| Less -> "<"
| Leq -> "<="
| Greater -> ">"
| Geq -> ">="
| Or -> "||"
| And -> "&&"

let string_of_asnop = function
  Sadd -> "+="
| Ssub -> "-="
| Smul -> "*="
| Sdiv -> "/="
| Smod -> "%="
| Asn -> "="

let rec string_of_expr = function
    BoolLit(lit) -> "BoolLit( " ^ string_of_bool lit ^ " )"
  | CharLit(lit) -> "CharLit( " ^ Char.escaped lit ^ " )"
  | NumLit(lit) -> "NumLit( " ^ string_of_int lit ^ " )"
  | StrLit(lit) -> "StrLit( " ^ lit ^ " )"
  | Negof(exp) -> "Negof( " ^ string_of_expr exp ^ " )"
  | Notof(exp) -> "Notof( " ^ string_of_expr exp ^ " )"
  | Id(id) -> "Id( " ^ id ^ " )"
  | Array(id, indices) -> "Array( " ^ id ^ ", indices: [" ^ String.concat "][" (List.map string_of_expr indices) ^ "] )" 
  | Binop(exp1, binop, exp2) -> "Binop( " ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ " )"
  | Assign(exp1, asnop, exp2) -> "Assign( " ^ string_of_expr exp1 ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp2 ^ " )" 
  | Call(fid, param) -> "Call( " ^ fid ^ ", " ^ String.concat "; " (List.map string_of_expr param) ^ " )"
  | GraphLit(edges) -> "GraphLit {\n" ^ String.concat "\n" (List.map string_of_edge edges) ^ "\n}"
  | MultiId(obj, dot, field) -> "MultiId( " ^ string_of_expr obj ^ "." ^ field
  | Null -> "Null"
  | Noexpr -> "Noexpr"

and string_of_edge edge =
  edge.src ^ " -> " ^ edge.dest ^ " weight = " ^ string_of_expr edge.weight

let string_of_ptypes = function
    Int -> "int"
  | Char -> "char"
  | Str -> "str"
  | Graph -> "graph"
  | Node -> "node"
  | Edge -> "edge"
  | Bool -> "bool"
  | Void -> "void"
  | _ -> "unknown_type"

let string_of_var_type vartype = "ptype: " ^ string_of_ptypes vartype.ptype ^ "; dimension_length: " ^ string_of_int (List.length vartype.dimension)

let string_of_var_decl vardecl = "Var( name: " ^ vardecl.vname ^ "; " ^ string_of_var_type vardecl.vtype ^ "; " ^ string_of_expr vardecl.vinit ^ " )"

let rec string_of_stmt = function
    Block(stmt_list) -> (String.concat "\n" (List.map string_of_stmt stmt_list)) ^ "\n"
  | Expr(expr) -> "Expr( " ^ (string_of_expr expr) ^ " )"
  | Return(expr) -> "Return( " ^ (string_of_expr expr) ^ ")"
  | If(expr, stmt1, stmt2) -> "if (" ^ (string_of_expr expr) ^ ")\n" ^ (string_of_stmt stmt1) ^ (string_of_stmt stmt2) ^ ")"
  | For(init, test, after, stmt) -> "for (" ^ string_of_expr init ^ ", " ^ string_of_expr test ^ ", " ^ string_of_expr after ^ ") {\n" ^ string_of_stmt stmt ^ "\n}"
  | While(test, stmt) -> "while( " ^ (string_of_expr test) ^ " ) {\n" ^ (string_of_stmt stmt) ^ "\n}"
  | Localvar(var) -> "LocalVar( " ^ (string_of_var_decl var) ^ " )"

let string_of_func_decl funcdecl = "Function( type: (" ^ string_of_var_type funcdecl.ret ^ ") name: \"" ^ funcdecl.fname ^ "\" formals: " 
  ^ (String.concat ", " (List.map string_of_var_decl funcdecl.formals))
  ^ ") {\n" ^ string_of_stmt funcdecl.body ^ "\n}"

let string_of_program prog = "Program_START\n" ^ (String.concat "\n" (List.map string_of_var_decl prog.gdecls)) ^ "\n\n" ^
	(String.concat "\n\n" (List.map string_of_func_decl prog.fdecls)) ^ "\nProgram_END\n"
