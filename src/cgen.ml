open Printf
open Ast
open Sast

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
    BoolLit(lit) -> string_of_bool lit
  | CharLit(lit) -> "'" ^ Char.escaped lit ^ "'"
  | NumLit(lit) -> string_of_int lit
  | StrLit(lit) -> "\"" ^ lit ^ "\""
  | Negof(exp) -> "(-" ^ string_of_expr exp ^ ")"
  | Notof(exp) -> "(!" ^ string_of_expr exp ^ ")"
  | Id(id) -> id
  | Array(id, indices) -> "(" ^ id ^ "[" ^ String.concat "][" (List.map string_of_expr indices) ^ "] )"
  | Binop(exp1, binop, exp2) -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ ")"
  | Assign(exp1, asnop, exp2) -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp2 ^ ")" 
  | Call(fid, param) -> (string_of_call fid param)
  | GraphLit(edges) -> "newGraph(" ^ string_of_int (List.length edges) ^ ", " ^ String.concat ", " (List.map string_of_edge edges) ^ ")"
  | MultiId(obj, dot, field) -> if (String.compare field "size()" == 0) then ("(int)(" ^ string_of_expr obj ^ "." ^ field ^ ")") else ("(" ^ string_of_expr obj ^ "." ^ field ^ ")")
  | Null -> "NULL"
  | Noexpr -> ""

and string_of_call fid param = fid ^ "(" ^ String.concat ", " (List.map string_of_expr param) ^ " )"

and string_of_edge edge =
  "new edge_decl(\"" ^ edge.src ^ "\", \"" ^ edge.dest ^ "\", " ^ string_of_expr edge.weight ^ ")"


let string_of_ptypes = function
    Int -> "int"
  | Char -> "char"
  | Str -> "string"
  | Graph -> "graph"
  | Node -> "node"
  | Edge -> "edge"
  | Bool -> "bool"
  | Void -> "void"
  | _ -> "unknown_type"

let rec string_of_var_type s_ptype dimensions =
  if dimensions == 0 then
    string_of_ptypes s_ptype
  else
    "vector < " ^ (string_of_var_type s_ptype (dimensions-1)) ^ " >"

let rec list_of_array_init list_dim acc_for_loop acc_resize_exp =
  match list_dim with
    [] -> []
  | hd :: tl -> let loop_var = "i" ^ string_of_int (List.length list_dim) in
      (acc_for_loop ^ acc_resize_exp ^ ".resize(" ^ (string_of_expr hd.exp) ^ ");\n") ::
      list_of_array_init
        tl
        (acc_for_loop ^ "for(int " ^ loop_var ^ " = 0; " ^ loop_var ^ " < " ^ (string_of_expr hd.exp) ^ "; " ^ loop_var ^ "++)\n")
        (acc_resize_exp ^ "[" ^ loop_var ^ "]")

let string_of_array_init list_dim id=
  String.concat "\n" (list_of_array_init list_dim "" id)

let string_of_global_var_decl vardecl =
  let str_decl = (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) ^ " " ^ vardecl.s_vname in
    if vardecl.s_vinit.exp == Noexpr then
      ((str_decl ^ ";\n"), (string_of_array_init (List.rev vardecl.s_vtype.s_dimension) vardecl.s_vname))
    else
      (str_decl ^ " = " ^ string_of_expr vardecl.s_vinit.exp ^ ";", "")

let string_of_var_decl vardecl =
  let str_decl = (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) ^ " " ^ vardecl.s_vname in
    if vardecl.s_vinit.exp == Noexpr then
      str_decl ^ ";\n" ^ (string_of_array_init (List.rev vardecl.s_vtype.s_dimension) vardecl.s_vname)
    else
      str_decl ^ " = " ^ string_of_expr vardecl.s_vinit.exp ^ ";"

let string_of_param_var_decl vardecl =
  if (List.length vardecl.s_vtype.s_dimension) > 0 || (vardecl.s_vtype.s_ptype == Graph || vardecl.s_vtype.s_ptype == Node || vardecl.s_vtype.s_ptype == Edge || vardecl.s_vtype.s_ptype == Str) then
    "const " ^ (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) ^ " &_" ^ vardecl.s_vname
  else 
    (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) ^ " " ^ vardecl.s_vname

let rec string_of_stmt = function
    S_Block(stmt_list) -> "{\n" ^ (String.concat "\n" (List.map string_of_stmt stmt_list)) ^ "\n}\n"
  | S_Expr(expr) -> "(" ^ (string_of_expr expr.exp) ^ ");"
  | S_Return(expr) -> "return " ^ (string_of_expr expr.exp) ^ ";"
  | S_If(expr, stmt1, stmt2) -> "if (" ^ (string_of_expr expr.exp) ^ ")\n" ^ (string_of_stmt stmt1) ^ "\nelse " ^ (string_of_stmt stmt2)
  | S_For(init, test, after, stmt) -> "for (" ^ string_of_expr init.exp ^ "; " ^ string_of_expr test.exp ^ "; " ^ string_of_expr after.exp ^ ") " ^ string_of_stmt stmt
  | S_While(test, stmt) -> "while (" ^ (string_of_expr test.exp) ^ ") " ^ (string_of_stmt stmt)
  | S_Localvar(var) -> string_of_var_decl var

let string_of_func_decl_only funcdecl = string_of_var_type funcdecl.s_ret.s_ptype (List.length funcdecl.s_ret.s_dimension) ^ " " ^ funcdecl.s_fname ^ "(" 
  ^ (String.concat ", " (List.map string_of_param_var_decl funcdecl.s_formals)) ^ ");"

let string_of_func_decl_with_body global_arr_init funcdecl =
  let cast_away_const vardecl =  
    if (List.length vardecl.s_vtype.s_dimension) > 0 || (vardecl.s_vtype.s_ptype == Graph || vardecl.s_vtype.s_ptype == Node || vardecl.s_vtype.s_ptype == Edge || vardecl.s_vtype.s_ptype == Str) then
      let nonconst_ref_type = (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) in
        nonconst_ref_type ^ " &" ^ vardecl.s_vname ^ " = (" ^ nonconst_ref_type ^ " &)_" ^ vardecl.s_vname
    else
      ""
  in
  string_of_var_type funcdecl.s_ret.s_ptype (List.length funcdecl.s_ret.s_dimension) ^ " " ^ funcdecl.s_fname ^ "(" 
  ^ (String.concat ", " (List.map string_of_param_var_decl funcdecl.s_formals))
  ^ ") {\n" ^ (String.concat ";\n" (List.map cast_away_const
  funcdecl.s_formals)) ^ ";\n" ^ (if (String.compare funcdecl.s_fname "_main") == 0 then global_arr_init else "") ^ string_of_stmt funcdecl.s_body ^ "\n}"

let string_of_program prog =
  let global_var_tuple_list = (List.map string_of_global_var_decl prog.s_gdecls) in
  "#include <vector>\n#include \"libprint.h\"\n#include \"libstring.h\"\n#include \"libgraph.h\"\nusing namespace std;\n"
  ^ (String.concat ";\n" (List.map fst global_var_tuple_list)) ^ ";\n"
	^ (String.concat "\n" (List.map string_of_func_decl_only prog.s_fdecls)) ^ "\n"
  ^ (String.concat "\n" (List.map (fun fdecls -> string_of_func_decl_with_body (String.concat "\n" (List.map snd global_var_tuple_list)) fdecls) prog.s_fdecls)) ^ "\n"
  ^ "int main() { _main(); return 0;}"

let write_c_program filename program =
  let file = open_out filename in
    fprintf file "%s" (string_of_program program);
    close_out file
