open Ast

module StringMap = Map.Make(String)

exception MultiId_err
exception Dup_var_id
exception Return_type_err
exception Init_type_err
exception Func_param_err
exception Func_duplicate
exception No_var
exception Arr_err
exception No_func
exception Type_err
exception Var_type
exception Err_s_check_stmt_list
exception Err_s_check_stmt_if
exception Err_s_check_stmt_for
exception Err_s_check_stmt_while
exception Main_not_found
exception Current_not_found
exception Edge_not_int_type
exception Msg_error of string

type t_expr = { exp: expr; typ: s_var_type }

and s_var_type = {
    s_ptype: ptypes;
    s_dimension: t_expr list;
}

and s_var_decl = {
    s_vname: string;
    s_vtype: s_var_type;
    s_vinit: t_expr;
}

and s_node = string

and s_edge = {
    s_src: s_node;
    s_dest: s_node;
    s_weight: t_expr;
}

and s_stmt = 
      S_Block of s_stmt list 
    | S_Expr of t_expr
    | S_Return of t_expr
    | S_If of t_expr * s_stmt * s_stmt
    | S_For of t_expr * t_expr * t_expr * s_stmt
    | S_While of t_expr * s_stmt
    | S_Localvar of s_var_decl

and s_func_decl = {
    s_fname : string;
    s_formals : s_var_decl list;
    s_body : s_stmt;
    s_ret : s_var_type
}

and s_program = {
    s_gdecls : s_var_decl list;
    s_fdecls : s_func_decl list;
}





let rec string_of_s_stmt = function
    S_Block(stmt_list) -> (String.concat "\n" (List.map string_of_s_stmt stmt_list)) ^ "\n"
  | S_Expr(expr) -> "Expr( " ^ (string_of_s_expr expr) ^ " )"
  | S_Return(expr) -> "Return( " ^ (string_of_s_expr expr) ^ " )"
  | S_Localvar(var) -> "LocalVar( " ^ (string_of_s_var_decl var) ^ " )"
  | S_If(expr, stmt1, stmt2) -> "if (" ^ (string_of_s_expr expr) ^ ")\n" ^ (string_of_s_stmt stmt1) ^ (string_of_s_stmt stmt2) ^ " )"
  | S_For(init, test, after, stmt) -> "for (" ^ string_of_s_expr init ^ ", " ^ string_of_s_expr test ^ ", " ^ string_of_s_expr after ^ ") {\n" ^ string_of_s_stmt stmt ^ "\n}"
  | S_While(test, stmt) -> "while( " ^ (string_of_s_expr test) ^ " ) {\n" ^ (string_of_s_stmt stmt) ^ "\n}"

and string_of_s_expr expr = "T_Expr( " ^ (string_of_expr expr.exp) ^ " type : "
^ (string_of_s_var_type expr.typ) ^ " )\n"

and string_of_s_var_type vartype = "ptype: " ^ string_of_ptypes vartype.s_ptype
^ "; dimension_length: " ^ string_of_int (List.length vartype.s_dimension)

and string_of_s_var_decl vardecl = "S_Var( name: " ^ vardecl.s_vname ^ "; " ^
string_of_s_var_type vardecl.s_vtype ^ "; " ^ string_of_s_expr vardecl.s_vinit  ^ " )"

and string_of_s_func_decl funcdecl = "Function ( type: (" ^ string_of_s_var_type
funcdecl.s_ret ^ ") name: \"" ^ funcdecl.s_fname ^ "\" formals: " ^
(String.concat ", " (List.map string_of_s_var_decl funcdecl.s_formals)) ^ ")
{\n" ^ string_of_s_stmt funcdecl.s_body ^ "\n}"

let string_of_program prog = "Sast_Program_Start\n" ^ (String.concat "\n" (List.map
string_of_s_var_decl prog.s_gdecls)) ^ "\n\n" ^
    (String.concat "\n\n" (List.map string_of_s_func_decl prog.s_fdecls)) ^
    "\nProgram_END\n"





let rec type_of_var id v_context = 
            if StringMap.mem id v_context then 
                fst (StringMap.find id v_context)
            else
                raise No_var

let type_of_obj_field obj field = {
  s_ptype = Node;
  s_dimension = []
}

let rec type_of_expr f_context v_context exp = match exp with
     BoolLit(lit) -> { s_ptype = Bool; s_dimension = [] }
   | CharLit(lit) -> { s_ptype = Char; s_dimension = [] }
   | NumLit(lit) -> { s_ptype = Int; s_dimension = [] }
   | StrLit(lit) -> { s_ptype = Str; s_dimension = [] }
   | Binop(exp1, binop, exp2) -> 
           (match binop with
            In -> { s_ptype = Bool; s_dimension = [] } 
           | Add | Sub | Mul | Div | Pow-> 
                   let type1 = type_of_expr f_context v_context exp1 and 
                        type2 = type_of_expr f_context v_context exp2 in
                        if (type1.s_ptype == Int || type1.s_ptype == Char) &&
                           (type2.s_ptype == Int || type2.s_ptype == Char) &&
                           (List.length type1.s_dimension == 0) && 
                           (List.length type2.s_dimension == 0) then
                                   if type1.s_ptype == type2.s_ptype then
                                        type1
                                   else
                                        { s_ptype = Int; s_dimension = [] }
                        else
                            raise Type_err

           | Mod ->  let type1 = type_of_expr f_context v_context exp1 and 
                        type2 = type_of_expr f_context v_context exp2 in
                        if (type1.s_ptype == Int || type1.s_ptype == Char) && 
                           (type2.s_ptype == Int || type2.s_ptype == Char) &&
                           (List.length type1.s_dimension == 0) &&
                           (List.length type2.s_dimension == 0) then
                                if type1.s_ptype == type2.s_ptype then
                                    type1
                                else 
                                    { s_ptype = Int; s_dimension = [] }
                        else
                            raise Type_err
           | Eq | Neq ->  let type1 = type_of_expr f_context v_context exp1 and 
                              type2 = type_of_expr f_context v_context exp2 in
                              if type1.s_ptype == type2.s_ptype && 
                                (List.length type1.s_dimension) == 
                                (List.length type2.s_dimension) then
                                { s_ptype = Bool; s_dimension = [] }
                              else
                                raise Type_err

           | Less | Leq | Greater | Geq ->  let type1 = type_of_expr f_context v_context exp1 and 
                        type2 = type_of_expr f_context v_context exp2 in
                        if (type1.s_ptype == Int || type1.s_ptype == Char || type1.s_ptype == Str) &&
                           (type2.s_ptype == Int || type2.s_ptype == Char || type2.s_ptype == Str) &&
                           (List.length type1.s_dimension == 0) && 
                           (List.length type2.s_dimension == 0) then
                               if type1.s_ptype == type2.s_ptype then
                                    { s_ptype = Bool; s_dimension = [] }
                               else
                                   if type1.s_ptype == Str || type2.s_ptype ==
                                       Str
                                   then
                                        raise Type_err
                                   else 
                                       { s_ptype = Bool; s_dimension = [] }
                        else
                            raise Type_err

           | Or | And ->  let type1 = type_of_expr f_context v_context exp1 and 
                        type2 = type_of_expr f_context v_context exp2 in
                        if (type1.s_ptype == Bool) &&
                           (type2.s_ptype == Bool) &&
                           (List.length type1.s_dimension == 0) && 
                           (List.length type2.s_dimension == 0) then
                               { s_ptype = Bool; s_dimension = [] }
                        else
                            raise Type_err )
   | Assign(exp1, asnop, exp2) -> (match asnop with
        Asn ->  let type1 = type_of_expr f_context v_context exp1 and 
                    type2 = type_of_expr f_context v_context exp2 in
                    if type1.s_ptype == type2.s_ptype && 
                    List.length type1.s_dimension == 
                        List.length type2.s_dimension then
                        type1
                    else raise Type_err
      | Sadd | Ssub | Smul | Sdiv  ->
              let type1 = type_of_expr f_context v_context exp1 and 
                  type2 = type_of_expr f_context v_context exp2 in
                  if (type1.s_ptype == Int || type1.s_ptype == Char) &&
                     (type2.s_ptype == Int || type2.s_ptype == Char) &&
                     (List.length type1.s_dimension == 0) && 
                     (List.length type2.s_dimension == 0) then
                        type1
                  else
                        raise Type_err
      | Smod ->
             let type1 = type_of_expr f_context v_context exp1 and 
                  type2 = type_of_expr f_context v_context exp2 in
                  if (type1.s_ptype == Int || type1.s_ptype == Char) &&
                     (type2.s_ptype == Int || type2.s_ptype == Char) &&
                     (List.length type1.s_dimension == 0) && 
                     (List.length type2.s_dimension == 0) then
                        type1
                  else
                        raise Type_err )

   | Call(fid, param) -> 
           type_of_func_ret fid param f_context v_context 
   | GraphLit(edges) ->  
           ignore (List.map (fun x -> type_of_expr f_context v_context x.weight) edges); 
           { s_ptype = Graph; s_dimension = [] }
   | Null -> { s_ptype = Void; s_dimension = [] } (* is this correct ? *)
   | Noexpr -> { s_ptype = Void; s_dimension = [] } (* is this correct ? *)
   | Notof(exp) -> 
           let type1 = type_of_expr f_context v_context exp in
           if type1.s_ptype == Bool then type1
           else raise Type_err (* Error if exp is not Bool *)
   | Id(id) -> type_of_var id v_context
   | MultiId(obj, dot, field) -> 
           type_of_obj_field obj field (*Check this... *)
   | Array(id, indices) -> 
           type_of_array id indices f_context v_context (* Error if indices and dimension not matched *)
   | Negof(exp) ->
      let exp_type = (type_of_expr f_context v_context exp) in
        if (exp_type.s_ptype == Int || exp_type.s_ptype == Char) then exp_type
        else raise Type_err

and type_of_array id indices f_context v_context = 
    (if StringMap.mem id v_context then
        let a_type = fst (StringMap.find id v_context) in
        if List.length a_type.s_dimension >= List.length indices then
            let s_indices = List.map (fun x -> type_of_expr f_context v_context x) indices in
            if List.length 
            (List.filter (fun a -> (a.s_ptype == Int || a.s_ptype == Char)) s_indices) == 
                List.length s_indices then
                    let rec cut_dimension num d_list = match d_list with
                        hd::tl -> if (num > 0) then cut_dimension (num-1) tl
                                        else d_list
                        | [] -> []
                    in
                    { s_ptype = a_type.s_ptype; 
                    s_dimension = cut_dimension (List.length s_indices) a_type.s_dimension}  
            else
                raise Arr_err
        else
            raise Arr_err
    else
        raise Arr_err)

and type_of_func_ret fid param f_context v_context= 
    if (String.compare fid "_len" == 0) && ((List.length param) == 1) && (List.length (type_of_expr f_context v_context (List.hd param)).s_dimension) != 0 then
      {s_ptype = Int; s_dimension = []}
    else
    if StringMap.mem fid f_context then
        let s_param = List.map (fun x -> type_of_expr f_context v_context x) param in
        let rec param_check t_l1 t_l2 = (match t_l1, t_l2 with
                          [], [] -> true
                        | hd::tl, [] -> false
                        | [], hd::tl -> false
                        | h1::t1, h2::t2 -> 
                                if h1.s_ptype == h2.s_ptype &&
                                List.length h1.s_dimension == List.length h2.s_dimension then
                                    param_check t1 t2
                                else
                                    false) in
        snd (List.find (fun a -> param_check (fst a) s_param) (StringMap.find fid f_context))
    else
        raise No_func


let rec s_check_expr f_context v_context in_exp = match in_exp with
      Binop(exp1, binop, exp2) -> 
            {exp = Binop((s_check_expr f_context v_context exp1).exp, binop, (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp }
    | Assign(exp1, asnop, exp2) ->  
            {exp = Assign((s_check_expr f_context v_context exp1).exp, asnop, (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp }
    | Call(fid, param) -> 
          if (String.compare fid "_len" == 0) && List.length param == 1 && List.length (s_check_expr f_context v_context (List.hd param)).typ.s_dimension != 0 then 
              {exp = MultiId(List.hd param, Dot, "size()"); typ = {s_ptype = Int; s_dimension = []}}
          else
              {exp = Call(fid, List.map (fun a -> (s_check_expr f_context v_context a).exp) param); typ = (type_of_expr f_context v_context in_exp)}
    | Notof(exp) -> 
           {exp = Notof((s_check_expr f_context v_context exp).exp); typ = type_of_expr f_context v_context in_exp} 
    | MultiId(obj, dot, field) ->
            if (type_of_expr f_context v_context obj).s_ptype == Graph
            then
                {exp = MultiId((s_check_expr f_context v_context obj).exp, dot, "getNode(\"" ^ field ^ "\")"); typ = {s_ptype = Node; s_dimension = []}}
            else
                if (type_of_expr f_context v_context obj).s_ptype == Str &&
                String.compare field "size()" == 0
                then
                    {exp = MultiId((s_check_expr f_context v_context obj).exp, dot, field); typ = {s_ptype = Int; s_dimension = []}}
                else
                    raise MultiId_err
    | Array(id, indices) -> 
            {exp = Array(id, List.map (fun a -> (s_check_expr f_context v_context a).exp) indices); typ = type_of_expr f_context v_context in_exp}
    | Negof(exp) ->
            {exp = Notof((s_check_expr f_context v_context exp).exp); typ = type_of_expr f_context v_context in_exp} 
    | GraphLit(edges) ->
            let t_edges = List.map (fun x -> (s_check_expr f_context v_context x.weight).typ) edges in
            if (List.length (List.filter (fun a -> a.s_ptype != Int || List.length a.s_dimension != 0) t_edges)) != 0 then
                raise Edge_not_int_type
            else
                {exp = in_exp; typ = { s_ptype = Graph; s_dimension = [] }}
    | _ -> {exp = in_exp; typ = (type_of_expr f_context v_context in_exp)}

let s_check_var_type f_context v_context vtype = 
    let dimention_type_list = (List.map (fun expr -> type_of_expr f_context v_context expr) vtype.dimension) in
      if List.length (List.filter (fun a -> (a.s_ptype == Int)) dimention_type_list) == List.length dimention_type_list
      then
          {s_ptype = vtype.ptype; 
          s_dimension = List.map (fun expr -> s_check_expr f_context v_context expr) vtype.dimension }
      else (* Error dimension not int*)
          raise Var_type

let s_stmt_context_v f_context v_context level stmt = match stmt with
    Localvar(vdecl) ->
      let lhs = (s_check_var_type f_context v_context vdecl.vtype) and rhs = (type_of_expr f_context v_context vdecl.vinit) in
        if vdecl.vinit == Noexpr || (List.length lhs.s_dimension == List.length rhs.s_dimension && lhs.s_ptype == rhs.s_ptype) then
            if StringMap.mem vdecl.vname v_context then
                let p_level = snd (StringMap.find vdecl.vname v_context) in
                if p_level == level then
                    raise Dup_var_id
                else
                    StringMap.add vdecl.vname ((s_check_var_type f_context v_context vdecl.vtype), level) v_context 
            else
                StringMap.add vdecl.vname ((s_check_var_type f_context v_context vdecl.vtype), level) v_context 
        else
            raise Init_type_err
  | _ -> v_context

let s_check_var_decl f_context v_context vdecl =
    let lhs = (s_check_var_type f_context v_context vdecl.vtype) and rhs =
        (type_of_expr f_context v_context vdecl.vinit) in
    if vdecl.vinit == Noexpr || (List.length lhs.s_dimension == List.length rhs.s_dimension && lhs.s_ptype == rhs.s_ptype) then
      { s_vname = vdecl.vname; s_vtype = (s_check_var_type f_context v_context vdecl.vtype); 
      s_vinit = (s_check_expr f_context v_context vdecl.vinit) }
    else
        raise Init_type_err

let rec s_check_stmt_list context_list stmt_list = match context_list, stmt_list with
     [], [] -> []
   | context_hd::context_tl, stmt_hd::stmt_tl -> 
           (s_check_stmt (fst context_hd) (snd context_hd) stmt_hd)
        :: (s_check_stmt_list context_tl stmt_tl)
   | _, _ -> raise Err_s_check_stmt_list

and s_check_stmt f_context v_context level stmt =
  match stmt with
     If(expr, stmt1, stmt2) -> 
      let exp_type = type_of_expr f_context v_context expr in
       if (exp_type.s_ptype == Bool && exp_type.s_dimension == [])
       then
           S_If(s_check_expr f_context v_context expr, 
           s_check_stmt f_context v_context level stmt1, 
           s_check_stmt f_context v_context level stmt2) 
       else
           raise Err_s_check_stmt_if; (* Error need boolean expression in if *)
   | For(expr1, expr2, expr3, stmt) ->
           let expr2_t = type_of_expr f_context v_context expr2 in
        if expr2_t.s_ptype ==  Bool && List.length expr2_t.s_dimension == 0
        then
            S_For(s_check_expr f_context v_context expr1, 
            s_check_expr f_context v_context expr2, 
            s_check_expr f_context v_context expr3, 
            s_check_stmt f_context v_context level stmt)
        else
            raise Err_s_check_stmt_for; (* Error need boolean expression in for *)
   | While(expr, stmt) ->
           let expr_t = type_of_expr f_context v_context expr in
        if expr_t.s_ptype == Bool && expr_t.s_dimension == []
        then 
            S_While(s_check_expr f_context v_context expr, 
            s_check_stmt f_context v_context level stmt)
        else
            raise Err_s_check_stmt_while; (* Error need boolean expression in while *)
   | Expr(expr) -> 
           S_Expr(s_check_expr f_context v_context expr) 
   | Return(expr) ->  
           let t_exp = s_check_expr f_context v_context expr in
           if StringMap.mem "0current" f_context then
             let cur = StringMap.find "0current" f_context in
             if t_exp.typ.s_ptype == (snd (List.hd cur)).s_ptype && 
                  List.length t_exp.typ.s_dimension == List.length (snd (List.hd cur)).s_dimension then
                      S_Return(s_check_expr f_context v_context expr)
             else
                 raise Return_type_err

           else
               raise Current_not_found 
   | Localvar(vdecl) -> 
           S_Localvar(s_check_var_decl f_context v_context vdecl)
   | Block(stmt_list) ->
           let first(f,_,_) = f and second(_,s,_) = s and third (_,_,t) = t in
           S_Block(
            List.rev ( first
            	(
            	List.fold_left  
           
           (fun x y ->
           	(((s_check_stmt
           		(second x)
           		(third x)
                (level+1)
           	y) :: 
           	(first x)),
           	 (second x),
           	(s_stmt_context_v (second x) (third x) (level+1) y)))

           ([], f_context, v_context)
       	   stmt_list)

        ))

let s_check_func_decl f_context v_context fdecl =
    let s_formals_t = List.map (fun var_decl -> s_check_var_decl f_context v_context
    var_decl) fdecl.formals in
    let s_ret_t = s_check_var_type f_context v_context fdecl.ret in
    {s_fname = fdecl.fname;
     s_formals = s_formals_t;
     s_ret = s_ret_t;
     s_body = s_check_stmt (StringMap.add "0current" [(List.map (fun a ->
         a.s_vtype) s_formals_t,s_ret_t)] f_context) (List.fold_left (fun a l ->
             if StringMap.mem l.s_vname a && snd (StringMap.find l.s_vname a) == 1 then 
                 raise Dup_var_id
             else
                 StringMap.add l.s_vname (l.s_vtype, 1) a) v_context s_formals_t) 1 fdecl.body
    }

let rec s_check_func_decls f_context v_context func_decl_list = match func_decl_list with
     [] -> []
   | hd::tl ->  
      (s_check_func_decl f_context v_context hd) :: (s_check_func_decls f_context v_context tl)

let s_var_decl_to_var_map map s_vdecl = 
    if StringMap.mem s_vdecl.s_vname map then
        raise Dup_var_id
    else
        StringMap.add s_vdecl.s_vname (s_vdecl.s_vtype, 0) map

let is_s_var_type_equal t1 t2 = 
    if t1.s_ptype == t2.s_ptype && List.length t1.s_dimension == List.length t2.s_dimension 
    then
        true
    else
        false

let rec is_s_var_type_list_equal l1 l2 = match l1, l2 with
    [], [] -> true
   | [], hd::tl -> false
   | hd::tl, [] -> false
   | h1::t1, h2::t2 -> 
           if is_s_var_type_equal h1 h2 then
               is_s_var_type_list_equal t1 t2
           else
               false

(* for overloading functions *)
let func_decl_check_func_map s_var_type_list_list s_var_type_list =
    let check = List.map (fun l1 -> is_s_var_type_list_equal (fst l1) s_var_type_list)
    s_var_type_list_list in
    if List.length (List.filter (fun a -> a) check) != 0 then
        true
    else
        false

let func_decl_to_func_map map fdecl v_context =
    let f_s_var_type_list = List.map (fun a -> a.s_vtype) (List.map (fun a -> s_check_var_decl StringMap.empty v_context a) fdecl.formals) in 
    if StringMap.mem fdecl.fname map 
    then 
        if func_decl_check_func_map (StringMap.find fdecl.fname map) f_s_var_type_list 
        then
            raise Func_duplicate
        else
            StringMap.add fdecl.fname ((f_s_var_type_list, s_check_var_type StringMap.empty v_context fdecl.ret)::StringMap.find fdecl.fname map) map
    else
        StringMap.add fdecl.fname [(f_s_var_type_list, s_check_var_type StringMap.empty v_context fdecl.ret)] map

let s_check_program prog =
  let temp_s_gdecls = List.map (fun var_decl -> s_check_var_decl StringMap.empty StringMap.empty var_decl) prog.gdecls 
  and std_func = (let map = StringMap.empty in 
  let map = StringMap.add "_print" [([{s_ptype = Int; s_dimension = []}], {s_ptype = Void; s_dimension = []}); 
                                   ([{s_ptype = Char; s_dimension =  []}], {s_ptype = Void; s_dimension = []}); 
                                   ([{s_ptype = Str; s_dimension = []}], {s_ptype = Void; s_dimension = []});
                                   ([{s_ptype = Bool; s_dimension = []}], {s_ptype = Void; s_dimension = []});
                                   ([{s_ptype = Node; s_dimension = []}], {s_ptype = Void; s_dimension = []})] map in
  let map = StringMap.add "_len" [([{s_ptype = Str; s_dimension = []}], {s_ptype = Int; s_dimension = []})] map in
  let map = StringMap.add "_sort" [([{s_ptype = Int; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]}], {s_ptype = Void; s_dimension = []});
                                  ([{s_ptype = Char; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]}], {s_ptype = Void; s_dimension = []});
                                  ([{s_ptype = Str; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]}], {s_ptype = Void; s_dimension = []});
                                  ([{s_ptype = Edge; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]}], {s_ptype = Void; s_dimension = []})] map in 
  let map = StringMap.add "_empty" [([{s_ptype = Str; s_dimension = []}], {s_ptype = Bool; s_dimension = []})] map in
  let map = StringMap.add "_at" [([{s_ptype = Str; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Char; s_dimension = []})] map in
  let map = StringMap.add "_append" [([{s_ptype = Str; s_dimension = []}; {s_ptype = Str; s_dimension = []}], {s_ptype = Void; s_dimension = []}); ([{s_ptype = Str; s_dimension = []}; {s_ptype = Char; s_dimension = []}], {s_ptype = Void; s_dimension = []})] map in
  let map = StringMap.add "_getNode" [([{s_ptype = Graph; s_dimension = []}; {s_ptype = Str; s_dimension = []}], {s_ptype = Node; s_dimension = []});
                                     ([{s_ptype = Graph; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Node; s_dimension = []})] map in
  let map = StringMap.add "_getEdge" [([{s_ptype = Graph; s_dimension = []}; {s_ptype = Str; s_dimension = []}; {s_ptype = Str; s_dimension = []}], {s_ptype = Edge; s_dimension = []});
                                     ([{s_ptype = Graph; s_dimension = []}; {s_ptype = Node; s_dimension = []}; {s_ptype = Node; s_dimension = []}], {s_ptype = Edge; s_dimension = []})] map in
  let map = StringMap.add "_getAllEdges" [([{s_ptype = Graph; s_dimension = []}], {s_ptype = Edge; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]})] map in
  let map = StringMap.add "_getAllNodes" [([{s_ptype = Graph; s_dimension = []}], {s_ptype = Node; s_dimension = [{exp = NumLit(0); typ = {s_ptype = Int; s_dimension = []}}]})] map in
  let map = StringMap.add "_getWeight" [([{s_ptype = Edge; s_dimension = []}],{s_ptype = Int; s_dimension = []});
                                       ([{s_ptype = Graph; s_dimension = []}; {s_ptype = Node; s_dimension = []}; {s_ptype = Node; s_dimension = []}],{s_ptype = Int; s_dimension = []})] map in
  let map = StringMap.add "_getNodeCount" [([{s_ptype = Graph; s_dimension = []}], {s_ptype = Int; s_dimension = []})] map in
  let map = StringMap.add "_getEdgeCount" [([{s_ptype = Graph; s_dimension = []}], {s_ptype = Int; s_dimension = []})] map in
  let map = StringMap.add "_getID" [([{s_ptype = Node; s_dimension = []}], {s_ptype = Int; s_dimension = []})] map in
  let map = StringMap.add "_getSrc" [([{s_ptype = Edge; s_dimension = []}], {s_ptype = Node; s_dimension = []})] map in
  let map = StringMap.add "_getDst" [([{s_ptype = Edge; s_dimension = []}], {s_ptype = Node; s_dimension = []})] map in
  let map = StringMap.add "_getInNeighbours" [([{s_ptype = Graph; s_dimension = []}; {s_ptype = Node; s_dimension = []}],{s_ptype = Node; s_dimension = [{exp = NumLit(0); typ = {s_ptype=Int; s_dimension =[]}}]})] map in
  let map = StringMap.add "_getOutNeighbours" [([{s_ptype = Graph; s_dimension = []}; {s_ptype = Node; s_dimension = []}],{s_ptype = Node; s_dimension = [{exp = NumLit(0); typ = {s_ptype=Int; s_dimension =[]}}]})] map in
  StringMap.add "_substr" [([{s_ptype = Str; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Str; s_dimension = []})] map)
  in
{
  s_gdecls = temp_s_gdecls;
  s_fdecls =
    let v_context = List.fold_left s_var_decl_to_var_map StringMap.empty temp_s_gdecls in
    let f_context = List.fold_left (fun x y -> func_decl_to_func_map x y v_context) std_func prog.fdecls in
    if StringMap.mem "_main" f_context then s_check_func_decls f_context v_context prog.fdecls
    else raise Main_not_found
}
