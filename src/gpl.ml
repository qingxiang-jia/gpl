(*
  GPL Complier
*)
type action = Ast | Sast | Cgen | Verbose

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast); ("-s", Sast); ("-c", Cgen); ]
  else Cgen in
  
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> print_string (Ast.string_of_program program)
  | Sast -> print_string (Sast.string_of_program (Sast.s_check_program program))
  | Cgen -> Cgen.write_c_program "intermediate.cc" (Sast.s_check_program program);
            ignore (Sys.command "g++ -w intermediate.cc -Isrc")
  | _ -> print_string "Unrecognized option"
