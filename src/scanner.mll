{ 
  open Parser
  open Scanf

  let unescaped s = 
      Scanf.sscanf s "%C%!" (fun x -> x)
}

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }
| "//"                 { singlelinecom lexbuf}
| '('                  { LPAR }
| ')'                  { RPAR }
| '['                  { LBKT }
| ']'                  { RBKT }
| '{'                  { LCUR }
| '}'                  { RCUR }
| ';'                  { SEMICOL }
| ':'                  { COL }
| ','                  { COMMA }
| '.'                  { DOT }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { MUL }
| '/'                  { DIV }
| '%'                  { MOD }
| "**"                 { POW }
| "+="                 { INC }
| "-="                 { DEC }
| "*="                 { SMUL }
| "/="                 { SDIV }
| "%="                 { SMOD }
| '='                  { ASN }
| "=="                 { EQ }
| "!="                 { NEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "&&"                 { AND }
| "||"                 { OR }
| '!'                  { NOT }
| "--"                 { EDGEU }
| "->"                 { EDGED }
| "--:"                { ADJU }
| "->:"                { ADJD }
| "in"                 { IN }
| "continue"           { CONT }
| "break"              { BREAK }
| "if"                 { IF }
| "else"               { ELSE }
| "for"                { FOR }
| "while"              { WHILE }
| "return"             { RET }
| "null"               { NULL }
| "int"                { INT }
| "char"               { CHAR }
| "string"             { STR }
| "graph"              { GRAPH }
| "node"               { NODE }
| "edge"               { EDGE }
| "void"               { VOID }
| "bool"               { BOOL }
| "true"               { BOOLEAN_LIT(true) }
| "false"              { BOOLEAN_LIT(false) }
| ('\'' ([' '-'&' '('-'[' ']'-'~'] as c) '\'')
                       { CHAR_LIT(c) }  
| ("'\\\\'" | "'\\''" | "'\\n'" | "'\\r'" | "'\\t'") as s
                       { CHAR_LIT(unescaped s) } 
| ('0' | ['1'-'9']+['0'-'9']*) as lit 
                       { NUM_LIT(int_of_string lit) }
| '"' (([' '-'!' '#'-'[' ']'-'~'] | '\\' ['\\' '"' 'n' 'r' 't'])* as s) '"' 
                       { STR_LIT(s) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lit 
                       { ID("_" ^ lit) }
| eof                  { EOF }
| _ as c               { raise (Failure("illegal character " ^ Char.escaped c)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlelinecom = parse
  "\n" { token lexbuf }
| eof  { EOF }
| _    { singlelinecom lexbuf}
