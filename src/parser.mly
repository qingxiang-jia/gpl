%{ open Ast %}

%token LPAR RPAR LBKT RBKT LCUR RCUR SEMICOL COMMA DOT COL
%token PLUS MINUS MUL DIV MOD POW INC DEC SMUL SDIV SMOD
%token ASN AND IS OR NOT
%token EQ NEQ LT LEQ GT GEQ 
%token EDGEU EDGED ADJU ADJD
%token FUNC IN CONT BREAK IF ELSE FOR WHILE RET VOID
%token INT CHAR STR BOOL GRAPH NODE EDGE
%token <int> NUM_LIT 
%token <string> ID
%token <bool> BOOLEAN_LIT
%token <string> STR_LIT
%token <char> CHAR_LIT
%token NULL
%token EOF

%left COMMA
%left EDGEU EDGED EDGEW EDGEWD
%right ASN INC DEC SMUL SDIV SMOD
%left OR
%left AND
%left IS NOT
%left IN
%left EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left MUL DIV MOD
%right POW
%left DOT
%left LBKT RBKT
%left LPAR RPAR
%nonassoc ELSE

%start program
%type <Ast.program> program
%type <Ast.var_type> vartype
%type <Ast.var_decl> vardecl
%type <Ast.func_decl> fdecl
%type <Ast.func_decl> procdecl
%type <Ast.expr> expr
%type <Ast.stmt> stmt
%type <Ast.node> node_declarator
%%

program:
   /* nothing */ { {gdecls=[]; fdecls=[] } }
 | program vardeclstmt { {gdecls=$2::$1.gdecls; fdecls=$1.fdecls} }
 | program fdecl { {gdecls= $1.gdecls; fdecls=$2::$1.fdecls} }
 | program procdecl { {gdecls= $1.gdecls; fdecls=$2::$1.fdecls} }

fdecl:
   retval formals_opt RPAR LCUR stmt_list RCUR
     { 
      if (String.compare (snd $1) "main") == 0 then
        raise Main_not_void
      else
        { fname = snd $1;
           formals = $2; 
           body = Block(List.rev $5);
           ret = fst $1
           } }

procdecl: /*procedure (aka. void function) declarator*/
    VOID ID LPAR formals_opt RPAR LCUR stmt_list RCUR {
      { fname = $2;
         formals = $4; 
         body = Block(List.rev $7);
         ret = {ptype = (if (String.compare $2 "main") == 0 then Int else Void); dimension = []}
      }
    }

retval:
    vartype ID LPAR { $1, $2 }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    vardecl                   { [$1] }
  | formal_list COMMA vardecl { $3 :: $1 }

vardeclstmt:
    vardecl SEMICOL { $1 }

vardecl:
    vartype ID      {
                        {
                        vname = $2;
                        vtype = $1;
                        vinit = Noexpr;
                        }
                    }
    | vartype ID ASN expr 
                    {
                        {
                            vname = $2;
                            vtype = $1;
                            vinit = $4;
                        }
                    }

vartype:
    INT         {
                    {
                        ptype = Int;
                        dimension = []
                    }
                }
    |CHAR       {
                    {
                        ptype = Char;
                        dimension = []
                    }
                }
    |STR        {
                    {
                        ptype = Str;
                        dimension = []
                    }
                }  
    |GRAPH      { 
                    {
                        ptype = Graph;
                        dimension = []
                    }
                }
    |NODE       {
                    {
                        ptype = Node;
                        dimension = []
                    }
                }
    |EDGE       {
                    {
                        ptype = Edge;
                        dimension = []
                    }
                }  
    |BOOL       { 
                    {
                        ptype = Bool;
                        dimension = []
                    }
                }
    |vartype LBKT expr_opt RBKT   {

                                {
                                    ptype = $1.ptype;
                                    dimension = (if $3 == Noexpr then NumLit(0) else $3) :: $1.dimension;
                                }
                            }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMICOL { Expr( $1 ) } 
  | RET expr SEMICOL { Return($2) }
  | RET SEMICOL { Return(Noexpr) }
  | LCUR stmt_list RCUR { Block(List.rev $2) }
  | IF LPAR expr RPAR stmt { If($3, $5, Block([])) }
  | IF LPAR expr RPAR stmt ELSE stmt   { If($3, $5, $7) }
  | FOR LPAR expr_opt SEMICOL expr_opt SEMICOL expr_opt RPAR stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAR expr RPAR stmt { While($3, $5) }
  | vardeclstmt { Localvar($1) }

expr_opt:
                  { Noexpr }
  | expr          { $1 }

expr:
    BOOLEAN_LIT      { BoolLit($1) }
  | CHAR_LIT         { CharLit($1) }
  | NUM_LIT          { NumLit($1) }
  | STR_LIT          { StrLit($1) }
  | NULL             { Null }
  | MINUS NUM_LIT    { NumLit(-$2) }
  | PLUS NUM_LIT     { NumLit($2) }
  | MINUS lvalue     { Negof($2)  }
  | PLUS lvalue      { $2 }
  | NOT expr         { Notof($2) } 
  | expr IN expr     { Binop($1, In, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr MUL  expr   { Binop($1, Mul,  $3) }
  | expr DIV expr    { Binop($1, Div,   $3) }
  | expr MOD expr    { Binop($1, Mod, $3) }
  | expr POW expr    { Binop($1, Pow, $3) }
  | expr EQ     expr { Binop($1, Eq, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr OR    expr  { Binop($1, Or,   $3) }
  | expr AND   expr  { Binop($1, And,   $3) }
  | expr INC expr    { Assign($1, Sadd, $3) }
  | expr DEC expr    { Assign($1, Ssub, $3) }
  | expr SMUL expr   { Assign($1, Smul, $3) }
  | expr SDIV expr   { Assign($1, Sdiv, $3) }
  | expr SMOD expr   { Assign($1, Smod, $3) }
  | expr ASN expr    { Assign($1, Asn, $3) }
  | f_call { Call(fst $1, snd $1) }
  | expr DOT ID   { MultiId($1, Dot, $3) }
  | expr DOT f_call { Call(fst $3, $1::(snd $3)) }  
  | lvalue           { $1 }
  | gexpr            { GraphLit($1) }

f_call:
    ID LPAR actuals_opt RPAR { $1, $3 }

gexpr:
    LBKT graph_body RBKT { $2 }

graph_body:
    edge_declaration_list { $1 }

edge_declaration_list:
    edge_declaration SEMICOL { $1 }
  | edge_declaration SEMICOL edge_declaration_list { $1 @ $3 }

edge_stmt:
    node_declarator { $1, [] }
  | node_declarator EDGEU edge_stmt { $1, {src = $1; dest = fst $3; weight =
      NumLit(0)} :: {src = fst $3; dest = $1; weight = NumLit(0)} ::snd $3 }
  | node_declarator EDGED edge_stmt { $1, {src = $1; dest = fst $3; weight =
      NumLit(0)} :: snd $3 }
  | node_declarator MINUS LPAR expr RPAR MINUS edge_stmt { $1, {src = $1; dest = fst $7; weight = $4} :: {src = fst $7; dest = $1; weight = $4} :: snd $7}
  | node_declarator MINUS LPAR expr RPAR GT edge_stmt { $1, {src = $1; dest = fst $7; weight = $4} :: snd $7}

edge_declaration:
    edge_stmt { snd $1 }
  | node_declarator COL MINUS LPAR expr RPAR MINUS node_declarator_list { let rec construct_dedges nodes =
                                                                            match nodes with
                                                                              [] -> []
                                                                            | node ::
                                                                                nodes -> { src = $1; dest = node; weight = $5 } ::
                                                                                         { src = node; dest = $1; weight = $5 } ::
                                                                                         (construct_dedges nodes)
                                                                          in construct_dedges $8 }
  | node_declarator COL MINUS LPAR expr RPAR GT node_declarator_list  { let construct_edge_triplet node = { src = $1; dest = node; weight = $5 }
                                                                        in List.map construct_edge_triplet $8 }
  | node_declarator COL EDGEU node_declarator_list { let rec construct_dedges nodes = match nodes with
                                                          [] -> []
                                                        | node :: nodes -> 
                                                                { src = $1; dest = node; weight = NumLit(0) } :: { src = node; dest = $1; weight = NumLit(0) } :: (construct_dedges nodes)
                                                     in construct_dedges $4 }
  | node_declarator COL EDGED node_declarator_list  { let construct_edge_triplet node = { src = $1; dest = node; weight = NumLit(0) }
                                                      in List.map construct_edge_triplet $4 }

node_declarator_list:
    node_declarator { [$1] }
    | node_declarator node_declarator_list    { $1 :: $2}
                                                       
node_declarator:
    ID  { $1 }

lvalue:
        var  {$1}
        | LPAR expr RPAR {$2}

var:
        ID      { Id($1) }
        | arr   { Array( fst $1, snd $1) }

arr:
    ID LBKT expr RBKT { $1, [$3] }
    | arr LBKT expr RBKT { fst $1, $3 :: snd $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
