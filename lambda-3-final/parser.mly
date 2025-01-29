%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token CONCAT 
%token LENGTH
%token BOOL
%token NAT
%token LETREC
%token STRING
%token QUIT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token PIPE
%token CASE
%token OF
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token LARROW
%token RARROW
%token SEMICOLON
%token DOUBLEARROW
%token AS
%token EOF
%token HEAD
%token TAIL
%token LIST
%token CONS
%token UNIT
%token NIL
%token ISEMPTY

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV


%start s
%type <Lambda.command> s

%%

s :
   IDT EQ ty EOF
      { TBind ($1, $3) }
    | IDV EQ term EOF
      { Bind ($1, $3) }
    | term EOF
      { Eval $1 }
    | QUIT EOF
      { Quit }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
      
  | LARROW IDV EQ term RARROW AS ty
      { TmTag ($2, $4, $7) }
  | CASE term OF casesTerm
      { TmCase ($2, $4) }
  | term AS ty
      { TmTag ("", $1, $3) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | LENGTH atomicTerm
      { TmLength $2}
  | appTerm pathTerm
      { TmApp ($1, $2) }
  | HEAD LBRACKET ty RBRACKET pathTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET pathTerm
      { TmTail ($3, $5) }
  | NIL LBRACKET ty RBRACKET
      { TmNil ($3) }
  | ISEMPTY LBRACKET ty RBRACKET pathTerm
      { TmisEmpty ($3, $5) }
  | CONS LBRACKET ty RBRACKET pathTerm pathTerm
       { TmCons ($3, $5, $6) }
       
| pathTerm AS ty
      { TmTag ("", $1, $3) }

pathTerm :
     NIL LBRACKET ty RBRACKET
      { TmNil ($3) }
    | pathTerm DOT INTV
        { TmProjection ($1, (string_of_int $3)) }
    | pathTerm DOT IDV
        { TmProjection ($1, $3) }
    | atomicTerm
        { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
          0 -> TmZero
        | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  
  | LBRACE tupleTerm RBRACE
      { TmTuple $2 }
  | LBRACE recordTerm RBRACE
      { TmRecord $2 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LBRACE tupleTY RBRACE
      { TyTuple $2 }
  | LBRACE recordType RBRACE
      { TyRecord $2 }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }
  | LARROW nonEmptyRecordType RARROW
      { TyVariant $2 }
  | IDV 
    {TyString }
  | IDT
    {TyVar $1}
    

tupleTY :
		ty 
			{ [$1] }
  | ty COMMA tupleTY 
			{ $1 :: $3 }   

tupleTerm :
    | term
        { [$1] }
    | term COMMA tupleTerm
        { $1 :: $3 }

recordTerm :
    | { [] }
    | nonemptyrec
        { $1 }

nonemptyrec :
    | IDV EQ term
        { [$1, $3] }
    | IDV EQ term COMMA nonemptyrec
        { ($1, $3) :: $5 }


recordType :
		{ [] }
	| nonEmptyRecordType
		{ $1 }

nonEmptyRecordType :
	IDV COLON ty
			{ [($1, $3)] }
	| IDV COLON ty COMMA nonEmptyRecordType
			{ ($1, $3) :: $5 }

casesTerm :
    LARROW IDV EQ IDV RARROW DOUBLEARROW term
        { [($2, $4, $7)] }
    | LARROW IDV EQ IDV RARROW DOUBLEARROW term PIPE casesTerm
        { ($2, $4, $7) :: $9 }
