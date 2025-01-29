
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "letrec"    { LETREC }
  | "concat"    { CONCAT }
  | "length"    { LENGTH }
  | "String"    { STRING }
  | "List"      { LIST }
  | "cons"      { CONS }
  | "nil"       { NIL }   
  | "head"      { HEAD }
  | "tail"      { TAIL }  
  | "isnil"     { ISEMPTY }
  | "case"      { CASE }
  | "of"        { OF }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | "quit"      { QUIT }
  | '}'         { RBRACE }
  | "<"         { LARROW }
  | ">"         { RARROW }
  | "as"        { AS }  
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | ','         { COMMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "|"         { PIPE }
  | "->"        { ARROW }
  | "=>"        { DOUBLEARROW}
  | ";;"        { SEMICOLON }

  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['A'-'Z' 'a'-'z' '_' '0'-'9']*
                { IDT (Lexing.lexeme lexbuf) }
  | '"' [^ '"' ';' '\n']*'"'
                {let s = Lexing.lexeme lexbuf in 
                STRINGV (String.sub s 1 (String.length s - 2))}
  | eof         { EOF }
  | _           { raise Lexical_error }

