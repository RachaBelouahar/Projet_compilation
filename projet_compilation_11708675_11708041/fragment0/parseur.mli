type token =
  | NOMBRE of (float)
  | BOOL of (bool)
  | IDENT of (string)
  | PLUS
  | MOINS
  | FOIS
  | DIV
  | MOD
  | GPAREN
  | DPAREN
  | EGALE
  | INTERRO
  | POINT
  | POINTVIRGULE
  | AFFECT
  | EOL

val programme :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AST.programme_a
