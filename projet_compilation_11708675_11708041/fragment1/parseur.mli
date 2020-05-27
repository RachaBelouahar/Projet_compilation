type token =
  | NOMBRE of (float)
  | BOOLEEN of (bool)
  | IDENT of (string)
  | PLUS
  | MOINS
  | FOIS
  | DIV
  | MOD
  | GPAREN
  | DPAREN
  | GACCOL
  | DACCOL
  | EGALE
  | NEGALE
  | SUPP
  | TYPEOF
  | INTERRO
  | POINT
  | SI
  | SINON
  | AFFECT
  | TQ
  | FAIRE
  | POUR
  | EOL
  | POINTVIRGULE
  | NON
  | ET
  | OU
  | ECRIRE
  | COMMENTAIRE

val programme :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AST.programme_a
