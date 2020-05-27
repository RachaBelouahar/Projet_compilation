
%{
open AST
%}

%token <float> NOMBRE
%token <bool> BOOL
%token <string> IDENT
%token PLUS MOINS FOIS DIV MOD GPAREN DPAREN EGALE INTERRO POINT POINTVIRGULE AFFECT EOL

%right AFFECT
%right INTERRO POINT
%left EGALE
%left PLUS MOINS
%left FOIS DIV MOD
%nonassoc UMOINS

%type <AST.programme_a> programme
%type <AST.commande_a> commande
%type <AST.expression_a>  expression

%start programme

%%
programme:
    EOL                                             { Nill (0) }
 |  commande                                        { Astc ($1,(AST.taille_com $1))}
 |  commande programme                              { Comdeprog ($1,$2,(AST.taille_prog2 $1 $2)) }
commande:
   POINTVIRGULE                                         { Nil (0) }
 | expression POINTVIRGULE                              { Commande ($1,(AST.taille_exp $1)) }
 | IDENT AFFECT expression POINTVIRGULE                 { Affect ($1,$3,(1+(AST.taille_exp $3))) }
 ;
expression:
   expression PLUS expression                       { Plus  ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression MOINS expression                      { Moins ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression FOIS expression                       { Mult  ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression DIV expression                        { Div   ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression MOD expression                        { Mod  ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression EGALE expression                      { Egale ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression INTERRO expression POINT expression   { Interro  ($1,$3,$5, (AST.taille_exp3 $1 $3 $5)) }
 | GPAREN expression DPAREN                         { $2 }
 | MOINS expression %prec UMOINS                    { Neg ($2,(1+(AST.taille_exp $2))) }
 | NOMBRE                                           { Num ($1,1) }
 | IDENT                                            { Ident ($1,1) }
 ;
