type expression_a =
    | Plus     of expression_a * expression_a * int
    | Moins    of expression_a * expression_a * int
    | Mult     of expression_a * expression_a * int
    | Div      of expression_a * expression_a * int
    | Mod      of expression_a * expression_a * int
    | Egale    of expression_a * expression_a * int
    | Interro  of expression_a* expression_a  * expression_a * int
    | Neg      of expression_a * int
    | Num      of float * int
    | Ident    of string * int
;;

type commande_a =
      Nil of int
    | Commande of expression_a * int
    | Affect of string * expression_a * int
;;

type programme_a =
      Nill of int
    | Astc of commande_a * int
    | Comdeprog of commande_a * programme_a * int
;;

let getTaille y = match y with
         | Plus     (g,d,t) -> t
         | Moins    (g,d,t) -> t
         | Mult     (g,d,t) -> t
         | Div      (g,d,t) -> t
         | Mod      (g,d,t) -> t
         | Egale    (g,d,t) -> t
         | Interro  (g,c,d,t) -> t
         | Neg      (e,t)  -> t
         | Num      (n, t) -> t
         | Ident    (i,t) -> t
;;

let rec print_binaire_comm form e t = Format.fprintf form "@[<2>%i%s@ %a%s@]" t "Commande(" print_AST e ")"
and print_binaire_aff form i e t = Format.fprintf form "@[<2>%i%s@ %s%s@ %a%s@]" t "Affect (" i " ," print_AST e ")"
and print_binaire_progl form l t = Format.fprintf form "@[<2>%i%s@ %a%s@]" t "Astc(" print_AST2 l ")"
and print_binaire_prog form c p t = Format.fprintf form "@[<2>%i%s@ %a%s@ %a%s@]" t "Comdeprog(" print_AST2 c " ," print_AST3 p ")"
and print_binaire form s g d t = Format.fprintf form "@[<2>%i%s%s@ %a%s@ %a%s@]" t s "(" print_AST g " ," print_AST d " )"
and print_binaire_Interro form g c d t =  Format.fprintf form "@[<2>%i%s@ %a%s@ %a%s@ %a%s@]" t "Interro(" print_AST g " ," print_AST c " ," print_AST d ")"
and print_AST form = let open Format in function
    | Plus  (g,d,t)     -> print_binaire form "Plus" g d t
    | Moins (g,d,t)     -> print_binaire form "Moins" g d t
    | Mult  (g,d,t)     -> print_binaire form "Mult" g d t
    | Div   (g,d,t)     -> print_binaire form "Div" g d t
    | Mod  (g,d,t)      -> print_binaire form "Mod" g d t
    | Egale (g,d,t)     -> print_binaire form "Egale" g d t
    | Interro  (g,c,d,t)  -> print_binaire_Interro form g c d t
    | Neg    (e,t)      -> fprintf form "@[<2>%i%s@ %a@]" t "Neg" print_AST e
    | Num    (n, t)     -> fprintf form "@[<2>%i%s@ %g@]" t "Num" n
    | Ident  (i,t)      -> fprintf form "@[<2>%i%s@ %s@]" t "Ident" i
and print_AST2 form = let open Format in function
    | Nil  t            -> ()
    | Commande  (e,t)        -> print_binaire_comm form e t
    | Affect  (i,e,t)      -> print_binaire_aff form i e t
and print_AST3 form = let open Format in function
     | Nill t            -> ()
     | Astc (l,t)        -> print_binaire_progl form l t
     | Comdeprog (c,p,t)      -> print_binaire_prog form c p t
;;




let rec print_exp2 s g d = (print_exp g)^"\n"^(print_exp d)^"\n"^s
and print_exp3 g c d = (print_exp g)^"\nConJmp "^string_of_int ((getTaille c)+1)^"\n"^(print_exp c)^"\nJump "^string_of_int (getTaille d)^"\n"^(print_exp d)
and print_exp  = function
    | Plus  (g,d,t) -> print_exp2 "AddiRe" g d
    | Moins (g,d,t) -> print_exp2 "SubsRe" g d
    | Mult  (g,d,t) -> print_exp2 "MultRe" g d
    | Div   (g,d,t) -> print_exp2 "DiviRe" g d
    | Mod   (g,d,t)  -> print_exp2 "Modulo" g d
    | Egale (g,d,t) -> print_exp2 "Equal" g d
    | Interro  (g,c,d,t) -> print_exp3 g c d
    | Neg    (e,t)    -> (print_exp e)^"\nNegaRe"
    | Num    (n, t)   -> "CstRe "^(string_of_float n)
    | Ident  (i,t)    -> "GetVar "^ i
and print_com = function
    | Nil t -> ""
    | Commande (ex,t) -> print_exp ex
    | Affect (i,e,t) -> (print_exp e)^"\nSetVar "^i
and print_prog = function
    | Nill t        -> ""
    | Astc (l,t)    -> print_com l
    | Comdeprog (c,p,t)  -> (print_com c)^"\n"^(print_prog p)
;;






let rec taille_exp2 x y = 1 + (taille_exp x) + (taille_exp y)
 and taille_exp k = match k with
         | Plus     (g,d,t) -> taille_exp2 g d
         | Moins    (g,d,t) -> taille_exp2 g d
         | Mult     (g,d,t) -> taille_exp2 g d
         | Div      (g,d,t) -> taille_exp2 g d
         | Mod      (g,d,t) -> taille_exp2 g d
         | Egale    (g,d,t) -> taille_exp2 g d
         | Interro  (g,c,d,t) -> taille_exp3 g c d
         | Neg      (e ,t) -> 1 + (taille_exp e)
         | Num      (n, t) -> t
         | Ident    (i,t) -> t

 and taille_exp3 x y z = 2 + (taille_exp x) + (taille_exp y) + (taille_exp z)
 and taille_com r = match r with
         | Nil t -> t
         | Commande (exp,t) -> taille_exp exp
         | Affect(i,e,t) -> 1 + taille_exp e

 and taille_prog2 c p = (taille_com c) + (taille_prog p)
 and taille_prog p = match p with
         | Nill t -> t
         | Astc (l,t) -> taille_com l
         | Comdeprog (c,p,t) -> taille_prog2 c p

;;
