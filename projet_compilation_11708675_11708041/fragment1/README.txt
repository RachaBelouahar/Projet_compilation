
ocamlc -c AST.ml
ocamllex lexeur.mll
ocamlyacc parseur.mly
ocamlc -c parseur.mli
ocamlc -c lexeur.ml
ocamlc -c parseur.ml
ocamlc -c TP.ml
ocamlc -o executable AST.cmo lexeur.cmo parseur.cmo TP.cmo
./executable Q4.jsm
