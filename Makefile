all : asyntax.cmo x86_64.cmo lexer.cmo parser.cmo aritha.cmo  
	ocamlc x86_64.cmo asyntax.cmo lexer.cmo parser.cmo aritha.cmo -o aritha && pdflatex rapport.tex

x86_64.cmo : x86_64.ml
	ocamlc -c x86_64.ml

x86_64.ml :
	ocamlc -c x86_64.mli

aritha.cmo :
	ocamlc -c aritha.ml

asyntax.cmo :
	ocamlc -c asyntax.ml

parser.cmo : parser.cmi
	ocamlc -c parser.ml

lexer.cmo : lexer.ml parser.cmi
	ocamlc -c lexer.ml

parser.cmi : parser.mli asyntax.cmo
	ocamlc -c parser.mli

parser.mli :
	ocamlyacc parser.mly

lexer.ml :
	ocamllex lexer.mll

clean :
	rm -f asyntax.cmi asyntax.cmo lexer.cmo lexer.cmi lexer.ml parser.cmi parser.cmo parser.ml parser.mli aritha aritha.cmi aritha.cmo rapport.pdf resultat.s rapport.aux rapport.log resultat
    


   


