all : asyntax.cmo x86_64.cmo lexer.cmo parser.cmo main.cmo  
	ocamlc x86_64.cmo asyntax.cmo lexer.cmo parser.cmo main.cmo -o main && pdflatex rapport.tex

x86_64.cmo : x86_64.ml
	ocamlc -c x86_64.ml

x86_64.ml :
	ocamlc -c x86_64.mli

main.cmo :
	ocamlc -c main.ml

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
	rm -f asyntax.cmi asyntax.cmo lexer.cmo lexer.cmi lexer.ml parser.cmi parser.cmo parser.ml parser.mli main main.cmi main.cmo rapport.pdf
    


   


