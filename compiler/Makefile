pbjc : ast.cmo parser.cmi parser.cmo scanner.cmo pbjc.cmo 
	ocamlc -o pbjc ast.cmo parser.cmo scanner.cmo pbjc.cmo 

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

scanner.ml : scanner.mll
	ocamllex scanner.mll

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f pbjc scanner.ml *.cmi *.cmo parser.ml parser.mli scanner.ml