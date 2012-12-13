pbjc : ast.cmo parser.cmi parser.cmo scanner.cmo compile.cmo pbjc.cmo 
	ocamlc -o pbjc ast.cmo parser.cmo scanner.cmo compile.cmo pbjc.cmo 

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

scanner.ml : scanner.mll
	ocamllex scanner.mll

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

control_for.java : pbjc
	pbjc -c test/control_for.pbj

control_if.java : pbjc
	pbjc -c test/control_if.pbj

control_if_else.java : pbjc
	pbjc -c test/control_if_else.pbj

control_while.java : pbjc
	pbjc -c test/control_while.pbj

control_do_while.java : pbjc
	pbjc -c test/control_do_while.pbj

.PHONY : clean
clean :
	rm -f pbjc scanner.ml *.cmi *.cmo parser.ml parser.mli scanner.ml