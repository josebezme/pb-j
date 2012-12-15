pbjc : ast.cmo parser.cmi parser.cmo scanner.cmo compile.cmo pbjc.cmo 
	ocamlc -o pbjc ast.cmo parser.cmo scanner.cmo compile.cmo pbjc.cmo 

pbjc.cmo : compile.ml parser.ml scanner.ml pbjc.ml 
	ocamlc -c pbjc.ml

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

scanner.ml : scanner.mll
	ocamllex scanner.mll

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

bool.java: pbjc
	pbjc -c test/bool.pbj

array_declare.java: pbjc
	pbjc -c test/array_declare.pbj

null.java: pbjc
	pbjc -c test/null.pbj

pbj_spread.java: pbjc
	pbjc test/pbj_spread.pbj
	pbjc -c test/pbj_spread.pbj

arrays.java: pbjc
	pbjc -c test/arrays.pbj

null: pbjc
	pbjc -b test/null.pbj
	java null

arrays: pbjc
	pbjc -b test/arrays.pbj
	java arrays


alltests.java: test1.java test2.java

.PHONY : clean
clean :
	rm -f pbjc scanner.ml *.cmi *.cmo parser.ml parser.mli parser.output scanner.ml
