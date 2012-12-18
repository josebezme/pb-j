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

string_assign.java: pbjc
	pbjc -c test/string_assign.pbj

test2.java: pbjc
	pbjc -c test/02_two_funcs.pbj

null.java: pbjc
	pbjc -c test/null.pbj

pbj_spread.java: pbjc
	pbjc -c test/pbj_spread.pbj

pbj_slice_spread.java: pbjc
	pbjc -c test/pbj_slice_spread.pbj

pbj_jam_spread.java: pbjc
	pbjc -c test/pbj_jam_spread.pbj

pbj_jam_spread_default.java: pbjc
	pbjc -c test/pbj_jam_spread_default.pbj

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
