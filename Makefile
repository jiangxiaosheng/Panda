all : example.out

pandac : parser.cmo scanner.cmo pandac.cmo
	ocamlc -w A -o pandac $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

example.out : pandac example.pd
	./pandac < example.pd > example.out

pandac.cmo : scanner.cmo parser.cmi ast.cmo
pandac.cmx : scanner.cmx parser.cmx ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml example.out pandac
