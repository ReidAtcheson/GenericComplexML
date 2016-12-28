


propcheck : propcheck.ml
	ocamlbuild -pkg num propcheck.native




.PHONY : clean


clean :
	ocamlbuild -clean
