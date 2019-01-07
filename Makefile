all:
	@ocamlfind ocamlc -o sat  -package landmarks -package landmarks.ppx -package unix -linkpkg str.cma sat.ml
pre:
	@ocamlfind ocamlc -o sat_pre  -package landmarks -package landmarks.ppx -package unix -linkpkg str.cma sat_pre.ml
clean:
	@rm -f *.cmo *.cmx *.cmi sat sat_prof
	@echo Done
