SOURCE=src/words.ml src/line.ml src/grid.ml src/annealing.ml src/main.ml

bin/Xword: $(SOURCE)
	ocamlformat --inplace $(SOURCE)
	ocamlopt -I src $(SOURCE) -o $@
	ocamldoc -I src -d doc -html $(SOURCE)
	rm src/*.cmx src/*.o src/*.cmi
