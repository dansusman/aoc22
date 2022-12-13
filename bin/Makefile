INPUTS=day1.txt

.THING: test

test: main $(INPUTS)
	./main $(INPUTS)

main: day1.ml
	ocamlopt -o main day1.ml
