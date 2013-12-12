
main: test.ml streams.ml
	ocamlc -c streams.ml
	ocamlc streams.cmo test.ml -o main

clean:
	rm -rf *.cmi *.cmo *.swp
