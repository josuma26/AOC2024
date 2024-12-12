
day = 8

all: run

run:
	ocaml day$(day).ml

clean:
	rm -f *.out *.cmi *.cmo

