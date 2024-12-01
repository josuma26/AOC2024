
day = 1

all: run

run:
	ocaml day$(day).ml

clean:
	rm -f *.out *.cmi *.cmo

