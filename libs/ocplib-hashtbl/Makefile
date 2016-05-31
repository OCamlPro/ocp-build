all: test.opt test-unsafe.opt

fastHashtbl.ml: fastHashtbl.header  fastHashtbl.funs  fastHashtbl.middle
	cat  fastHashtbl.header  fastHashtbl.funs  fastHashtbl.middle  fastHashtbl.funs > fastHashtbl.ml

test.opt:  stdHashtbl.mli stdHashtbl.ml fastHashtbl.ml main.ml
	ocamlopt -g unix.cmxa -o test.opt stdHashtbl.mli stdHashtbl.ml fastHashtbl.mli fastHashtbl.ml main.ml

test-unsafe.opt:  stdHashtbl.mli stdHashtbl.ml fastHashtbl.ml main.ml
	ocamlopt -unsafe unix.cmxa -o test-unsafe.opt  stdHashtbl.mli stdHashtbl.ml fastHashtbl.mli fastHashtbl.ml main.ml

clean:
	rm -f *.byte *.opt *.cm? *.cm?? *.o *~

