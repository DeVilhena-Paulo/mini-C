
test: main.native mini-c
	@./mini-c test.c

main.native: *.ml*
	ocamlbuild $@

mini-c:
	ln -s main.native $@
