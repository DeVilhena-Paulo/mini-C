
main.native: *.ml*
	ocamlbuild $@

mini-c:
	ln -s main.native $@
