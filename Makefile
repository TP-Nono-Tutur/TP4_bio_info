EXE=bioseq.native

all:
	ocamlbuild -I src -I deps/bio-info_ultimate/src -I deps/Module_perso ${EXE}

get_deps:
	git clone https://github.com/altor/Module_perso.git deps/Module_perso
	git clone https://github.com/TP-Nono-Tutur/bio-info_ultimate.git deps/bio-info_ultimate
clean:
	rm -Rf deps/*
	rm -R _build ${EXE}
