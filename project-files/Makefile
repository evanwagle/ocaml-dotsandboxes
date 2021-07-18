MODULES=board ai player command state author gui 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip game.zip *.ml* *.json *.sh *.md _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
docs: docs-public 
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal -package graphics \
		-html -stars -d _doc.public $(MLIS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private game.zip
