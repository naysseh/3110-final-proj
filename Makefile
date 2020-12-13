# add modules as our codebase evolves
MODULES=database field cluster types makeCluster user main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	cp issues.txt issues.init ;\
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential ;\
	rm issues.txt ;\
	mv issues.init issues.txt

start: 
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip trakio.zip *.ml* *.txt _tags .ocamlinit .merlin Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal  \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private trakio.zip