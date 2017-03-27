BIN=query

all:
	ocamlbuild main.native
	mv main.native $(BIN)

clean:
	rm -rf _build $(BIN)