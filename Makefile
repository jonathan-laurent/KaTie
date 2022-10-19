BIN=query

.PHONY: build clean test full-clean

build:
	dune build src/query.exe
	mv _build/default/src/query.exe $(BIN)

clean:
	rm -rf _build $(BIN)

test: build
	cd tests/catphos ; make

full-clean: clean
	cd tests/catphos ; make clean