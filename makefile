debug:
	dune exec sitegen

build:
	dune build

release:
	dune clean
	dune build --profile=release
	cp _build/default/src/sitegen.exe ./sitegen

clean:
	dune clean
