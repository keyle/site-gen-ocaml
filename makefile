debug:
	dune exec sitegen

build:
	dune build

release:
	dune clean
	rm -rf release/
	dune build --profile=release
	mkdir -p release/
	mv _build/default/src/sitegen.exe release/sitegen

clean:
	dune clean
	rm -rf release/
