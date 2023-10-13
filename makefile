debug:
	dune exec sitegen

build:
	dune clean
	rm -rf release/
	dune build --profile=release
	mkdir -p release/
	mv _build/default/src/sitegen.exe ./sitegen
	mv ./sitegen release/sitegen

clean:
	dune clean
	rm -rf release/
