debug:
	dune exec sitegen

build:
	dune build --profile=release @install

clean:
	dune clean