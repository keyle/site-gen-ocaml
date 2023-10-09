debug:
	dune build --profile=dev @install
	dune exec sitegen

build:
	dune build --profile=release @install

clean:
	dune clean