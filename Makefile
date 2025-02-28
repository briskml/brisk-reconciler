.PHONY: build run-lambda-term test bench clean

build:
	dune build

run-lambda-term:
	dune exec ./example/lambda-term/lambda_term.exe

test:
	dune test

bench:
	dune exec benchmark

clean:
	dune clean
