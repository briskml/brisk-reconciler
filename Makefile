.PHONY: build run-lambda-term test clean

build:
	dune build

run-lambda-term: build
	dune exec ./example/lambda-term/lambda_term.exe

test:
	dune test

clean:
	dune clean
