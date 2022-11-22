all: build test

SHELL:=/bin/bash

build:
	@dune build
	@echo "build ok"

.PHONY: test

test: build
	@echo "run tests..."
	@cd test; ./testall.sh