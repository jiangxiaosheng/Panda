all: build test

SHELL:=/bin/bash

build:
	@dune build
	@echo "build ok"

.PHONY: test

test:
	@echo "run tests..."
	@cd test; ./testall.sh