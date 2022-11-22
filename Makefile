all: build test

SHELL:=/bin/bash

build:
	@dune build
	@echo "build ok"
	@cp _build/default/bin/main.exe test

.PHONY: test

test:
	@echo "run tests..."
	@cd test; ./testall.sh