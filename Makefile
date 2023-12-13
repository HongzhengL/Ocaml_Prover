# Makefile to zip specified directories and files and additional functionalities

# Name of the output zip file
ZIP_NAME := ocaml_prover.zip

# Directories to be zipped
DIRS := bin/ lib/

# Files to be zipped
FILES := dune-project prover.opam .ocamlformat

# Default target
all: build

# Build the project using Dune
build:
	eval $$(opam env) && \
	dune build

# The zip command
zip:
	zip -r $(ZIP_NAME) $(DIRS) $(FILES)

# Test the project using main executable
test_1: build
	./main.exe --simple getting_started.ml

test_2: build
	./main.exe --simple more_proofs.ml

test_3: build
	./main.exe --simple sample.ml

# Clean the project using Dune
clean:
	eval $$(opam env) && \
	dune clean
	rm -f ocaml_prover.zip

.PHONY: zip build test clean all
