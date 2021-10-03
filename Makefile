#!/usr/bin/env make

.PHONY: build check tags style lint test exec bench doc install setup ghci clean cleanall

TARGET	:= hsort
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h

default: build

build:
	@stack build

all:	check build test bench doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:
	@hlint --color $(SRCS)

test:
	@stack test --coverage
	./testsort.sh

exec:	# Example:  make ARGS="-h" exec
	@stack exec -- $(TARGET) $(ARGS)

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

doc:
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@$(RM) -rf dist random.*

cleanall: clean
	@stack clean --full
	@$(RM) -rf .stack-work/
