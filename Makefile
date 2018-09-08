#!/usr/bin/env make

TARGET 	:= hsort
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

all:	check build tags install doc

check:	style lint tags

style:	$(SRCS)
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	@hlint $(SRCS) --git --color --show

tags:	$(SRCS)
	@hasktags --ctags --extendedctag $(SRCS)

build:	$(SRCS)
	@stack build

.PHONY: doc
doc:
	@stack haddock

.PHONY: bench
bench:
	@stack bench

.PHONY: install
install:
	@stack install --local-bin-path $(HOME)/bin $(TARGET)

.PHONY: setup
setup:
	-stack setup
	-stack query
	-stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@$(RM) -rf dist random.*

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/ *.test

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults
