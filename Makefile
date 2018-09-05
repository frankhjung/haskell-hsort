#!/usr/bin/env make

TARGET 	:= hsort
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

all:	check build tags install

check:	style lint tags

style:	$(SRCS)
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	@hlint $(SRCS) --git --color --show

tags:	$(SRCS)
	@hasktags --ctags --extendedctag $(SRCS)

build:	$(SRCS)
	@stack build

doc:	$(SRCS)
	@stack haddock

.PHONY: bench
bench:
	@stack bench

.PHONY: bench
setup:
	-stack setup
	-stack query
	-stack ls dependencies

install: $(SRCS)
	@stack install --local-bin-path $(HOME)/bin $(TARGET)

clean:
	@stack clean
	@$(RM) -rf dist

cleanall: clean
	@$(RM) -rf .stack-work/ *.test

ghci:
	@stack ghci --ghci-options -Wno-type-defaults
