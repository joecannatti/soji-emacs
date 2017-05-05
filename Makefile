EMACS ?= emacs
CASK ?= cask

all: test

test: compile
	${MAKE} unit

unit:
	${CASK} exec ert-runner

compile: clean
	${CASK} install
	${CASK} build
	${CASK} package

clean:
	rm -rf dist
	${CASK} clean-elc

.PHONY:	all test unit compile clean
