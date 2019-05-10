
include ../Makeconf

.PHONY: literate all julia lisp ess inf r r-indent literate cases indent-cases literate-cases

all: compile lisp

julia:
	@$(MAKE) -C .. julia

lisp: julia
	${EMACS} -Q --script run-tests

ess:
	${EMACS} -Q --script run-tests --ess

inf:
	${EMACS} -Q --script run-tests --inf

r r-core:
	${EMACS} -Q --script run-tests --r-core

r-indent:
	${EMACS} -Q --script run-tests --r-indent

r-pkg:
	${EMACS} -Q --script run-tests --r-pkg

literate:
	${EMACS} -Q --script run-tests --literate

cases: indent-cases literate-cases

indent-cases:
	${EMACS} -Q --script generate-indent-cases

literate-cases:
	${EMACS} -Q --script generate-literate-cases

.PHONY: compile
compile:
	@cd .. && make -k lisp \
		"COMPILE-FLAGS = --eval \"(setq byte-compile-error-on-warn t)\""
