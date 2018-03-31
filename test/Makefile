EMACS?=emacs

.PHONY: literate


all:
	${EMACS} -Q --script run-tests

ess:
	${EMACS} -Q --script run-tests --ess

inf:
	${EMACS} -Q --script run-tests --inf

r:
	${EMACS} -Q --script run-tests --rstats

r-indent:
	${EMACS} -Q --script run-tests --rstats-indent

literate:
	${EMACS} -Q --script run-tests --literate


cases: indent-cases literate-cases

indent-cases:
	${EMACS} -Q --script generate-indent-cases

literate-cases:
	${EMACS} -Q --script generate-literate-cases
