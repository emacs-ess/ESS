EMACS=/usr/bin/emacs

all:
	${EMACS} -Q --script run-tests

inf:
	${EMACS} -Q --script run-tests --inf

r:
	${EMACS} -Q --script run-tests --rstats

r-indent:
	${EMACS} -Q --script run-tests --rstats-indent


cases: indent-cases literate-cases

indent-cases:
	${EMACS} -Q --script generate-indent-cases

literate-cases:
	${EMACS} -Q --script generate-literate-cases
