EMACS=/usr/bin/emacs

cases: indent-cases literate-cases

indent-cases:
	${EMACS} -Q --script generate-indent-cases

literate-cases:
	${EMACS} -Q --script generate-literate-cases


all:
	${EMACS} -Q --script run-tests

r:
	${EMACS} -Q --script run-tests --rstats

r-indent:
	${EMACS} -Q --script run-tests --rstats-indent
