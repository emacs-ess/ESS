## $Id: Makefile,v 5.1 1997/12/05 14:46:38 ess Exp $
## Top Level Makefile


all : ESS doc


ESS : 
	(cd lisp; make all)

doc :
	(cd doc; make all)



