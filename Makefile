## $Id: Makefile,v 5.2 1998/04/17 12:15:34 maechler Exp $
## Top Level Makefile
SHELL = /bin/sh

Subdirs = lisp doc

all:
	for D in $(Subdirs); do cd $$D; make $@ ; cd .. ; done

ESS:
	cd lisp; make all
docs:
	cd doc; make all


clean distclean:
	for D in $(Subdirs); do cd $$D; make $@ ; cd .. ; done
