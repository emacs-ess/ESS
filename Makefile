## $Id: Makefile,v 5.3 1998/09/10 06:10:54 hornik Exp $
## Top Level Makefile
SHELL = /bin/sh

Subdirs = lisp doc

all install clean distclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

ESS:
	cd lisp; make all
docs:
	cd doc; make all
