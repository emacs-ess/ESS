## $Id: Makefile,v 5.39 2001/06/20 19:41:43 ess Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any !}

## Finally using the "ess/VERSION" file -- for everything.
## These only work with GNU make, but are only used by ESS maintainers
ESSVERSION=$(shell cat VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)
## The following MUST NOT contain "."'s.
ESSVERSIONTAG=ess-$(shell sed 's/\./_/g' VERSION)

## The following 2 variables facilitate imitation of an XEmacs distribution
## You may need to over-ride them by environment variables with the -e option
## If you are not using GNU make, then you may have to over-ride ESSVERSION too

## XEMACSDIR:  parent directory of the xemacs-packages sub-directory
XEMACSDIR=/usr/local/lib/xemacs
## ESSDIR:  parent directory of ESS
ESSDIR=$(XEMACSDIR)/site-packages

Subdirs = lisp doc

INTRO.DEPENDS= VERSION doc/credits.texi doc/inst_cvs.texi \
	doc/newfeat.texi  doc/authors.texi  doc/currfeat.texi \
	doc/inst_tar.texi doc/bugrept.texi  doc/license.texi  \
	doc/requires.texi doc/bugs.texi     doc/getting.texi  \
	doc/mailing.texi  doc/stabilty.texi


all install clean distclean realclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

ESS:
	cd lisp; make all

docs:
	cd doc; make info

## This target is used to create a new version of the tar-file.
## prefix'ing with "-" implies that errors are non-critical.

README : doc/readme.texi $(INTRO.DEPENDS)
	cd doc ; $(MAKE) readme.texi; $(MAKEINFOascii) readme.texi \
	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' > ../README

ANNOUNCE: doc/announc.texi $(INTRO.DEPENDS)
	cd doc; $(MAKE) readme.texi; $(MAKEINFOascii) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

dist: README ANNOUNCE docs
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSVERSIONDIR)"
	@echo "** (must set CVSROOT, etc, prior to checkout for security)"
	@echo "**********************************************************"
	@echo "** Committing README and ANNOUNCE **"
	cvs commit -m "Updating README, ANNOUNCE for new version [make dist]" \
		README ANNOUNCE
	cvs commit -m "Updating docs for new version [make dist]" doc
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ess@franz.stat.wisc.edu>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)
	$(MAKE) tar
	@echo "** Placing tar and zip files **"
	scp ESS-$(ESSVERSION).tar.gz ess@franz.stat.wisc.edu:~/public_html
	scp ESS-$(ESSVERSION).zip    ess@franz.stat.wisc.edu:~/public_html

tar:
	@echo "** Exporting Files **"
	cvs export -D today ess
	@echo "** Creating tar file **"
	ln -s ess $(ESSVERSIONDIR)
	chmod a-w $(ESSVERSIONDIR)/lisp/*.el
	chmod a-w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	chmod u+w $(ESSVERSIONDIR)/doc/ess.info*
	chmod u+w $(ESSVERSIONDIR)/lisp/ess-site.el $(ESSVERSIONDIR)/Make*
	chmod u+w $(ESSVERSIONDIR)/doc/Makefile $(ESSVERSIONDIR)/lisp/Makefile
	tar hcvof ESS-$(ESSVERSION).tar $(ESSVERSIONDIR)
	gzip ESS-$(ESSVERSION).tar
	@echo "** Creating zip file **"
	zip -r ESS-$(ESSVERSION).zip $(ESSVERSIONDIR)
	@echo "** Cleaning up **"
	rm -rf ess $(ESSVERSIONDIR)

doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3: doc/ess.texi 
	make docs

xemacs-links: doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3
	rm -f $(XEMACSDIR)/xemacs-packages/etc/ess-* $(XEMACSDIR)/xemacs-packages/lisp/ess-* \
	    $(XEMACSDIR)/xemacs-packages/info/ess.info*
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/etc             $(XEMACSDIR)/xemacs-packages/etc/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/lisp            $(XEMACSDIR)/xemacs-packages/lisp/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info    $(XEMACSDIR)/xemacs-packages/info/ess.info
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-1  $(XEMACSDIR)/xemacs-packages/info/ess.info-1
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-2  $(XEMACSDIR)/xemacs-packages/info/ess.info-2
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-3  $(XEMACSDIR)/xemacs-packages/info/ess.info-3
