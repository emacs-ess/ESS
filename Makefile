## $Id: Makefile,v 5.47 2001/08/17 15:49:08 maechler Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any !}

## Set ESSVERSION to the contents of VERSION
## This is only set correctly by GNU make, but you will only
## need this if you are performing an XEmacs installation or
## you are an ESS developer.
## If you don't have GNU make, create an environment variable
## ESSVERSION set to the contents of VERSION and use "make -e"

ESSVERSION=$(shell cat VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)
## The following MUST NOT contain "."'s.
ESSVERSIONTAG=ess-$(shell sed 's/\./_/g' VERSION)

## XEMACSDIR and ESSDIR facilitate imitation of an XEmacs distribution
## If you don't have XEmacs or ESS installed in the usual places, then
## you will need to set them by "make -e" with environment variables
## If you are not using GNU make, then see remarks above.

## make    xemacs-links	# w/  GNU make, XEmacs/ESS in the usual places
##

## make -e xemacs-links	# w/  GNU make, XEmacs/ESS not in the usual places
##                      # environment variables XEMACSDIR/ESSDIR set

## make -e xemacs-links	# w/o GNU make, XEmacs/ESS in the usual places
##                      # environment variable ESSVERSION set

## make -e xemacs-links	# w/o GNU make, XEmacs/ESS not in the usual places
##                      # environment variables ESSVERSION/XEMACSDIR/ESSDIR set

## XEMACSDIR:  parent directory of the xemacs-packages sub-directory
XEMACSDIR=/usr/local/lib/xemacs
## ESSDIR:  parent directory of ESSVERSIONDIR
ESSDIR=$(XEMACSDIR)/site-packages/ess

Subdirs = lisp doc

INTRO.DEPENDS= VERSION doc/credits.texi doc/inst_cvs.texi \
	doc/newfeat.texi  doc/authors.texi  doc/currfeat.texi \
	doc/inst_tar.texi doc/bugrept.texi  doc/license.texi  \
	doc/requires.texi doc/bugs.texi     doc/getting.texi  \
	doc/mailing.texi  doc/stabilty.texi


all install clean distclean realclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

ESS:
	cd lisp; $(MAKE) all

docs:
	cd doc; $(MAKE) info


README : doc/readme.texi $(INTRO.DEPENDS)
	cd doc ; $(MAKE) readme.texi; $(MAKEINFOascii) readme.texi \
	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' > ../README

ANNOUNCE: doc/announc.texi $(INTRO.DEPENDS)
	cd doc; $(MAKE) readme.texi; $(MAKEINFOascii) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

pre-dist: README ANNOUNCE docs
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
	cvs commit -m'Version .. released [make dist]' ChangeLog
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)

dist: pre-dist tar
	@echo "** Placing tar and zip files **"
	scp ESS-$(ESSVERSION).tar.gz ess@franz.stat.wisc.edu:~/public_html
	scp ESS-$(ESSVERSION).zip    ess@franz.stat.wisc.edu:~/public_html

tar: docs
	@echo "** Exporting Files **"
	cvs export -D today ess
	@echo "** Correct Write Permissions and RM Papers **"
	ln -s ess $(ESSVERSIONDIR)
	chmod a-w $(ESSVERSIONDIR)/lisp/*.el
	chmod a-w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	chmod u+w $(ESSVERSIONDIR)/doc/ess.info*
	chmod u+w $(ESSVERSIONDIR)/lisp/ess-site.el $(ESSVERSIONDIR)/Make*
	chmod u+w $(ESSVERSIONDIR)/doc/Makefile $(ESSVERSIONDIR)/lisp/Makefile
	for D in techrep dsc2001-rmh; do DD=$(ESSVERSIONDIR)/doc/$$D; \
	  chmod -R u+w $$DD ; rm -rf $$DD ; done
	@echo "** Creating tar file **"
	tar hcvof ESS-$(ESSVERSION).tar $(ESSVERSIONDIR)
	gzip ESS-$(ESSVERSION).tar
	@echo "** Creating zip file **"
	zip -r ESS-$(ESSVERSION).zip $(ESSVERSIONDIR)
	@echo "** Cleaning up **"
	chmod -R u+w ess; rm -rf ess $(ESSVERSIONDIR)

doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3: doc/ess.texi
	$(MAKE) docs

xemacs-links: doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3
	rm -f $(XEMACSDIR)/xemacs-packages/etc/ess-* $(XEMACSDIR)/xemacs-packages/lisp/ess-* \
	    $(XEMACSDIR)/xemacs-packages/info/ess.info*
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/etc             $(XEMACSDIR)/xemacs-packages/etc/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/lisp            $(XEMACSDIR)/xemacs-packages/lisp/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info    $(XEMACSDIR)/xemacs-packages/info/ess.info
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-1  $(XEMACSDIR)/xemacs-packages/info/ess.info-1
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-2  $(XEMACSDIR)/xemacs-packages/info/ess.info-2
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-3  $(XEMACSDIR)/xemacs-packages/info/ess.info-3
