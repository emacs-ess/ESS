## $Id: Makefile,v 5.57 2002/05/06 05:27:54 rmh Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any !}

## Set EMACS to either emacs or xemacs depending on your situation.
EMACS=emacs

## compile with non-interactive, clean environment:
## EMACS 21
BATCHFLAGS = --batch --no-site-file --no-init-file
## XEMACS 21
#BATCHFLAGS = -batch -no-site-file -no-init-file

## Set ESSVERSION to the contents of VERSION
## This is only set correctly by GNU make, but you will only
## need this if you are performing an XEmacs installation or
## you are an ESS developer.
## If you don't have GNU make, create an environment variable
## ESSVERSION set to the contents of VERSION and use "make -e"

ESSVERSION=$(shell cat VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)
## The following MUST NOT contain "."'s.
ESSVERSIONTAG=ESS-$(shell sed 's/\./-/g' VERSION)

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

## Updating ChangeLog via CVS with emacs
## If you would like to build ChangeLog directly from CVS
## with emacs, then you need to define EMACS and BATCHFLAGS
## appropriately.  See above.
## Note that this requires that the vc package is available!

EMACSLOGCVS=$(EMACS) $(BATCHFLAGS) -f vc-update-changelogs

Subdirs = lisp doc

INTRO.DEPENDS= VERSION doc/credits.texi doc/inst_cvs.texi \
	doc/newfeat.texi  doc/authors.texi  doc/currfeat.texi \
	doc/inst_tar.texi doc/bugrept.texi  doc/license.texi  \
	doc/requires.texi doc/bugs.texi     doc/getting.texi  \
	doc/mailing.texi  doc/stabilty.texi

all install clean distclean realclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

compile:
	cd lisp; $(MAKE) all EMACS=$(EMACS) BATCHFLAGS="$(BATCHFLAGS)"

README: doc/readme.texi $(INTRO.DEPENDS)
	cd doc; $(MAKE) readme.texi; $(MAKEINFOascii) readme.texi \
	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' > ../README

ANNOUNCE: doc/announc.texi $(INTRO.DEPENDS)
	cd doc; $(MAKE) readme.texi; $(MAKEINFOascii) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

docs: README ANNOUNCE
	@echo "** Committing README and ANNOUNCE **"
	cvs commit -m "Updating README, ANNOUNCE for new version" \
		README ANNOUNCE
	cd doc
	$(MAKE) info ESSVERSION=$(ESSVERSION) ESSINFODIR=$(ESSVERSIONDIR)/info
	cd ..
	cvs commit -m "Updating docs for new version" doc

dist: docs
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSVERSIONDIR)"
	@echo "** (must set CVSROOT, etc, prior to checkout for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	cvs export -D today ess 
	@echo "** Correct Write Permissions and RM Papers **"
	mv ess $(ESSVERSIONDIR)
	chmod a-w $(ESSVERSIONDIR)/lisp/*.el
	chmod a-w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	chmod u+w $(ESSVERSIONDIR)/doc/ess.info*
	chmod u+w $(ESSVERSIONDIR)/lisp/ess-site.el $(ESSVERSIONDIR)/Make*
	chmod u+w $(ESSVERSIONDIR)/doc/Makefile $(ESSVERSIONDIR)/lisp/Makefile
	for D in jcgs techrep dsc2001-rmh; do DD=$(ESSVERSIONDIR)/doc/$$D; \
	  chmod -R u+w $$DD ; rm -rf $$DD ; done
	test -f $(ESSVERSIONDIR).tar.gz && rm -rf $(ESSVERSIONDIR).tar.gz || true
	@echo "** Creating tar file **"
	tar hcvof $(ESSVERSIONDIR).tar $(ESSVERSIONDIR)
	gzip $(ESSVERSIONDIR).tar
	test -f $(ESSVERSIONDIR).zip && rm -rf $(ESSVERSIONDIR).zip || true
	@echo "** Creating zip file **"
	zip -r $(ESSVERSIONDIR).zip $(ESSVERSIONDIR)
	@echo "** Cleaning up **"
	chmod -R u+w $(ESSVERSIONDIR); rm -rf $(ESSVERSIONDIR)

ChangeLog:
	$EMACSLOGCVS
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ess@franz.stat.wisc.edu>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	cvs commit -m 'Version .. released' ChangeLog

rel: ChangeLog dist
	@echo "** Placing tar and zip files **"
	scp $(ESSVERSIONDIR).tar.gz software.biostat.washington.edu:/home/ess/downloads
	scp $(ESSVERSIONDIR).zip    software.biostat.washington.edu:/home/ess/downloads

tag: 
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)

doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3 doc/ess.info-4: doc/ess.texi
	$(MAKE) docs

xemacs-links: doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3 doc/ess.info-4
	rm -f $(XEMACSDIR)/xemacs-packages/etc/ess-* $(XEMACSDIR)/xemacs-packages/lisp/ess-* \
	    $(XEMACSDIR)/xemacs-packages/info/ess.info*
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/etc             $(XEMACSDIR)/xemacs-packages/etc/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/lisp            $(XEMACSDIR)/xemacs-packages/lisp/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info    $(XEMACSDIR)/xemacs-packages/info/ess.info
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-1  $(XEMACSDIR)/xemacs-packages/info/ess.info-1
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-2  $(XEMACSDIR)/xemacs-packages/info/ess.info-2
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-3  $(XEMACSDIR)/xemacs-packages/info/ess.info-3
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/ess.info-4  $(XEMACSDIR)/xemacs-packages/info/ess.info-4
