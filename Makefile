## $Id: Makefile,v 5.35 2000/10/30 01:29:32 rossini Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any !}

## Finally using the "ess/VERSION" file -- for everything.
## These only work with GNU make, but are only used by ESS maintainers
ESSVERSION=$(shell cat VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)
## The following MUST NOT contain "."'s.
ESSVERSIONTAG=ess-$(shell sed 's/\./_/g' VERSION)

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
	cd doc; make all

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
