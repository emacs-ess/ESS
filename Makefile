## $Id: Makefile,v 5.61 2002/07/24 22:19:32 rsparapa Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any!}

## Set ESSVERSIONTAG to ESS-$(ESSVERSION) with .'s replaced by -s.
## CVS tags can NOT contain .'s.
## This will only work with GNU make, but you won't
## need to change this unless you are an ESS developer.
## If you don't have GNU make, use the command line; for example:
## make tag ESSVERSION=5.2.0 ESSVERSIONTAG=ESS-5-2-0
ESSVERSIONTAG=ESS-$(shell sed 's/\./-/g' VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)

## Updating ChangeLog via CVS with emacs requires the vc package!
## If this setting doesn't suit you, you can use the command line:
## make ChangeLog EMACSLOGCVS="myemacs -mybatchflags -mychangelogflags"

EMACSLOGCVS=$(EMACSBATCH) -f vc-update-changelogs

Subdirs = lisp doc

INTRO.DEPENDS= VERSION doc/credits.texi doc/inst_cvs.texi \
	doc/newfeat.texi  doc/authors.texi  doc/currfeat.texi \
	doc/inst_tar.texi doc/bugrept.texi  doc/license.texi  \
	doc/requires.texi doc/bugs.texi     doc/getting.texi  \
	doc/mailing.texi  doc/stabilty.texi

## This is the default target, i.e. 'make' and 'make compile' are the same.
## However, you may still need to specify EMACS and BATCHFLAGS.
## See the discussion of EMACS and BATCHFLAGS above. 

default:
	cd lisp; $(MAKE) all

all install clean distclean realclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@; cd ..; done

info:   VERSION
	@echo "** Committing README, ANNOUNCE and info **"
	cd doc; $(MAKE) $@; cd ..
	cvs commit -m "Updating README, ANNOUNCE for new version" \
		README ANNOUNCE
	cvs commit -m "Updating info for new version" doc

dist: info
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
#	chmod u+w $(ESSVERSIONDIR)/doc/ess.info*
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
	$(EMACSLOGCVS)
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

#doc/ess.info doc/ess.info-1 doc/ess.info-2 doc/ess.info-3 doc/ess.info-4: doc/ess.texi
#	$(MAKE) docs


