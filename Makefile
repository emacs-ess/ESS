## $Id: Makefile,v 5.69 2002/08/08 09:04:35 maechler Exp $
## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

UPLOAD_SITE = software.biostat.washington.edu:/home/ess/downloads

## Set ESSVERSIONTAG to ESS-$(ESSVERSION) with .'s replaced by -s.
## CVS tags can NOT contain .'s.
## This will only work with GNU make, but you won't
## need to change this unless you are an ESS developer.
## If you don't have GNU make, use the command line; for example:
## make tag ESSVERSION=5.2.0 ESSVERSIONTAG=ESS-5-2-0
ESSVERSIONTAG=ESS-$(shell sed 's/\./-/g' VERSION)
ESSDIR=ess-$(ESSVERSION)

## Updating ChangeLog via CVS with emacs requires the vc package!
## If this setting doesn't suit you, you can use the command line:
## make ChangeLog EMACSLOGCVS="myemacs -mybatchflags -mychangelogflags"

EMACSLOGCVS=$(EMACSBATCH) -f vc-update-changelogs

Subdirs = lisp doc

## This is the default target, i.e. 'make' and 'make default' are the same.

default:
	cd lisp; $(MAKE) all

all clean distclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@; cd ..; done

dist: VERSION
	cd doc;  $(MAKE) docs; cd ..
	cd lisp; $(MAKE) dist; grep 'ess-version' ess-cust.el; cd ..
	@echo "** Committing VERSION, README, ANNOUNCE and info **"
	cvs commit -m "Updating toplevel files for new version" \
		VERSION README ANNOUNCE
	cvs commit -m "Updating info for new version" info
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSDIR)"
	@echo "** (must set CVSROOT, etc, prior to checkout for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	cvs export -D today ess
	@echo "** Correct Write Permissions and RM Papers **"
	mv ess $(ESSDIR)
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod a-w $(ESSDIR)/ChangeLog $(ESSDIR)/doc/*
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make*
	chmod u+w $(ESSDIR)/doc/Makefile $(ESSDIR)/lisp/Makefile
	for D in jcgs techrep dsc2001-rmh; do DD=$(ESSDIR)/doc/$$D; \
	  chmod -R u+w $$DD ; rm -rf $$DD ; done
	test -f $(ESSDIR).tar.gz && rm -rf $(ESSDIR).tar.gz || true
	@echo "** Creating tar file **"
	tar hcvof $(ESSDIR).tar $(ESSDIR)
	gzip $(ESSDIR).tar
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	@echo "** Creating zip file **"
	zip -r $(ESSDIR).zip $(ESSDIR)
	@echo "** Cleaning up **"
	chmod -R u+w $(ESSDIR); rm -rf $(ESSDIR)
	touch $@

ChangeLog: VERSION
	$(EMACSLOGCVS)
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ess@franz.stat.wisc.edu>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	cvs commit -m 'Version .. released' ChangeLog

# FIXME: "ChangeLog" does not work (for MM)
#rel: ChangeLog dist tag
rel: dist tag
	@echo "** Placing tar and zip files **"
	scp -p $(ESSDIR).tar.gz $(ESSDIR).zip $(UPLOAD_SITE)

tag:
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)

