## $Id: Makefile,v 5.77 2004/05/10 18:22:58 rsparapa Exp $
## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

Subdirs = lisp doc

## This is the default target, i.e. 'make' and 'make all' are the same.

all install clean distclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@; cd ..; done

#ESS is now an official XEmacs package, but the xemacs-links target 
#persists since there is generally a lag between an ESS release and 
#the corresponding XEmacs ESS package release
#see http://www.xemacs.org/Documentation/packageGuide.html
#for package installation instructions 
xemacs-links: info/ess.info info/ess.info-1 info/ess.info-2 info/ess.info-3 info/ess.info-4
	rm -f $(XEMACSDIR)/xemacs-packages/etc/ess-* $(XEMACSDIR)/xemacs-packages/lisp/ess-* \
	    $(XEMACSDIR)/xemacs-packages/info/ess.info*
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/etc              $(XEMACSDIR)/xemacs-packages/etc/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/lisp             $(XEMACSDIR)/xemacs-packages/lisp/$(ESSVERSIONDIR)
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/info/ess.info    $(XEMACSDIR)/xemacs-packages/info/ess.info
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/info/ess.info-1  $(XEMACSDIR)/xemacs-packages/info/ess.info-1
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/info/ess.info-2  $(XEMACSDIR)/xemacs-packages/info/ess.info-2
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/info/ess.info-3  $(XEMACSDIR)/xemacs-packages/info/ess.info-3
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/info/ess.info-4  $(XEMACSDIR)/xemacs-packages/info/ess.info-4

## the rest of the targets are for ESS developer use
## --- PRE-release ---

dist: VERSION cleanup-dist
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
	-cvs export -D today ess
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
	$(MAKE) cleanup-dist
	touch $@

cleanup-dist:
	@echo "** Cleaning up **"
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) && rm -rf $(ESSDIR) ; fi)

ChangeLog: VERSION
	$(EMACSLOGCVS)
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@stat.math.ethz.ch>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	cvs commit -m 'Version .. released' ChangeLog

## --- RELEASE ---

# Note: we do not want to tag every minor release
#       ==> "tag" manually after `important' releases

# FIXME: "ChangeLog" does not work (for MM);
#	if it would it still needed cleanup of full-path names
#rel: ChangeLog dist
rel: dist
	@echo "** Placing tar and zip files **"
	scp -p $(ESSDIR).tar.gz $(ESSDIR).zip $(UPLOAD_SITE):$(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	-ssh $(UPLOAD_SITE) rm "$(UPLOAD_DIR)/LATEST.IS.*"
	ssh $(UPLOAD_SITE) touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)

tag:
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)

