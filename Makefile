## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

Subdirs = lisp doc

## This is the default target, i.e. 'make' and 'make all' are the same.

all install clean distclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@; cd ..; done

#ESS is now an official xemacs-package, but the xemacs-links target
#persists since there is generally a lag between an ESS release and
#the corresponding xemacs-package release
#Previous ESS xemacs-package installation is stored prev-ver-ess
xemacs-links: 
	test -d $(XEMACSDIR)/xemacs-packages/prev-ver-ess/etc || mkdir -p $(XEMACSDIR)/xemacs-packages/prev-ver-ess/etc
	-mv -f $(XEMACSDIR)/xemacs-packages/etc/ess*  $(XEMACSDIR)/xemacs-packages/prev-ver-ess/etc 
	test -d $(XEMACSDIR)/xemacs-packages/prev-ver-ess/lisp || mkdir $(XEMACSDIR)/xemacs-packages/prev-ver-ess/lisp
	-mv -f $(XEMACSDIR)/xemacs-packages/lisp/ess* $(XEMACSDIR)/xemacs-packages/prev-ver-ess/lisp 
	test -d $(XEMACSDIR)/xemacs-packages/prev-ver-ess/info || mkdir $(XEMACSDIR)/xemacs-packages/prev-ver-ess/info
	-mv -f $(XEMACSDIR)/xemacs-packages/info/ess.info* $(XEMACSDIR)/xemacs-packages/prev-ver-ess/info 
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/etc                  $(XEMACSDIR)/xemacs-packages/etc/ess
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/lisp                 $(XEMACSDIR)/xemacs-packages/lisp/ess
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/info/ess.info    $(XEMACSDIR)/xemacs-packages/info/ess.info
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/info/ess.info-1  $(XEMACSDIR)/xemacs-packages/info/ess.info-1
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/info/ess.info-2  $(XEMACSDIR)/xemacs-packages/info/ess.info-2
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/info/ess.info-3  $(XEMACSDIR)/xemacs-packages/info/ess.info-3
	ln -s $(ESSDIR)/$(ESSVERSIONDIR)/doc/info/ess.info-4  $(XEMACSDIR)/xemacs-packages/info/ess.info-4

## the rest of the targets are for ESS developer's use only :

## --- PRE-release ---

dist: VERSION RPM.spec
	cd doc;  $(MAKE) docs; cd ..
	cd lisp; $(MAKE) dist; grep 'ess-version' ess-cust.el; cd ..
	$(MAKE) cleanup-dist
	svn cleanup
	@echo "** Committing VERSION, README, ANNOUNCE, info etc **"
	svn commit -m "Updating toplevel files for new version" \
		VERSION README ANNOUNCE RPM.spec
	svn commit -m "Updating ess-version, info & html for new version" lisp/ess-cust.el doc/info doc/html
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSDIR)"
	@echo "** (must have setup subversion with cached authentication, prior for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	svn checkout --quiet $(SVN_URL)/trunk $(ESSDIR)-svn
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-svn; tar cvf - --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); tar xf - )
	@echo "** Clean-up docs, Make docs, and Correct Write Permissions **"
	CLEANUP="jcgs techrep dsc2001-rmh philasug user-* Why_* README.*"; \
	 cd $(ESSDIR)/doc; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 make all cleanaux ; cd ../..
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make*
	chmod u+w $(ESSDIR)/doc/Makefile $(ESSDIR)/lisp/Makefile
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod a-w $(ESSDIR)/ChangeLog $(ESSDIR)/doc/*
	test -f $(ESSDIR).tar.gz && rm -rf $(ESSDIR).tar.gz || true
	@echo "** Creating tar file **"
	tar hcvofz $(ESSDIR).tar.gz $(ESSDIR)
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	@echo "** Creating zip file **"
	zip -r $(ESSDIR).zip $(ESSDIR)
	$(MAKE) cleanup-dist
	touch $@

.PHONY: cleanup-dist
cleanup-dist:
	@echo "** Cleaning up **"
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-svn && rm -rf $(ESSDIR) $(ESSDIR)-svn; fi)

%.spec: %.spec.in VERSION
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@


## --- RELEASE ---

ChangeLog: VERSION
#	$(EMACSBATCH) $(EMACSLOGCVS)
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@stat.math.ethz.ch>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	svn commit -m 'Version $(ESSVERSION)' ChangeLog

rel: ChangeLog dist tag
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing tar and zip files **"
	cp -p $(ESSDIR).tar.gz $(ESSDIR).zip   $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)

tag:
	@echo "** Tagging the release **"
	svn cp -m'release tagging' $(SVN_URL)/trunk $(SVN_URL)/tags/$(ESSVERSIONTAG)

## TODO (when MM has GPG set up properly): add this to 'rel'
.PHONY: buildrpm
buildrpm: dist
	rpmbuild -ta --sign $(ESSDIR).tar.gz
