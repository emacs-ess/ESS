## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

## This is the default target, i.e. 'make' and 'make all' are the same.

all install clean distclean:
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
#bad dog@for D in lisp doc etc; do cd $$D; $(MAKE) $@; cd ..; done

## the rest of the targets are for ESS developer's use only :

## --- PRE-release ---

# new target to create .tgz and .zip files only
# run in the foreground so you can accept the certificate
# for real men
# GNUTAR=gtar make downloads
downloads:
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSDIR)"
	@echo "** (must have setup subversion with cached authentication, prior for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	svn checkout --quiet $(SVN_URL)/trunk $(ESSDIR)-svn
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-svn; $(GNUTAR) cvf - --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); $(GNUTAR) xf - )
	@echo "** Clean-up docs, Make docs, and Correct Write Permissions **"
	CLEANUP="jcgs techrep dsc2001-rmh philasug user-* Why_* README.*"; \
	 cd $(ESSDIR)/doc; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 make all cleanaux ; cd ../..
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make*
	chmod u+w $(ESSDIR)/doc/Makefile $(ESSDIR)/lisp/Makefile
	chmod a-w $(ESSDIR)/lisp/*.el
#      should be newer than 'VERSION' :
	touch $(ESSDIR)/lisp/ess-cust.el
# Not really desirable in many cases -- commented 2008-11-24 (for ESS 5.3.9):
#	chmod a-w $(ESSDIR)/ChangeLog $(ESSDIR)/doc/*
	@echo "** Creating .tgz file **"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)
# Change of plans:  no longer think this is a good idea
# Rather, the improved installation docs for xemacs will serve us better
#	@echo "** Creating .tgz and .zip files for the XEmacs Package System **"
#	test -f $(ESSDIR)-xemacs-pkg.tgz && rm -rf $(ESSDIR)-xemacs-pkg.tgz || true
#	test -f $(ESSDIR)-xemacs-pkg.zip && rm -rf $(ESSDIR)-xemacs-pkg.zip || true
#	cd $(ESSDIR); mv etc ess; mkdir etc; mv ess etc; mkdir info; \
#	cp doc/info/ess.info info; mv lisp ess; mkdir lisp; mv ess lisp; \
#	$(GNUTAR) hcvofz ../$(ESSDIR)-xemacs-pkg.tgz etc info lisp; \
#	zip -r ../$(ESSDIR)-xemacs-pkg.zip etc info lisp; cd ..

dist: VERSION RPM.spec downloads
	cd doc;  $(MAKE) docs; cd ..
	cd lisp; $(MAKE) dist; grep 'ess-version' ess-cust.el; cd ..
	$(MAKE) cleanup-dist
	svn cleanup
	@echo "** Committing VERSION, README, ANNOUNCE, info etc **"
	svn commit -m "Updating toplevel files for new version" \
		VERSION README ANNOUNCE RPM.spec
	svn commit -m "Updating ess-version, info & html for new version" lisp/ess-cust.el doc/info doc/html
	$(MAKE) downloads
	$(MAKE) cleanup-dist
	touch $@

.PHONY: cleanup-dist cleanup-rel
cleanup-dist:
	@echo "** Cleaning up **"
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-svn && rm -rf $(ESSDIR) $(ESSDIR)-svn; fi)
##  should only be called manually (if at all):
cleanup-rel:
	@rm -f dist lisp/dist $(ESSDIR)*

%.spec: %.spec.in VERSION
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@


## --- RELEASE ---

## NB: Typically use  'make -W VERSION ChangeLog' before 'make rel'
##	since          ~~~~~~~~~~~~~~~~~~~~~~~~~
## 	ChangeLog often ends up newer than VERSION
ChangeLog: VERSION
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@stat.math.ethz.ch>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	@rm ChangeLog.old
	svn commit -m 'Version $(ESSVERSION)' ChangeLog

rel: ChangeLog dist tag homepage
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing .tgz and .zip files **"
	cp -p $(ESSDIR).tgz $(ESSDIR).zip $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)
	@echo "If all is perfect, eventually call   'make cleanup-rel'"

tag:
	@echo "** Tagging the release **"
	svn cp -m'release tagging' $(SVN_URL)/trunk $(SVN_URL)/tags/$(ESSVERSIONTAG)
homepage:
	@echo "** Updating ESS Webpage **"
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	cd $(ESS_HOMEPAGE); ./update-VERSION $(ESSVERSION)

## TODO (when MM has GPG set up properly): add this to 'rel'
.PHONY: buildrpm
buildrpm: dist
	rpmbuild -ta --sign $(ESSDIR).tgz

builddeb:
	dpkg-buildpackage -uc -us -rfakeroot -tc
