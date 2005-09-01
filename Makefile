## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

## This is the default target, i.e. 'make' and 'make all' are the same.

all install clean distclean:
	@for D in lisp doc etc; do cd $$D; $(MAKE) $@; cd ..; done

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
#      should be newer than 'VERSION' :
	touch $(ESSDIR)/lisp/ess-cust.el
	chmod a-w $(ESSDIR)/ChangeLog $(ESSDIR)/doc/*
	test -f $(ESSDIR).tar.gz && rm -rf $(ESSDIR).tar.gz || true
	@echo "** Creating tar file **"
	tar hcvofz $(ESSDIR).tar.gz $(ESSDIR)
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	@echo "** Creating zip file **"
	zip -r $(ESSDIR).zip $(ESSDIR)
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

ChangeLog: VERSION
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@stat.math.ethz.ch>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	@rm ChangeLog.old
	svn commit -m 'Version $(ESSVERSION)' ChangeLog

rel: ChangeLog dist tag
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing tar and zip files **"
	cp -p $(ESSDIR).tar.gz $(ESSDIR).zip   $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)
	@echo "If all is perfect, eventually call   'make cleanup-rel'"

tag:
	@echo "** Tagging the release **"
	svn cp -m'release tagging' $(SVN_URL)/trunk $(SVN_URL)/tags/$(ESSVERSIONTAG)

## TODO (when MM has GPG set up properly): add this to 'rel'
.PHONY: buildrpm
buildrpm: dist
	rpmbuild -ta --sign $(ESSDIR).tar.gz
