## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

## This is the default target, i.e. 'make' and 'make all' are the same.
all install: VERSION
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@


## the rest of the targets are for ESS developer's use only :

VERSION:
	@echo "$(ESSVERSION)" > $@
## manually
VERSION+:
	echo "$(ESSVERSIONsvn)" > VERSION


## --- PRE-release ---

# new target to create .tgz and .zip files only
# run in the foreground so you can accept the certificate
# for real men
# GNUTAR=gtar make downloads
downloads: all RPM.spec cleanup-dist
	[ x$(ESSfromSVN) = xyes ] || (echo 'ESS must be "from SVN"'; exit 1 )
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
	CLEANUP="jcgs techrep dsc2001-rmh philasug user-* useR-* Why_* README.*"; \
	 cd $(ESSDIR)/doc; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 $(MAKE) all cleanaux ; cd ../..
	svn cleanup
## ugly hack; otherwise get ess-revision "12-04-rexported":
	cd lisp; $(MAKE) -W ../VERSION ess-custom.el; cp ess-custom.el ../$(ESSDIR)/lisp/; cd ..
	cd $(ESSDIR)/lisp; $(MAKE) ess-custom.el; fgrep ess-revision ess-custom.el; cd ../..
	cp -p RPM.spec $(ESSDIR)/
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make* $(ESSDIR)/*/Makefile
	@echo "** Creating .tgz file **"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)

dist: downloads
	grep -E 'defvar ess-(version|revision)' lisp/ess-custom.el \
	  $(ESSDIR)/lisp/ess-custom.el
	touch $@

.PHONY: cleanup-dist cleanup-rel
cleanup-dist:
	@echo "** Cleaning up **"
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-svn && rm -rf $(ESSDIR) $(ESSDIR)-svn; fi)
##  should only be called manually (if at all):
cleanup-rel:
	@rm -f dist lisp/dist $(ESSDIR)*

%.spec: %.spec.in
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@


## --- RELEASE ---

ChangeLog:
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@r-project.org>" ; \
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


## 'clean'     shall remove *exactly* those things that are *not* in version control
## 'distclean' removes also things in VC (svn, when they are remade by "make"):
clean distclean: cleanup-dist
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
	rm -f VERSION dist
