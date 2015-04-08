## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

ETC_FILES = etc/SVN-REVISION # etc/ESSR-VERSION

## This is the default target, i.e. 'make' and 'make all' are the same.
all install uninstall: $(ETC_FILES)
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@

lisp: $(ETC_FILES)
	cd lisp; $(MAKE)

## the rest of the targets are for ESS developer's use only :

# VERSION:
# 	@echo "$(ESSVERSION)" > $@
## Hmm, this is a bit brittle ... but for distribution, there's no problem
etc/SVN-REVISION etc/SVN-REVISION-tmp: VERSION lisp/*.el doc/*.texi doc/Makefile etc/Makefile lisp/Makefile Makefile Makeconf
	(LC_ALL=C TZ=GMT svn info || $(ECHO) "Revision: unknown") 2> /dev/null \
	    | sed -n -e '/^Revision/p' -e '/^Last Changed Date/'p \
	    | cut -d' ' -f1,2,3,4 > $@-tmp
	if [ -s $@-tmp ]; then mv $@-tmp $@ ; elif [ ! -f $@ ]; then echo 'not available' > $@ ; fi

# etc/ESSR-VERSION: etc/ESSR/DESCRIPTION
# 	sed -n '/^Version: */{ s///; s/ *$$//p }' $< > $@


## --- PRE-release ---

# new target to create .tgz and .zip files only
# run in the foreground so you can accept the certificate
# for real men
# GNUTAR=gtar make downloads
downloads: all RPM.spec cleanup-dist
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSDIR)"
	@echo "** (must have setup git / github with cached authentication, prior for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	git clone git@github.com:emacs-ess/ESS.git $(ESSDIR)-git
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-git; $(GNUTAR) cvf - --exclude=.git --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); $(GNUTAR) xf - )
	@echo "** Clean-up docs, Make docs, and Correct Write Permissions **"
	CLEANUP="user-* useR-* Why_* README.*"; \
	 cd $(ESSDIR)/doc; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 $(MAKE) all cleanaux ; cd ../..
	 #svn svn cleanup
	cd lisp; $(MAKE) ess-custom.el; cp ess-custom.el ../$(ESSDIR)/lisp/; cd ..
         # touch: make it newer than VERSION in the tarball:
         #svn sr=etc/SVN-REVISION ; touch $$sr ; cp -p $$sr $(ESSDIR)/etc/
	cp -p RPM.spec $(ESSDIR)/
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make* $(ESSDIR)/*/Makefile
	@echo "** Creating .tgz file **"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)

dist: VERSION downloads
	grep -E 'defvar ess-(version|revision)' lisp/ess-custom.el \
	  $(ESSDIR)/lisp/ess-custom.el
##	touch $@

.PHONY: cleanup-dist cleanup-rel dist
cleanup-dist:
	@echo "** Cleaning up **"
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-git && rm -rf $(ESSDIR) $(ESSDIR)-git; fi)
##  should only be called manually (if at all):
cleanup-rel:
	@rm -f $(ESSDIR)*
##	@rm -f dist lisp/dist $(ESSDIR)*

%.spec: %.spec.in VERSION
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@


## --- RELEASE ---

## NB: Typically use  'make -W VERSION ChangeLog' before 'make rel'   <<---- MUST ---
##	since          ~~~~~~~~~~~~~~~~~~~~~~~~~
## 	ChangeLog often ends up newer than VERSION
ChangeLog: VERSION
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ESS-core@r-project.org>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	@rm ChangeLog.old
	git commit -m 'Version $(ESSVERSION)' ChangeLog

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
	git tag -m'release tagging' v$(ESSVERSION)
	 #svn cp -m'release tagging' $(SVN_URL)/trunk $(SVN_URL)/tags/$(ESSVERSION)
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
	rm -f etc/SVN-REVISION*
