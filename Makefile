## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

.PHONY: all install uninstall
all install uninstall: version
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@

.PHONY: version
version:
	@echo "********************* VERSIONS **************************"
	@echo $(shell $(EMACS) --version | sed -n 1p)
	@echo ESS $(ESSVERSION)
	@echo git HEAD $(shell git rev-parse --short HEAD)
	@echo "*********************************************************"

.PHONY: lisp
lisp: version
	cd lisp; $(MAKE)

.PHONY: test
test: version
	cd test; $(EMACS) --script run-tests

generate-indent-cases:
	cd test; $(EMACS) --script generate-indent-cases

.PHONY: julia
julia:
	cd lisp; $(MAKE) julia-mode.el

.PHONY: autoloads
autoloads:
	cd lisp; $(MAKE) ess-autoloads.el

## the rest of the targets are for ESS developer's use only :

## --- PRE-release ---

# Create .tgz and .zip files only
# GNUTAR=gtar make tarballs
tarballs: $(ESSDIR)
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for (pre)release $(ESSVERSION) from $(ESSDIR)/"
	@echo "** Creating .tgz file **"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)
	touch $@

# Create the "release" directory
# run in the foreground so you can accept the certificate
$(ESSDIR): all RPM.spec cleanup-dist
	@echo "**********************************************************"
	@echo "** Making $(ESSDIR) directory of ESS for release $(ESSVERSION),"
	@echo "** (must have setup git / github with cached authentication, prior for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	git clone git@github.com:emacs-ess/ESS.git $(ESSDIR)-git
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-git; $(GNUTAR) cvf - --exclude=.git --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); $(GNUTAR) xf - )
	@echo "** Clean-up docs, Make docs, and Correct Write Permissions **"
	CLEANUP="user-* useR-* Why_* README.*"; ED=$(ESSDIR)/doc; \
	 if [ -d $$ED ] ; then CD=`pwd`; cd $$ED; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 $(MAKE) all cleanaux ; cd $$CD; fi
#	just in case: update from VERSION:
	cd lisp; $(MAKE) ess-custom.el; $(INSTALL) ess-custom.el ../$(ESSDIR)/lisp/; cd ..
	cd lisp; $(MAKE) julia-mode.el; $(INSTALL) julia-mode.el ../$(ESSDIR)/lisp/; cd ..
	$(INSTALL) RPM.spec $(ESSDIR)/
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make* $(ESSDIR)/*/Makefile
	touch $(ESSDIR)/etc/.IS.RELEASE
#	# Get (the first 12 hexdigits of) the git version into the release tarball:
	cut -c 1-12 $(ESSDIR)-git/.git/refs/heads/master > $(ESSDIR)/etc/git-ref

dist: VERSION tarballs
	grep -E 'defvar ess-(version|revision)' lisp/ess-custom.el \
	  $(ESSDIR)/lisp/ess-custom.el
	touch $@

.PHONY: cleanup-dist cleanup-rel dist
cleanup-dist:
	@echo "** Cleaning up **"
	rm -f $(ESSDIR)/etc/.IS.RELEASE $(ESSDIR)/etc/git-ref
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-git && rm -rf $(ESSDIR) $(ESSDIR)-git; fi)
##  should only be called manually (if at all):
cleanup-rel:
	@rm -f $(ESSDIR)*
##	@rm -f dist lisp/dist $(ESSDIR)*

%.spec: %.spec.in VERSION
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@


## --- RELEASE section ---

## NB: Typically use  'make -W VERSION ChangeLog' before 'make rel' <<--- MUST
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

tag:
	@echo "** Tagging the release **  1) pull existing;  2) tag  3) push it"
	git pull --tags
	git tag -a -m'release tagging' v$(ESSVERSION)
	@echo '** Pushing the 	v$(ESSVERSION)  upstream ...'
	git push origin v$(ESSVERSION)
	@touch $@

homepage:
	@echo "** Updating ESS Webpage **"
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	cd $(ESS_HOMEPAGE); ./update-VERSION $(ESSVERSION)
	@touch $@

upload:
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing .tgz and .zip files **"
	cp -p $(ESSDIR).tgz $(ESSDIR).zip $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)
	touch $@

#==== RELEASE : ====

rel: ChangeLog dist tag homepage upload
	@echo "If all is perfect, eventually call   'make cleanup-rel'"
	touch $@


## TODO (when MM has GPG set up properly): add this to 'rel'
.PHONY: buildrpm
buildrpm: dist
	rpmbuild -ta --sign $(ESSDIR).tgz

builddeb:
	dpkg-buildpackage -uc -us -rfakeroot -tc

## Old Note (clean and distclean are now the same):
## 'clean'     shall remove *exactly* those things that are *not* in version control
## 'distclean' removes also things in VC (svn, when they are remade by "make"):
clean distclean: cleanup-dist
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
#	rm -f etc/SVN-REVISION*
