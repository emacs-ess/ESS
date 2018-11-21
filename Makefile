## Top Level Makefile

include ./Makeconf

.DEFAULT_GOAL := all

ESSVERSION := $(shell cat VERSION)
PKGVERSION := $(shell sed -n 's/;; Version: *\(.*\) */\1/p' lisp/ess.el)
ESSDIR := ess-$(ESSVERSION)
ifneq ($(ESSVERSION), $(PKGVERSION))
  $(shell sed -i 's/Version: .*/Version: $(ESSVERSION)/' VERSION)
  ${shell sed -i 's/;; Version: .*/;; Version: $(ESSVERSION)/' lisp/ess.el}
  ${shell sed -i 's/(defconst ess-version .*/(defconst ess-version "$(ESSVERSION)"/' lisp/ess.el}
endif

ESSR-VERSION := $(shell sed -n "s/;; ESSR-Version: *\(.*\) */\1/p" lisp/ess.el)
.PHONY: all install uninstall
all install uninstall:
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
	cd etc; $(MAKE) $@

.PHONY: version
version:
	@echo "********************* VERSIONS **************************"
	@echo $(shell $(EMACS) --version | sed -n 1p)
	@echo ESS $(ESSVERSION)
	@echo ESSR $(ESSR-VERSION)
	@echo "*********************************************************"

.PHONY: lisp
lisp: version
	cd lisp; $(MAKE)

.PHONY: doc
doc: version
	cd doc; $(MAKE)

.PHONY: etc
etc: version
	cd etc; $(MAKE)

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

.PHONY: essr
essr: VERSION
	@echo "**********************************************************"
	@echo "** Making ESSRv$(ESSR-VERSION) **"
	@sed -i "s/^ *VERSION <- .*/    VERSION <- \"$(ESSR-VERSION)\"/" etc/ESSR/R/.load.R
	@sed -i "s/(defconst essr-version .*/(defconst essr-version \"$(ESSR-VERSION)\"/" lisp/ess.el
	@cd etc/ESSR/; ./BUILDESSR; cd -
	@git add etc/ESSR.rds lisp/ess.el etc/ESSR/R/.load.R
	git commit -m"ESSR Version $(ESSR-VERSION)"
	git tag "ESSRv"$(ESSR-VERSION)


## the rest of the targets are for ESS developer's use only :

# Create .tgz and .zip files only
# GNUTAR=gtar make tarballs
.PHONY: rel-tarballs
rel-tarballs: rel-staging
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for (pre)release $(ESSVERSION) from $(ESSDIR)/"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	@echo "** Creating .tgz file **"
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)
	@echo "Signing tgz file"
	-$(GPG) -ba -o $(ESSDIR).tgz.sig $(ESSDIR).tgz
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)
	@echo "Signing zip file"
	-$(GPG) -ba -o $(ESSDIR).zip.sig $(ESSDIR).zip

# Create the "release" directory
# run in the foreground so you can accept the certificate
# NB 'all', 'cleanup-rel' must not be targets: otherwise, e.g.
#    'make tarball' re-builds the tarballs always!  (??? -- dickmao)
.PHONY: rel-staging
rel-staging: cleanup-rel
	@echo "**********************************************************"
	@echo "** Making $(ESSDIR) directory of ESS for release $(ESSVERSION),"
	@echo "** (must have setup git / github with cached authentication, prior for security)"
	@echo "**********************************************************"
	@echo "** Exporting Files **"
	mkdir -p $(ESSDIR)-git
	stash=`git stash create`; git archive --format tar $${stash:-HEAD} | (cd $(ESSDIR)-git ; $(GNUTAR) xf -)
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-git; $(GNUTAR) cvf - --exclude=.git --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); $(GNUTAR) xf - )
	cd $(ESSDIR) ; $(MAKE) dist
	cd $(ESSDIR)/doc ; $(MAKE) cleanaux
	cd lisp; $(MAKE) ess-custom.el; $(INSTALL) ess-custom.el ../$(ESSDIR)/lisp/
	cd lisp; $(MAKE) julia-mode.el; $(INSTALL) julia-mode.el ../$(ESSDIR)/lisp/
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make* $(ESSDIR)/*/Makefile
	touch $(ESSDIR)/etc/.IS.RELEASE
#	# Get (the first 12 hexdigits of) the git version into the release tarball:
	if git diff-index --quiet HEAD -- ; then \
	  commit=`git stash create | cut -c 1-12` ; \
	else \
	  commit=`git rev-parse HEAD | cut -c 1-12` ; \
	fi ; echo $commit > $(ESSDIR)/etc/git-ref

dist:
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@

.PHONY: cleanup-rel
cleanup-rel:
	@echo "** Cleaning up **"
	rm -f $(ESSDIR).tgz* $(ESSDIR).zip*
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-git && rm -rf $(ESSDIR) $(ESSDIR)-git; fi)

%.spec: %.spec.in VERSION
	sed 's/@@VERSION@@/$(ESSVERSION)/g' $< > $@

## --- RELEASE section ---

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
	@echo "Creating tag (no signing, as that fails for MM)"
	git tag -m'release tagging' v$(ESSVERSION)
	@echo '** Pushing the 	v$(ESSVERSION)  upstream ...'
	git push origin v$(ESSVERSION)
	@touch $@

# signing fails for MM (gpg2 / gpg ??) --> use a non-signed tag above

# @echo "Creating tag and signing using $(GPG)"
# git tag -s -m'release tagging' v$(ESSVERSION)

homepage:
	@echo "** Updating ESS Webpage **"
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	cd $(ESS_HOMEPAGE); ./update-VERSION $(ESSVERSION)
	@touch $@

upload:
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing .tgz and .zip files and their .sig's **"
	cp -p $(ESSDIR).tgz $(ESSDIR).tgz.sig $(ESSDIR).zip $(ESSDIR).zip.sig $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)
	touch $@

#==== RELEASE : ====

rel: ChangeLog rel-tarballs tag homepage upload
	@echo "If all is perfect, eventually call   'make cleanup-rel'"
	touch $@

## Old Note (clean and distclean are now the same):
## 'clean'     shall remove *exactly* those things that are *not* in version control
## 'distclean' removes also things in VC (svn, when they are remade by "make"):
clean distclean: cleanup-rel
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
