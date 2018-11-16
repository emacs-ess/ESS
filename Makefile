## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

## 'all' is the default target, i.e. 'make' and 'make all' are the same.
.PHONY: all install uninstall
all install uninstall: $(ETC_FILES)
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
	cd etc; $(MAKE) $@

.PHONY: version
version:
	@echo "********************* VERSIONS **************************"
	@echo $(shell $(EMACS) --version | sed -n 1p)
	@echo ESS $(ESSVERSION)
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
test: version julia
	cd test; $(EMACS) --script run-tests

generate-indent-cases:
	cd test; $(EMACS) --script generate-indent-cases

.PHONY: julia
julia:
	cd lisp; $(MAKE) julia-mode.el

.PHONY: autoloads
autoloads:
	cd lisp; $(MAKE) ess-autoloads.el

.PHONY: dist
dist:
#	mmaechler, this grep must succeed, but why?
	grep -E 'defvar ess-(version|revision)' lisp/ess-custom.el
	cd doc ; $(MAKE) dist

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


.PHONY: tag
tag:
	@echo "** Tagging the release **  1) pull existing;  2) tag  3) push it"
	git pull --tags
	@echo "Creating tag (no signing, as that fails for MM)"
	git tag -m'release tagging' v$(ESSVERSION)
	@echo '** Pushing the 	v$(ESSVERSION)  upstream ...'
	git push origin v$(ESSVERSION)

# signing fails for MM (gpg2 / gpg ??) --> use a non-signed tag above

# @echo "Creating tag and signing using $(GPG)"
# git tag -s -m'release tagging' v$(ESSVERSION)

.PHONY: homepage
homepage:
	@echo "** Updating ESS Webpage **"
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	cd $(ESS_HOMEPAGE); ./update-VERSION $(ESSVERSION)

.PHONY: upload
upload:
	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
	@echo "** Placing .tgz and .zip files and their .sig's **"
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*

.PHONY: rel
rel: ChangeLog dist tag homepage upload
	@echo "If all is perfect, eventually call   'make cleanup-rel'"

.PHONY: clean distclean
clean distclean:
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
