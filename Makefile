include ./Makeconf

ESSVERSION := $(shell cat VERSION)
PKGVERSION := $(shell sed -n 's/;; Version: *\(.*\) */\1/p' ess.el)
ifneq ($(ESSVERSION), $(PKGVERSION))
  $(shell sed -i 's/Version: .*/Version: $(ESSVERSION)/' VERSION)
  ${shell sed -i 's/;; Version: .*/;; Version: $(ESSVERSION)/' ess.el}
  ${shell sed -i 's/(defconst ess-version .*/(defconst ess-version "$(ESSVERSION)"/' ess.el}
endif

ESSR-VERSION := $(shell sed -n "s/;; ESSR-Version: *\(.*\) */\1/p" ess.el)

.PHONY: all
all: lisp ess-autoloads.el
	$(MAKE) -C doc ../README
	$(MAKE) -C doc all
	$(MAKE) -C etc all

.PHONY: help
help:
	$(info all: Compile lisp, ess-autoloads.el, and make documentation)
	$(info version: Show version information)
	$(info doc: Make documentation)
	$(info test: Perform tests)
	$(info ess-autoloads.el: Make autoloads file)
	$(info )
	$(info tgz: Make tgz file of version)
	$(info zip: Make zip file of version)
	$(info package: Make package.el-installable tar file of version)
	@printf "\n"

.PHONY: version
version:
	@echo "********************* VERSIONS **************************"
	@echo $(shell $(EMACS) --version | sed -n 1p)
	@echo ESS $(ESSVERSION)
	@echo ESSR $(ESSR-VERSION)
	@echo "*********************************************************"

ELS = $(filter-out ess-autoloads.el, $(wildcard *.el obsolete/*.el))
ELC = $(ELS:.el=.elc)

.dependencies: $(ELS)
	@echo Computing dependencies
	@rm -f .dependencies
	@for f in $(ELS); do \
	  sed -n "s|^(require '\(ess.*\)).*$$|$${f}c: \1.elc|p" $${f} >> .dependencies;\
	done
	@for f in obsolete/*.el; do \
	  echo "$$(basename $${f}c):$${f}c" >> .dependencies;\
	done

-include .dependencies

.el.elc:
	$(COMPILE) $<

.PHONY: lisp
lisp: version $(ELC)

.PHONY: doc
doc: version
	$(MAKE) -C doc all

.PHONY: etc
etc: version
	$(MAKE) -C etc all

.PHONY: test
test: version
	$(MAKE) -C test all

test-%: version
	$(MAKE) -C test $*

ess-autoloads.el:
	@printf	"\nGenerating $@\n"
	$(EMACSBATCH) --eval "(progn\
	(setq make-backup-files nil)\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

.PHONY: essr
essr: VERSION
	@echo "**********************************************************"
	@echo "** Making ESSRv$(ESSR-VERSION) **"
	@sed -i "s/^ *VERSION <- .*/    VERSION <- \"$(ESSR-VERSION)\"/" etc/ESSR/R/.load.R
	@sed -i "s/(defconst essr-version .*/(defconst essr-version \"$(ESSR-VERSION)\"/" ess.el
	cd etc/ESSR/; ./BUILDESSR; cd -
	@git add etc/ESSR.rds ess.el etc/ESSR/R/.load.R
	git commit -m"ESSR Version $(ESSR-VERSION)"
	git tag "ESSRv"$(ESSR-VERSION)

install: all
	mkdir $(ESSDIR)
	$(INSTALL) -R ./* $(ESSDIR)/

uninstall:
	rm -rf $(ESSDIR)


## the rest of the targets are for ESS developer's use only :
.PHONY: tarballs sign-tarballs
TARBALLS = ess-$(ESSVERSION).tar ess-$(ESSVERSION).tgz ess-$(ESSVERSION).zip # TODO: ess-plus-$(VERSION).tar
tarballs: $(TARBALLS)

SIGNED_TARBALLS = $(addsuffix .sig, $(TARBALLS))
sign-tarballs: $(SIGNED_TARBALLS)
$(SIGNED_TARBALLS): $(TARBALLS)
	@echo "Signing $@"
	$(GPG) -ba -o $@ $<

.PHONY: tgz
tgz: ess-$(ESSVERSION).tgz
ess-$(ESSVERSION).tgz:
	@echo "** Creating $@ **"
	git archive -o $@ HEAD

.PHONY: zip
zip: ess-$(ESSVERSION).zip
ess-$(ESSVERSION).zip:
	@echo "** Creating $@ **"
	git archive -o $@ HEAD

.PHONY: package
package: ess-$(ESSVERSION).tar
ess-$(ESSVERSION).tar:
	@echo "Creating $@"
	@git archive HEAD -o ess-$(ESSVERSION).tar
	@mkdir ess-$(ESSVERSION)
	@$(GNUTAR) -C ess-$(ESSVERSION) -xf ess-$(ESSVERSION).tar
	@cd ess-$(ESSVERSION) && $(EMACS) -Q --script "targets/create-pkg-file.el"
	@$(GNUTAR) c -f ess-$(ESSVERSION).tar ess-$(ESSVERSION)
	@rm -rf ess-$(ESSVERSION)/

## --- RELEASE section ---
tag:
	@echo "** Tagging the release **  1) pull existing;  2) tag  3) push it"
	git pull --tags
	@echo "Creating tag (no signing, as that fails for MM)"
	git tag -m'release tagging' v$(ESSVERSION)
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
	@echo "** Placing .tgz and .zip files and their .sig's **"
	cp -p ess-$(ESSVERSION).tgz $(ESSVERSION).tgz.sig $(ESSVERSION).zip $(ESSVERSION).zip.sig $(UPLOAD_DIR)
	@echo "** Creating LATEST.IS. file **"
	rm -f $(UPLOAD_DIR)/LATEST.IS.*
	touch $(UPLOAD_DIR)/LATEST.IS.$(ESSDIR)
	touch $@

rel: tag homepage upload
	touch $@

clean distclean:
	rm .dependencies
	rm -f ess*.elc
	rm -f ess-*.tar
	rm -f ess-*.tgz
	rm -f ess-*.zip
	$(MAKE) -C etc clean
	$(MAKE) -C doc clean
