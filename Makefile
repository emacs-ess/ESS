## Top Level Makefile

## Before making changes here, please take a look at Makeconf
include ./Makeconf

ESSVERSION := $(shell cat VERSION)
PKGVERSION := $(shell sed -n 's/;; Version: *\(.*\) */\1/p' lisp/ess.el)
ESSDIR := ess-$(ESSVERSION)
ifneq ($(ESSVERSION), $(PKGVERSION))
  $(shell sed -i 's/Version: .*/Version: $(ESSVERSION)/' VERSION)
  ${shell sed -i 's/;; Version: .*/;; Version: $(ESSVERSION)/' lisp/ess.el}
endif

ESSR-VERSION := $(shell sed -n "s/;; ESSR-Version: *\(.*\) */\1/p" lisp/ess.el)

.PHONY: all
all: lisp doc etc autoloads

.PHONY: version
version:
	@echo "********************* VERSIONS **************************"
	@echo $(shell $(EMACS) --version | sed -n 1p)
	@echo ESS $(ESSVERSION)
	@echo ESSR $(ESSR-VERSION)
	@echo "*********************************************************"

.PHONY: lisp
lisp: version
	$(MAKE) -C lisp all

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

.PHONY: julia
julia:
	@cd lisp; $(MAKE) julia-mode.el

.PHONY: autoloads
autoloads:
	cd lisp; $(MAKE) ess-autoloads.el

.PHONY: essr
essr: VERSION
	@echo "**********************************************************"
	@echo "** Making ESSRv$(ESSR-VERSION) **"
	@sed -i "s/^ *VERSION <- .*/    VERSION <- \"$(ESSR-VERSION)\"/" etc/ESSR/R/.load.R
	cd etc/ESSR/; ./BUILDESSR; cd -
	@git add etc/ESSR.rds lisp/ess.el etc/ESSR/R/.load.R
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
ess-$(ESSVERSION).tgz: $(ESSDIR)
	@echo "** Creating .tgz file **"
	test -f $(ESSDIR).tgz && rm -rf $(ESSDIR).tgz || true
	$(GNUTAR) hcvofz $(ESSDIR).tgz $(ESSDIR)

.PHONY: zip
zip: ess-$(ESSVERSION).zip
ess-$(ESSVERSION).zip: $(ESSDIR)
	@echo "** Creating .zip file **"
	test -f $(ESSDIR).zip && rm -rf $(ESSDIR).zip || true
	zip -r $(ESSDIR).zip $(ESSDIR)

.PHONY: package
package: ess-$(ESSVERSION).tar
ess-$(ESSVERSION).tar:
	@echo "Creating $@"
	@rm -rf $(ESSDIR)
	@git archive HEAD -o ess-$(ESSVERSION).tar
	@mkdir ess-$(ESSVERSION)
	@$(GNUTAR) -C ess-$(ESSVERSION) -xf ess-$(ESSVERSION).tar
	@cd ess-$(ESSVERSION) && $(EMACS) -Q --script "targets/create-pkg-file.el"
	@$(GNUTAR) c -f ess-$(ESSVERSION).tar ess-$(ESSVERSION)
	@rm -rf ess-$(ESSVERSION)/

# Create the "release" directory
# run in the foreground so you can accept the certificate
# NB 'all', 'cleanup-dist' must not be targets: otherwise, e.g.
#    'make tarball' re-builds the tarballs always!
$(ESSDIR): RPM.spec
	$(MAKE) all
#	remove previous ESSDIR, etc:
	$(MAKE) cleanup-dist
	@echo "** Exporting Files **"
	git clone . $(ESSDIR)-git
	mkdir -p $(ESSDIR)
	(cd $(ESSDIR)-git; $(GNUTAR) cvf - --exclude=.git --exclude=.svn --no-wildcards .) | (cd $(ESSDIR); $(GNUTAR) xf - )
	@echo "** Clean-up docs, Make docs, and Correct Write Permissions **"
	CLEANUP="user-* useR-* Why_* README.*"; ED=$(ESSDIR)/doc; \
	 if [ -d $$ED ] ; then CD=`pwd`; cd $$ED; chmod -R u+w $$CLEANUP; rm -rf $$CLEANUP; \
	 $(MAKE) all cleanaux ; cd $$CD; fi
#   just in case: update from VERSION:
	cd lisp; $(INSTALL) ess.el ../$(ESSDIR)/lisp/; cd ..
	cd lisp; $(MAKE) julia-mode.el; $(INSTALL) julia-mode.el ../$(ESSDIR)/lisp/; cd ..
	$(INSTALL) RPM.spec $(ESSDIR)/
	chmod a-w $(ESSDIR)/lisp/*.el
	chmod u+w $(ESSDIR)/lisp/ess-site.el $(ESSDIR)/Make* $(ESSDIR)/*/Makefile
	touch $(ESSDIR)/etc/.IS.RELEASE
#	# Get (the first 12 hexdigits of) the git version into the release tarball:
	$(shell git-rev-parse master | cut -c 1-12 ) > $(ESSDIR)/etc/git-ref

dist: VERSION tarballs
	@echo "** Making pdf and html documentation"
	@cd $(ESSDIR)/doc/ ; $(MAKE) pdf
	@cd $(ESSDIR)/doc/ ; $(MAKE) html
	grep -E 'defvar ess-(version|revision)' lisp/ess-custom.el \
	  $(ESSDIR)/lisp/ess-custom.el > $@

.PHONY: cleanup-dist cleanup-rel
cleanup-dist:
	@echo "** Cleaning up **"
	rm -f $(ESSDIR)/etc/.IS.RELEASE $(ESSDIR)/etc/git-ref
	(if [ -d $(ESSDIR) ] ; then \
	  chmod -R u+w $(ESSDIR) $(ESSDIR)-git && rm -rf $(ESSDIR) $(ESSDIR)-git; fi)

##  should only be called manually (if at all):
cleanup-rel:
#	@rm -rf $(ESSDIR)*
	@rm -f tarballs dist tag homepage upload rel

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

rel: ChangeLog dist tag homepage upload
	@echo "If all is perfect, eventually call   'make cleanup-rel'"
	touch $@


## NB: The rpm (SuSE, RH, FC) and debian packages are built *and* signed
##     by the down stream maintainers:
.PHONY: buildrpm
buildrpm: dist
	rpmbuild -ta --sign $(ESSDIR).tgz

builddeb:
	dpkg-buildpackage -uc -us -rfakeroot -tc

clean distclean: cleanup-dist
	rm -f ess-$(ESSVERSION).tar
	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@
	cd doc; $(MAKE) $@
