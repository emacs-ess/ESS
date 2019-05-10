### Makefile - for compiled e-lisp of ESS distribution.

## Before making changes here, please take a look at Makeconf
include ../Makeconf


### Targets

.PHONY: all dist install uninstall distclean clean compile

JULIAS := julia-mode.el julia-latexsubs.el
ELS = $(filter-out ess-autoloads.el, $(wildcard *.el obsolete/*.el)) $(JULIAS)
ELC = $(ELS:.el=.elc)

all dist: compile ess-autoloads.el

compile: $(ELC)

install: dist
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) $(ELS) ess-autoloads.el $(LISPDIR)
	$(INSTALL) $(ELC) $(LISPDIR)
	if [ -f /etc/debian_version -a -n "$(SITELISP)" -a ! -f "$(SITELISP)/ess-site.el" ] ; \
	then \
		ln -s $(LISPDIR)/ess-site.el $(SITELISP)/ess-site.el ; \
	fi;

distclean clean:
	rm -f $(ELC) ess-autoloads.el $(JULIAS) .dependencies


### File Targets

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
obsolete/%.elc: obsolete/%.el
	$(COMPILE-SIMPLE) $<

JULIA-REPO=https://raw.githubusercontent.com/JuliaEditorSupport/julia-emacs/master
## Should happen before building ESS; definitely *NOT* after unpacking tarball :
$(JULIAS):
	test -f ../etc/.IS.RELEASE || $(DOWNLOAD) $(JULIA-REPO)/julia-mode.el > julia-mode.el
	test -f ../etc/.IS.RELEASE || $(DOWNLOAD) $(JULIA-REPO)/julia-latexsubs.el > julia-latexsubs.el
julia-%.elc: julia-%.el
	$(COMPILE-SIMPLE) $<

ess-autoloads.el:
	@printf	"\nGenerating $@\n"
	$(EMACSBATCH) --eval "(progn\
	(setq make-backup-files nil)\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

