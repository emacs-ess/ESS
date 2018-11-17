### Makefile - for compiled e-lisp of ESS distribution.

## Before making changes here, please take a look at Makeconf
include ../Makeconf

## For noweb extraction of code and documentation.

NOTANGLE=notangle -L
#NOTANGLE=notangle for no indexing.
NOWEAVE=noweave

## SUFFIXES are in ../Makeconf :
.nw.html: ;     $(NOWEAVE) -filter l2h -index -html $*.nw > $*.html
.nw.tex: ;      $(NOWEAVE) -index -delay $*.nw | cpif $*.tex
.nw.el:  ;      $(NOTANGLE) -R$*.el | cpif $*.el
##.nw.c:  ;       $(NOTANGLE) -R$*.c | cpif $*.c
##.nw.h:  ;       $(NOTANGLE) -R$*.h | cpif $*.h
.tex.dvi: ;     latex '\scrollmode \input '"$*"; while grep -s 'Rerun to get cross-references right' $*.log; do latex '\scrollmode \input '"$*"; done

# Have to preload the files that contain macro definitions or the byte compiler
# will compile everything that references them incorrectly. Also preload a file
# that sets byte compiler options.
COMPILE = $(EMACSBATCH) --directory . -f batch-byte-compile


### Targets

ELS = $(filter-out ess-autoloads.el, $(wildcard *.el obsolete/*.el))
ELC = $(ELS:.el=.elc)

.PHONY: all dist install uninstall distclean clean

all dist: julia-mode.elc $(ELC) ess-autoloads.el

install: dist
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) $(ELS) ess-autoloads.el $(LISPDIR)
	$(INSTALL) $(ELC) $(LISPDIR)
	if [ -f /etc/debian_version -a -n "$(SITELISP)" -a ! -f "$(SITELISP)/ess-site.el" ] ; \
	then \
		ln -s $(LISPDIR)/ess-site.el $(SITELISP)/ess-site.el ; \
	fi;

uninstall:
	-cd $(LISPDIR) && $(UNINSTALL) *.elc *.el
	if [ -n "$(SITELISP)" -a -h "$(SITELISP)/ess-site.el" ] ; \
	then \
		cd $(SITELISP) && $(UNINSTALL) ess-site.el ; \
	fi;

distclean clean:
	rm -f $(ELC) ess-autoloads.el julia-mode.el .dependencies


### File Targets

.dependencies: $(ELS)
	@echo Computing dependencies
	@rm -f .dependencies
	@for f in $(ELS); do \
	  sed -n "s/^(require '\(ess[^)]*\)).*$$/$${f}c: \1.elc/p" $${f} >> .dependencies;\
	done

-include .dependencies

.el.elc:
	$(COMPILE) $<

## Should happen before building ESS; definitely *NOT* after unpacking tarball :
julia-mode.el:
	test -f ../etc/.IS.RELEASE || $(DOWNLOAD) \
	https://raw.githubusercontent.com/JuliaEditorSupport/julia-emacs/master/julia-mode.el > $@

ess-julia.elc: julia-mode.el

ess-autoloads.el:
	@printf	"\nGenerating $@\n"
	$(EMACSBATCH) --eval "(progn\
	(setq make-backup-files nil)\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

# Use this to print an envvar for debugging purposes.
# Example: make print-EMACS
print-%: ; @echo $* = $($*)
