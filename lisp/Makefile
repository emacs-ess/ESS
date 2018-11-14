### Makefile - for compiled e-lisp of ESS distribution.
###

include ../Makeconf
LOCAL_CONF := $(strip $(wildcard ../Makeconf.local))
ifeq ($(LOCAL_CONF),)
$(error "No ../Makeconf.local found.  Please read INSTALL")
else
include ../Makeconf.local
endif

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

ELS = $(wildcard *.el obsolete/*.el)
ELC = $(ELS:.el=.elc)

PKBUILD:=2.3

# ESSR_VER =`cat ../etc/ESSR-VERSION`
##ESSR_VER = $(shell cat ../etc/ESSR-VERSION)

### Targets

all: julia-mode.el $(ELC) ess-custom.el ess-autoloads.el

.PHONY: dist
dist: all

dist/package.out: 
	$(INSTALLDIR) dist
	if [ ! -s "dist/$(PKBUILD).tar.gz" ] ; then \
	  cd dist ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd dist ; tar xfz $(PKBUILD).tar.gz
	cd dist/package-build-$(PKBUILD) ; make -s loaddefs
	$(INSTALLDIR) dist/recipes
	if [ ! -s "dist/recipes/ess" ] ; then \
	  cd dist/recipes ; curl -sLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/ess ; fi
	$(EMACSBATCH) -L dist/package-build-$(PKBUILD) -l tmp/ess-package.el > dist/package.out 2>&1

.PHONY: install-dry
install-dry:
	-rm -f dist/package.out
	$(MAKE) install-dry-1

install-dry-1: dist/package.out
	@! ( cat dist/package.out | egrep -a "Error: " )

install: dist install-dry ess-autoloads.el
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) dist/ess-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].*/ess-pkg.el $(LISPDIR)
	$(INSTALL) $(ELS) $(LISPDIR)
	$(INSTALL) $(filter-out ess-autoloads.elc, $(ELC)) $(LISPDIR)
	if [ -f /etc/debian_version -a -n "$(SITELISP)" -a ! -f "$(SITELISP)/ess-site.el" ] ; \
	then \
		ln -s $(LISPDIR)/ess-site.el $(SITELISP)/ess-site.el ; \
	fi;
#	test "$(LISPDIR)" = "." || $(INSTALL) *.elc $(LISPDIR)

uninstall:
	-cd $(LISPDIR) && $(UNINSTALL) *.elc *.el
	if [ -n "$(SITELISP)" -a -h "$(SITELISP)/ess-site.el" ] ; \
	then \
		cd $(SITELISP) && $(UNINSTALL) ess-site.el ; \
	fi;

clean:
	rm -f $(ELC)
	rm -f ess-autoloads.el
	rm -rf dist

distclean: clean

# and potentially more

### Targets below here are only for developers - and these must have perl

### Replace Version numbers (as in ../doc/Makefile !):
## "../VERSION" if that changes, *definitely* need to update ess-custom.el
ess-custom.el: ../VERSION
	EV='$(ESSVERSION)'; perl -pi -e "s/\".*\"/\"$$EV\"/ if /ess-version/" $@

# ess-r-mode.el: ../etc/ESSR-VERSION
# 	sed -i -e '/let.*ESSR-version/s/"[-.0-9]*"/"$(ESSR_VER)"/' $@

## Should happen before building ESS; definitely *NOT* after unpacking tarball :
julia-mode.el:
	test -f ../etc/.IS.RELEASE || $(DOWNLOAD) \
	https://raw.githubusercontent.com/JuliaEditorSupport/julia-emacs/master/julia-mode.el > $@

ess-julia.elc: julia-mode.elc

ess-autoloads.el: $(filter-out ess-autoloads.el, $(ELS))
	$(EMACSBATCH) --eval "(package-initialize)" --eval "(package-generate-autoloads \"ess\" \".\")"

### File Dependencies

.el.elc:
	$(COMPILE) $<

ess-custom.elc: ess-custom.el
	$(COMPILE) ess-custom.el

ess.elc: ess.el ess-custom.elc
	$(COMPILE) ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el $(CORE)
# 	@echo " "
# 	@echo "For the next compile, please disregard the messages"
# 	@echo " ** the function set-keymap-parent is not known to be defined."
# 	@echo " ** assignment to free variable comint-last-input-end"
# 	@echo "from the byte compiler if they occur. It is completely normal."
	$(COMPILE) ess-inf.el

ess-trns.elc: ess-trns.el $(CORE)
	$(COMPILE) ess-trns.el

ess-help.elc: ess-help.el $(CORE)

## S languages

ess-s-lang.elc : ess-s-lang.el ess.elc

ess-sas-d.elc : ess-sas-d.el ess.elc

ess-s3-d.elc : ess-s3-d.el ess-s-lang.elc ess.elc

ess-sp3-d.elc : ess-sp3-d.el ess-s-lang.elc ess.elc

ess-s4-d.elc : ess-s4-d.el ess-s-lang.elc ess.elc

ess-r-mode.elc : ess-r-mode.el ess-s-lang.elc ess.elc

ess-swv.elc: ess-swv.el ess-noweb.elc

## Lisp Languages

ess-lsp-l.elc : ess-lsp-l.el ess.elc

ess-xls-d.elc : ess-xls-d.el ess-lsp-l.elc ess.elc

ess-vst-d.elc : ess-vst-d.el ess-lsp-l.elc ess.elc

## SAS

ess-sas-l.elc : ess-sas-l.el ess-sas-a.el ess.elc

ess-sas-d.elc : ess-sas-d.el ess-sas-l.elc ess.elc

## Stata

ess-stata-lang.elc : ess-stata-lang.el ess.elc

ess-stata-mode.elc : ess-stata-mode.el ess.elc

## BUGS

ess-bugs-l.elc : ess-bugs-l.el

ess-bugs-d.elc : ess-bugs-d.el ess-bugs-l.elc

## JAGS

ess-jags-d.elc : ess-jags-d.el ess-bugs-l.elc ess-utils.elc ess-inf.elc

## GRETL

ess-gretl.elc : ess-gretl.el

ess-r-gui.elc : ess-r-gui.el ess-dde.elc


# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(COMPILE) %s.el" file file file file)))

# Use this to print an envvar for debugging purposes.
# Example: make print-EMACS
print-%: ; @echo $* = $($*)
