### Makefile - for compiled e-lisp of ESS distribution.
###

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
COMPILE = $(EMACSBATCH) --directory . --directory obsolete -f batch-byte-compile

CORE = ess.elc ess-site.elc

ELC = $(CORE) ess-custom.elc \
	ess-help.elc ess-inf.elc ess-mode.elc \
	ess-swv.elc ess-toolbar.elc \
	ess-trns.elc ess-utils.elc \
	ess-sas-l.elc ess-sas-d.elc ess-sas-a.elc \
	ess-stata-lang.elc ess-stata-mode.elc make-regexp.elc \
	ess-bugs-l.elc ess-bugs-d.elc ess-jags-d.elc \
	ess-noweb.elc ess-noweb-mode.elc ess-noweb-font-lock-mode.elc \
	ess-roxy.elc ess-rutils.elc ess-r-completion.elc \
	ess-s-lang.elc \
	ess-rdired.elc ess-r-syntax.elc ess-r-mode.elc ess-rd.elc \
	ess-r-flymake.elc ess-r-package.elc ess-r-xref.elc ess-tracebug.elc ess-julia.elc\
	julia-mode.elc\
	ess-generics.elc \
	obsolete/ess-omg-l.elc obsolete/ess-omg-d.elc \
	obsolete/ess-dde.elc obsolete/ess-font-lock.elc \
	obsolete/ess-r-gui.elc\
	obsolete/ess-sp3-d.elc obsolete/ess-sp4-d.elc obsolete/ess-sp5-d.elc obsolete/ess-sp6-d.elc \
	obsolete/ess-s3-d.elc obsolete/ess-s4-d.elc \
	obsolete/msdos.elc obsolete/ess-eldoc.elc obsolete/ess-r-args.elc\
	obsolete/essd-els.elc obsolete/ess-gretl.elc \
	obsolete/ess-lsp-l.elc obsolete/ess-xls-d.elc obsolete/ess-vst-d.elc obsolete/ess-arc-d.elc \
	obsolete/ess-sp6w-d.elc
##  ^^^^^^^^^^^^^^ Windows only (but be platform-oblivious)
## julia-mode.el : really from the julia sources -- but want in tarball!


# ESSR_VER =`cat ../etc/ESSR-VERSION`
##ESSR_VER = $(shell cat ../etc/ESSR-VERSION)

### Targets

all: $(ELC) ess-custom.el

.PHONY: dist
dist: all julia-mode.el

install: dist
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) *.elc $(LISPDIR)
	$(INSTALL) obsolete/*.elc $(LISPDIR)
	-export J; for I in *.elc; do J=$$I; $(INSTALL) `basename $$J .elc`.el $(LISPDIR); done
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


ess-mode.elc: ess-mode.el $(CORE)
	$(COMPILE) ess-mode.el

ess-trns.elc: ess-trns.el $(CORE)
	$(COMPILE) ess-trns.el

ess-help.elc: ess-help.el $(CORE)

## S languages

ess-s-lang.elc : ess-s-lang.el ess.elc

ess-sas-d.elc : ess-sas-d.el ess.elc

ess-r-mode.elc : ess-r-mode.el ess-s-lang.elc ess.elc

ess-swv.elc: ess-swv.el ess-noweb.elc

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

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(COMPILE) %s.el" file file file file)))

# Use this to print an envvar for debugging purposes.
# Example: make print-EMACS
print-%: ; @echo $* = $($*)
