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

# have to preload the files that contain macro definitions or the
# byte compiler will compile everything that references them
# incorrectly.  also preload a file that sets byte compiler options.
PRELOADS = -l ./ess-comp.el


## files that contain key macro definitions.  almost everything
## depends on them because the byte-compiler inlines macro expansions.
## everything also depends on the byte compiler options file since
## this might do odd things like turn off certain compiler
## optimizations.  When these change, RECOMPILE.
CORE = ess.elc ess-site.elc

### Everything but ess-debug.el, ess-install.el
###		  ess-send.el , ess-send2.el
### DEPRECATED:  essl-bug.el
##
ELC = $(CORE) ess-comp.elc ess-custom.elc \
	ess-dde.elc ess-compat.elc ess-font-lock.elc \
	ess-help.elc ess-inf.elc ess-mode.elc \
	ess-menu.elc ess-mouse.elc mouseme.elc \
	ess-swv.elc ess-toolbar.elc \
	ess-trns.elc ess-utils.elc \
	essd-els.elc ess-gretl.elc \
	ess-lsp-l.elc ess-xls-d.elc ess-vst-d.elc ess-arc-d.elc \
	ess-sas-l.elc ess-sas-d.elc ess-sas-a.elc \
	ess-sta-l.elc ess-sta-d.elc make-regexp.elc \
	ess-omg-l.elc ess-omg-d.elc \
	ess-bugs-l.elc ess-bugs-d.elc ess-jags-d.elc \
	ess-noweb.elc ess-noweb-mode.elc ess-noweb-font-lock-mode.elc \
	ess-eldoc.elc ess-roxy.elc ess-rutils.elc ess-r-completion.elc \
	ess-s-l.elc ess-s3-d.elc ess-s4-d.elc \
	ess-sp3-d.elc ess-sp4-d.elc ess-sp5-d.elc ess-sp6-d.elc \
	ess-rdired.elc ess-r-args.elc ess-r-syntax.elc ess-r-d.elc ess-rd.elc \
	ess-r-package.elc ess-tracebug.elc ess-julia.elc\
	julia-mode.elc\
	ess-generics.elc ess-r-gui.elc\
	ess-sp6w-d.elc msdos.elc
##      ^^^^^^^^^^^^^^^^^^^^^^^ Windows only (but be platform-oblivious)
## julia-mode.el : really from the julia sources -- but want in tarball!


# ESSR_VER =`cat ../etc/ESSR-VERSION`
##ESSR_VER = $(shell cat ../etc/ESSR-VERSION)

### Targets

all: $(ELC) ess-custom.el julia-mode.el

.PHONY: dist
dist: all
##	@touch dist
## svn committing happens in ../Makefile

install: dist
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) ChangeLog *.elc $(LISPDIR)
	-export J; for I in *.elc; do J=$$I; $(INSTALL) `basename $$J .elc`.el $(LISPDIR); done
	if [ -f /etc/debian_version -a -n "$(SITELISP)" -a ! -f "$(SITELISP)/ess-site.el" ] ; \
	then \
		ln -s $(LISPDIR)/ess-site.el $(SITELISP)/ess-site.el ; \
	fi;
#	test "$(LISPDIR)" = "." || $(INSTALL) *.elc $(LISPDIR)

uninstall:
	-cd $(LISPDIR) && $(UNINSTALL) ChangeLog *.elc *.el
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

# ess-r-d.el: ../etc/ESSR-VERSION
# 	sed -i -e '/let.*ESSR-version/s/"[-.0-9]*"/"$(ESSR_VER)"/' $@

## This should typically happen before building ESS
julia-mode.el:
	$(DOWNLOAD) https://raw.githubusercontent.com/JuliaLang/julia-emacs/master/julia-mode.el > julia-mode.el

### File Dependencies

.el.elc:
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile $<


ess-custom.elc: ess-custom.el ess-comp.el
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-custom.el

ess.elc: ess.el ess-comp.el ess-custom.elc
	$(EMACSBATCH) $(PRELOADS) -l ess-custom.elc -f batch-byte-compile ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el ess-comp.el $(CORE)
# 	@echo " "
# 	@echo "For the next compile, please disregard the messages"
# 	@echo " ** the function set-keymap-parent is not known to be defined."
# 	@echo " ** assignment to free variable comint-last-input-end"
# 	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-inf.el


ess-mode.elc: ess-mode.el ess-comp.el $(CORE)
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-mode.el

ess-trns.elc: ess-trns.el ess-comp.el $(CORE)
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-trns.el

ess-help.elc: ess-help.el $(CORE)

## S languages

ess-s-l.elc : ess-s-l.el ess.elc

ess-sas-d.elc : ess-sas-d.el ess.elc

ess-s3-d.elc : ess-s3-d.el ess-s-l.elc ess.elc

ess-sp3-d.elc : ess-sp3-d.el ess-s-l.elc ess.elc

ess-s4-d.elc : ess-s4-d.el ess-s-l.elc ess.elc

ess-r-d.elc : ess-r-d.el ess-s-l.elc ess.elc

ess-swv.elc: ess-swv.el ess-noweb.elc

## Lisp Languages

ess-lsp-l.elc : ess-lsp-l.el ess.elc

ess-xls-d.elc : ess-xls-d.el ess-lsp-l.elc ess.elc

ess-vst-d.elc : ess-vst-d.el ess-lsp-l.elc ess.elc

## SAS

ess-sas-l.elc : ess-sas-l.el ess-sas-a.el ess.elc

ess-sas-d.elc : ess-sas-d.el ess-sas-l.elc ess.elc

## Stata

ess-sta-l.elc : ess-sta-l.el ess.elc

ess-sta-d.elc : ess-sta-d.el ess.elc

## BUGS

ess-bugs-l.elc : ess-bugs-l.el ess-compat.elc

ess-bugs-d.elc : ess-bugs-d.el ess-bugs-l.elc

## JAGS

ess-jags-d.elc : ess-jags-d.el ess-bugs-l.elc ess-utils.elc ess-inf.elc

## GRETL

ess-gretl.elc : ess-gretl.el

ess-r-gui.elc : ess-r-gui.el ess-dde.elc

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
