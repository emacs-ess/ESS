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
ELC = $(CORE) ess-comp.elc ess-cust.elc \
	ess-dde.elc ess-emcs.elc ess-font-lock.elc \
	ess-help.elc ess-inf.elc ess-mode.elc \
	ess-menu.elc ess-mous.elc mouseme.elc \
	ess-swv.elc ess-toolbar.elc \
	ess-trns.elc ess-utils.elc \
	essl-s.elc essd-s3.elc essd-s4.elc \
	essd-sp3.elc essd-sp4.elc essd-sp5.elc essd-sp6.elc \
	essd-r.elc ess-rdired.elc essd-r-args.elc \
	essd-els.elc \
	essl-lsp.elc essd-xls.elc essd-vst.elc essd-arc.elc \
	essl-sas.elc essd-sas.elc essa-sas.elc \
	essl-sta.elc essd-sta.elc make-regexp.elc \
	essl-omg.elc essd-omg.elc \
	essl-bugs.elc essd-bugs.elc essd-jags.elc \
	ess-noweb.elc noweb-mode.elc noweb-font-lock-mode.elc \
	essa-r.elc essddr.elc ess-eldoc.elc ess-roxygen.elc \
	essd-sp6w.elc msdos.elc
##      ^^^^^^^^^^^^^^^^^^^^^^^ Windows only (but be platform-oblivious)

### Targets

all: $(ELC)

install: $(ELC)
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) ChangeLog *.elc $(LISPDIR)
	export J; for I in *.elc; do J=$$I; $(INSTALL) `basename $$J .elc`.el $(LISPDIR); done
#	test "$(LISPDIR)" = "." || $(INSTALL) *.elc $(LISPDIR)

dist: ess-cust.el
	@touch dist
## svn committing happens in ../Makefile

clean:
	rm -f $(ELC)

distclean: clean
# and potentially more


### Targets below here are only for developers - and these must have perl

### Replace Version numbers (as in ../doc/Makefile !):
ess-cust.el: ../VERSION
	perl -pi -e 's/".*"/"$(ESSVERSION)"/ if /ess-version/' $@


### File Dependencies

.el.elc:
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile $<


ess-cust.elc:	ess-cust.el
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-cust.el

ess.elc: ess.el ess-cust.elc
	$(EMACSBATCH) $(PRELOADS) -l ess-cust.elc -f batch-byte-compile ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el $(CORE)
# 	@echo " "
# 	@echo "For the next compile, please disregard the messages"
# 	@echo " ** the function set-keymap-parent is not known to be defined."
# 	@echo " ** assignment to free variable comint-last-input-end"
# 	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-inf.el


ess-mode.elc: ess-mode.el $(CORE)
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-mode.el

ess-trns.elc: ess-trns.el $(CORE)
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-trns.el

ess-help.elc: ess-help.el $(CORE)

## S languages

essl-s.elc : essl-s.el ess.elc

essd-sas.elc : essd-sas.el ess.elc

essd-s3.elc : essd-s3.el essl-s.elc ess.elc

essd-sp3.elc : essd-sp3.el essl-s.elc ess.elc

essd-s4.elc : essd-s4.el essl-s.elc ess.elc

essd-r.elc : essd-r.el essl-s.elc ess.elc

ess-swv.elc: ess-swv.el ess-noweb.elc

## Lisp Languages

essl-lsp.elc : essl-lsp.el ess.elc

essd-xls.elc : essd-xls.el essl-lsp.elc ess.elc

essd-vst.elc : essd-vst.el essl-lsp.elc ess.elc

## SAS

essl-sas.elc : essl-sas.el essa-sas.el ess.elc

essd-sas.elc : essd-sas.el essl-sas.elc ess.elc

## Stata

essl-sta.elc : essl-sta.el ess.elc

essd-sta.elc : essd-sta.el ess.elc

## BUGS

essl-bugs.elc : essl-bugs.el ess-emcs.elc

essd-bugs.elc : essd-bugs.el essl-bugs.elc

## JAGS 

essd-jags.elc : essd-jags.el essl-bugs.elc ess-utils.elc ess-inf.elc

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
