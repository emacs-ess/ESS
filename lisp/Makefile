### Makefile - for ESS distribution.
###
### Maintainer: A.J. Rossini <rossini@stat.sc.edu>
### For ESS Version: 5.x.y
###
### File: $Revision: 5.47 $.
###
### Edit the makefile, type `make', and follow the instructions.  Based
### on (i.e. Borrowed from) VM 5.70, AUC-TeX 9.7p distributions.  Thanks
### Kyle and Per!

include ../Makeconf

#### no user servicable parts beyond this point ####

## For noweb extraction of code and documentation.

NOTANGLE=notangle -L
#NOTANGLE=notangle for no indexing.
NOWEAVE=noweave

## GNU make only (?) (Sun make can't make readme.dvi) .. :
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

TOCOMPILE = ess-cust.el $(CORE) \
	ess-emcs.el ess-sysdp.el ess-utils.el \
	ess-inf.el ess-iw32.el essnt204.el \
	ess-mode.el ess-trns.el ess-help.el \
	ess-comp.el \
	ess-menu.el ess-mous.el mouseme.el \
	essl-s.el essd-s3.el essd-s4.el \
	essd-sp3.el essd-sp4.el essd-sp4com.el essd-sp5.el essd-sp6.el \
	essd-r.el \
	essd-els.el \
	essl-lsp.el essd-xls.el essd-vst.el essd-arc.el \
	essl-sas.el essd-sas.el \
	essl-sta.el essd-sta.el make-regexp.el \
	essl-omg.el essd-omg.el \
	essl-bug.el \
	ess-noweb.el noweb-mode.el noweb-font-lock-mode.el \
	ess-font-lock.el \
	essa-r.el \
	essddr.el
##?	ess-latex.el \

SOURCES = $(TOCOMPILE) ess-debug.el


SCRIPTS = ess-sas-sh-command install-sh mkinstalldirs

AUXFILES = COPYING Makefile ChangeLog $(SCRIPTS)

# The following works mainly with GNU Make.

OBJECTS = $(TOCOMPILE:.el=.elc)


### Targets

all: ESS

ESS: $(OBJECTS)

install: all
	if [ $(LISPDIR) != . ]; then $(INSTALL) *.elc $(LISPDIR); fi

dist: ess-cust.el
	cvs commit -m "'make dist' for new version" ess-cust.el
	@touch dist
clean:
	rm -f $(OBJECTS)

distclean: clean
# and potentially more


### Targets below here are only for developers.

### Replace Version numbers (as in ../doc/Makefile !):
## Use $(shell ..) only here, so only these are GNU make dependent :
ess-cust.el: ../VERSION
	perl -pi -e 's/".*"/"$(shell cat $^)"/ if /ess-version/' $@

### File Dependencies

.el.elc:
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile $<


ess-cust.elc:	ess-cust.el
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-cust.el

ess.elc: ess.el ess-cust.elc
	$(EMACSBATCH) $(PRELOADS) -l ess-cust.elc -f batch-byte-compile ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the messages"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo " ** assignment to free variable comint-last-input-end"
	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-inf.el


ess-mode.elc: ess-mode.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the message"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo " ** variable last-sexp bound but not referenced"
	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-mode.el

ess-trns.elc: ess-trns.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the message"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo "from the byte compiler if it occurs. It is completely normal."
	$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile ess-trns.el

ess-help.elc: ess-help.el $(CORE)

## S languages

essl-s.elc : essl-s.el ess.elc

essd-sas.elc : essd-sas.el ess.elc

essd-s3.elc : essd-s3.el essl-s.elc ess.elc

essd-sp3.elc : essd-sp3.el essl-s.elc ess.elc

essd-s4.elc : essd-s4.el essl-s.elc ess.elc

essd-r.elc : essd-r.el essl-s.elc ess.elc

## Lisp Languages

essl-lsp.elc : essl-lsp.el ess.elc

essd-xls.elc : essd-xls.el essl-lsp.elc ess.elc

essd-vst.elc : essd-vst.el essl-lsp.elc ess.elc

## SAS

essl-sas.elc : essl-sas.el ess.elc

essd-sas.elc : essd-sas.el essl-sas.elc ess.elc

## Stata

essl-sta.elc : essl-sta.el ess.elc

essd-sta.elc : essd-sta.el ess.elc

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACSBATCH) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
