### Makefile - for ESS distribution.
###
### Maintainer: A.J. Rossini <rossini@stat.sc.edu>
### For ESS Version: 5.0
###
### File: $Revision: 5.16 $.
###
### Edit the makefile, type `make', and follow the instructions.  Based
### on (i.e. Borrowed from) VM 5.70, AUC-TeX 9.7p distributions.  Thanks
### Kyle and Per!

PREFIX=/usr/local

## Emacs version that you are installing ESS for.  It might be simpler to do:
##     make EMACS=xemacs
## for example, than to edit this file.
##
EMACS = emacs-19.34
#EMACS = xemacs

## For strange setups...
INSTALL = cp

# where the .elc, files should go
LISPDIR = .

# What GZCAT is called on your system (GNU gzip "cat")
#GZCAT = gzcat
GZCAT = zcat

## compile with noninteractive and relatively clean environment:
## XEMACS 19.xx and before.
#BATCHFLAGS = -batch -no-site-file -no-init-file
## EMACS and XEMACS 20.x.
BATCHFLAGS = -batch --no-site-file --no-init-file


#### no user servicable parts beyond this point ####

# no csh please
SHELL = /bin/sh

VERSION = 5.1.8

#--- Dont forget to set  ess-version  in  ess-vars.el !!!!
#			 ~~~~~~~~~~~      -----------
VERSIONDIR = ESS-$(VERSION)

LASTVERSION = 5.1.7

## For noweb extraction of code and documentation.

NOTANGLE=notangle -L
#NOTANGLE=notangle for no indexing.
NOWEAVE=noweave

.SUFFIXES: .i3 .m3 .nw .tex .dvi .html .c .h .el
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

SOURCES = ess-vars.el ess.el ess-site.el ess-utils.el \
	ess-inf.el ess-iw32.el \
	ess-mode.el ess-trns.el ess-help.el \
	ess-comp.el ess-debug.el \
	essl-s.el essd-s3.el essd-s4.el essd-sp3.el essd-sp4.el essd-sp5.el \
	essd-r.el \
	essd-els.el \
	essl-lsp.el essd-xls.el essd-vst.el \
	essl-sas.el essd-sas.el \
	essl-sta.el essd-sta.el \
	essddr.el

SCRIPTS = ess-sas-sh-command install-sh mkinstalldirs

AUXFILES = COPYING Makefile ChangeLog $(SCRIPTS)

# The following works mainly with GNU Make.

OBJECTS = $(SOURCES:.el=.elc)

DISTFILES = $(addprefix $(VERSIONDIR)/,$(SOURCES)) \
	    $(addprefix $(VERSIONDIR)/,$(AUXFILES))

DISTSCRIPTS = $(addprefix $(VERSIONDIR)/,$(SCRIPTS))


### Targets

all: ESS

ESS: $(OBJECTS)

install: all
	mkdir -p $(LISPDIR)
	if [ x$(LISPDIR) != x. ]; then $(INSTALL) *.elc $(LISPDIR) ; fi

clean:
	rm -f $(OBJECTS)

distclean: clean
# and potentially more


### Targets below here are only for developers.

## This should checkout source into a subdirectory and then package it 
## back up.  (now that we are using CVS).

dist:
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(VERSION)"
	@echo "**********************************************************"
	chmod a-w $(VERSIONDIR)/*.el
	chmod a-w $(VERSIONDIR)/ChangeLog $(VERSIONDIR)/Doc/*
	chmod a-w $(VERSIONDIR)/ess.info* $(VERSIONDIR)/ess.dvi
	chmod u+w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
	chmod ugo+rx $(VERSIONDIR)/ess-sas-sh-command
	chmod ugo+x $(DISTSCRIPTS)
	tar hcovf $(VERSIONDIR).tar $(DISTFILES)
	gzip $(VERSIONDIR).tar
	cp $(VERSIONDIR).tar.gz $(MYWWWDIR)
	cp $(VERSIONDIR).tar.gz $(MYFTPDIR)
	chmod u+w $(VERSIONDIR)/README
	cp $(VERSIONDIR)/README $(MYWWWDIR)/$(VERSIONDIR)-README
	cp $(VERSIONDIR)/README $(MYFTPDIR)/$(VERSIONDIR)-README
	chmod u-w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
	chmod u-w $(VERSIONDIR)/README
	chmod u+w $(VERSIONDIR)/Doc


patch: ESS-$(VERSION).tar.gz ESS-$(LASTVERSION).tar.gz
	-rm -rf ESS-$(VERSION)
	mkdir ESS-$(VERSION)
	(cd ESS-$(VERSION); $(GZCAT) ../ESS-$(VERSION).tar.gz | tar xvf -)
	-rm -rf ESS-$(LASTVERSION)
	mkdir ESS-$(LASTVERSION)
	(cd ESS-$(LASTVERSION); tar zxvf ../ESS-$(LASTVERSION).tar.gz -)
	-diff -bBw -C 2 ESS-$(LASTVERSION) ESS-$(VERSION) \
		 > diff-$(LASTVERSION)-$(VERSION)
	-rm -rf ESS-$(VERSION)
	-rm -rf ESS-$(LASTVERSION)

### File Dependencies

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<


ess-vars.elc:	ess-vars.el
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-vars.el

ess.elc: ess.el ess-vars.elc
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -l ess-vars.elc -f batch-byte-compile ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the messages"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo " ** assignment to free variable comint-last-input-end"
	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-inf.el


ess-mode.elc: ess-mode.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the message"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo " ** variable last-sexp bound but not referenced"
	@echo "from the byte compiler if they occur. It is completely normal."
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-mode.el

ess-trns.elc: ess-trns.el $(CORE)
	@echo " "
	@echo "For the next compile, please disregard the message"
	@echo " ** the function set-keymap-parent is not known to be defined."
	@echo "from the byte compiler if it occurs. It is completely normal."
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-trns.el

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
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
