# Makefile - for ESS distribution.
#
# Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
# For ESS Version: 5.0
#
# Edit the makefile, type `make', and follow the instructions.
# Based/Stolen from VM 5.70, AUC-TeX 9.7p distributions.  Thanks Kyle and Per!

PREFIX=/usr/local

# what emacs is called on your system
EMACS = emacs
## XEmacs
#EMACS = xemacs

# where the Info file should go -- THIS MIGHT BE WRONG!
INFODIR = $(PREFIX)/info
## XEmacs
#INFODIR = $(PREFIX)/lib/$(EMACS)/info

## For strange setups...
INSTALL = cp

# program to convert .texi{nfo} to .info
MAKEINFO = makeinfo

# program to convert .texi{nfo} to .html
MAKEHTML = texi2html -verbose -iso -split_chapter -menu -glossary

# where the .elc, files should go
LISPDIR = .

# What GZCAT is called on your system (GNU gzip "cat")
#GZCAT = gzcat
GZCAT = zcat

#### no user servicable parts beyond this point ####

# no csh please
SHELL = /bin/sh

VERSION = 5.0-pre4
VERSIONDIR = ESS-$(VERSION)

LASTVERSION = 4.8.MM6.XE2

MYWWWDIR = /stat2/faculty/rossini/public_html/projects
MYFTPDIR = /m2/ftp/rossini

# have to preload the files that contain macro definitions or the
# byte compiler will compile everything that references them
# incorrectly.  also preload a file that sets byte compiler options.
PRELOADS = -l ./ess-comp.el

## compile with noninteractive and relatively clean environment
## XEMACS 19.xx and before.
#BATCHFLAGS = -batch -no-site-file -no-init-file
## EMACS and XEMACS 20.x.
BATCHFLAGS = -batch --no-site-file --no-init-file

## files that contain key macro definitions.  almost everything
## depends on them because the byte-compiler inlines macro expansions.
## everything also depends on the byte compiler options file since
## this might do odd things like turn off certain compiler
## optimizations.  When these change, RECOMPILE.
CORE = ess.elc ess-site.elc

SOURCES = ess-vars.el ess.el ess-site.el \
	ess-inf.el ess-mode.el ess-trns.el ess-help.el \
	ess-comp.el \
	essl-s.el   essd-s3.el  essd-s4.el  essd-s+3.el essd-r.el \
	essl-lsp.el essd-xls.el essd-vst.el \
	essl-sas.el essd-sas.el \
	essl-sta.el \
	essddr.el

DOCS = Doc/TODO Doc/Why_S-mode_Rocks.DMS \
	Doc/README-19.28 Doc/README.additions Doc/NEWS \
	Doc/ajr-talk.tex Doc/rmh-talk.tex Doc/slverb.sty \
	README.S README.XLispStat README.SAS 

SCRIPTS = ess-sas-sh-command install-sh mkinstalldirs

AUXFILES = COPYING Makefile README ess.texi ChangeLog \
	$(SCRIPTS) $(DOCS)

# The following works mainly with GNU Make.
OBJECTS = $(SOURCES:.el=.elc)

DISTFILES = $(addprefix $(VERSIONDIR)/,$(SOURCES)) \
	    $(addprefix $(VERSIONDIR)/,$(AUXFILES)) 

DISTSCRIPTS = $(addprefix $(VERSIONDIR)/,$(SCRIPTS))

### Commands

ESS: $(OBJECTS)

all:	ESS info html

info:	ess.info

html:	html/ess_toc.html

dvi:	ess.dvi

install: all
	mkdir -p $(INFODIR)
	$(INSTALL) ess.info* $(INFODIR)
	mkdir -p $(LISPDIR)
	if [ x$(LISPDIR) != x. ]; then $(INSTALL) *.elc $(LISPDIR) ; fi

clean:
	rm -f $(OBJECTS) ess.info* ess.aux ess.cp ess.dvi \
	  ess.ky ess.log ess.pg ess.toc ess.tp ess.vr ess.cps ess.fns ess.vrs \
	  ess.fn texput.log

dist:
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(VERSION)"
	@echo "**********************************************************"
	chmod a-w $(VERSIONDIR)/*.el $(VERSIONDIR)/ChangeLog $(VERSIONDIR)/Doc 
	chmod u+w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
	chmod ugo+x $(DISTSCRIPTS)
	tar hcovf $(VERSIONDIR).tar $(DISTFILES)
	gzip $(VERSIONDIR).tar
	cp $(VERSIONDIR).tar.gz $(MYWWWDIR)
	cp $(VERSIONDIR).tar.gz $(MYFTPDIR)
	cp $(VERSIONDIR)/README $(MYWWWDIR)/$(VERSIONDIR)-README
	cp $(VERSIONDIR)/README $(MYFTPDIR)/$(VERSIONDIR)-README
	chmod u-w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile

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

### TARGETS

.SUFFIXES: .el .elc

ess.info: ess.texi
	@echo "making Info documentation..."
	$(MAKEINFO) ess.texi

html/ess_toc.html: ess.texi
	@echo "making HTML documentation..."
	-(mkdir -p html; cd html; $(MAKEHTML) ../ess.texi)
	-@(cd html; ln -s ess_toc.html index.html)
	@echo "Test by pointing your browser to 'file://localhost/$$PWD/html/index.html'"

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<


ess-vars.elc:	ess-vars.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile ess-vars.el

ess.elc: ess.el ess-vars.elc
	$(EMACS) $(BATCHFLAGS) -l ess-vars.elc -f batch-byte-compile ess.el

ess-site.elc: ess-site.el ess.elc

ess-inf.elc: ess-inf.el $(CORE)

ess-mode.elc: ess-mode.el $(CORE)

ess-trns.elc: ess-trns.el $(CORE)

ess-help.elc: ess-help.el $(CORE)

## S languages

essl-s.elc : essl-s.el ess.elc

essd-sas.elc : essd-sas.el ess.elc

essd-s3.elc : essd-s3.el essl-s.elc ess.elc

essd-s+3.elc : essd-s+3.el essl-s.elc ess.elc

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

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
