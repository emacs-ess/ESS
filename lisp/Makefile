### Makefile - for ESS distribution.
###
### Maintainer: A.J. Rossini <rossini@stat.sc.edu>
### For ESS Version: 5.0
###
### Revision: $Revision: 4.7 $.
###
### Edit the makefile, type `make', and follow the instructions.  Based
### on (i.e. Borrowed from) VM 5.70, AUC-TeX 9.7p distributions.  Thanks
### Kyle and Per!

PREFIX=/usr/local

# Emacs version that you are installing ESS for.
EMACS = emacs
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

VERSION = 5.0
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

DOCS = 	README README.S README.XLispStat README.SAS \
	ess.texi dir ess.info ess.info-1 ess.info-2 ess.info-3 ess.dvi \
	Doc/TODO Doc/Why_S-mode_Rocks.DMS \
	Doc/README-19.28 Doc/README.additions Doc/NEWS \
	Doc/ajr-talk.tex Doc/rmh-talk.tex Doc/slverb.sty 

SCRIPTS = ess-sas-sh-command install-sh mkinstalldirs

AUXFILES = COPYING Makefile ChangeLog $(SCRIPTS) $(DOCS) 

DOCDIST = ess.ps ess.pdf \
	Doc/ajr-talk.ps Doc/ajr-talk.pdf \
	Doc/rmh-talk.ps Doc/rmh-talk.pdf

# The following works mainly with GNU Make.

OBJECTS = $(SOURCES:.el=.elc)

DISTFILES = $(addprefix $(VERSIONDIR)/,$(SOURCES)) \
	    $(addprefix $(VERSIONDIR)/,$(AUXFILES)) 

DISTSCRIPTS = $(addprefix $(VERSIONDIR)/,$(SCRIPTS))

# DOCDISTFILES = $(addprefix $(VERSIONDIR)/,$(DOCDIST)) 

### Targets

ESS: $(OBJECTS)

ESS-doc: info html dvi 

all:	ESS info html

ess.info: ess.texi

html:	html/ess_toc.html

ess.dvi: ess.texi

ps:	ess.dvi
	dvips ess.dvi -o ess.ps

pdf: 	ess.ps
	rm -f ess.pdf
	ps2pdf ess.ps > ess.pdf

install: all
	mkdir -p $(INFODIR)
	$(INSTALL) ess.info* $(INFODIR)
	mkdir -p $(LISPDIR)
	if [ x$(LISPDIR) != x. ]; then $(INSTALL) *.elc $(LISPDIR) ; fi

clean:
	rm -f $(OBJECTS) ess.aux ess.cp ess.ky ess.log ess.pg ess.toc \
	  ess.tp ess.vr ess.cps ess.fns ess.vrs ess.fn texput.log


### Targets below here are only for developers.

# ess.dvi ess.info
dist:  
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(VERSION)"
	@echo "**********************************************************"
	chmod a-w $(VERSIONDIR)/*.el
	chmod a-w $(VERSIONDIR)/ChangeLog $(VERSIONDIR)/Doc/* 
	chmod a-w $(VERSIONDIR)/ess.info* $(VERSIONDIR)/ess.dvi
	chmod u+w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
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


##dist-doc: $(DOCDISTFILES)
##	@echo "**********************************************************"
##	@echo "** Making distribution of ESS-DOC for release $(VERSION)"
##	@echo "**********************************************************"
##	tar hcovf $(VERSIONDIR)-doc.tar $(DOCDISTFILES)
##	gzip $(VERSIONDIR)-doc.tar
##	cp $(VERSIONDIR)-doc.tar.gz $(MYWWWDIR)
##	cp $(VERSIONDIR)-doc.tar.gz $(MYFTPDIR)


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
