# Stolen from the VM 5.70 distribution.  Thanks Kyle!

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
MAKEHTML = texi2html -split_chapter -menu -glossary

# where the .elc, files should go
LISPDIR = .

# What GZCAT is called on your system (GNU gzip "cat")
#GZCAT = gzcat
GZCAT = zcat

############## no user servicable parts beyond this point ###################

# no csh please
SHELL = /bin/sh

VERSION = 4.9-b14

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
	essl-s.el essl-lsp.el essl-sas.el \
	essd-s3.el essd-s4.el essd-s+3.el essd-r.el essd-xls.el essd-sas.el \
	ess-inf.el ess-mode.el ess-trns.el ess-help.el \
	ess-doc.el

OBJECTS = $(SOURCES:.el=.elc)

AUXFILES = COPYING Makefile NEWS README TODO \
	ess.texi ChangeLog

### Commands

ESS: $(OBJECTS)

all:	ESS info html

info:	ess.info

html:	html/ess_toc.html

install: all
	mkdir -p $(INFODIR)
	$(INSTALL) ess.info* $(INFODIR)
	mkdir -p $(LISPDIR)
	$(INSTALL) *.elc $(LISPDIR)

clean:
	rm -f $(OBJECTS) ess.info* ess.aux ess.cp ess.dvi \
	  ess.ky ess.log ess.pg ess.toc ess.tp ess.vr \
	  ess.fn texput.log

distrib: $(SOURCES) $(AUXFILES)
	chmod a-w *.el
	chmod u+w ess-site.el Makefile
	tar cvf ESS-$(VERSION).tar $(SOURCES) $(AUXFILES)
	gzip ESS-$(VERSION).tar

patch: ESS-$(VERSION).tar.gz ESS-$(LASTVERSION).tar.gz
	-rm -rf ESS-$(VERSION)
	mkdir ESS-$(VERSION)
	(cd ESS-$(VERSION); $(GZCAT) ../ESS-$(VERSION).tar.gz | tar xvf -)
	-rm -rf ESS-$(LASTVERSION)
	mkdir ESS-$(LASTVERSION)
	(cd ESS-$(LASTVERSION); $(GZCAT) ../ESS-$(LASTVERSION).tar.gz|tar xvf -)
	-diff -bBw -C 2 ESS-$(LASTVERSION) ESS-$(VERSION) > diff-$(LASTVERSION)-$(VERSION)
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

ess-site.elc:  ess-site.el ess.elc
#	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile ess-site.el

ess-vars.elc:	ess-vars.el
#	$(EMACS) $(BATCHFLAGS)  -f batch-byte-compile ess-vars.el

ess.elc:	ess.el
#	$(EMACS) $(BATCHFLAGS)  -f batch-byte-compile ess.el


ess-inf.elc:  ess-inf.el $(CORE)

ess-mode.elc:  ess-mode.el $(CORE)

ess-trns.elc:  ess-trns.el $(CORE)

ess-help.elc:  ess-help.el $(CORE)

# For Dialect Files:
#	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-r.el

essd-sas.elc : essd-sas.el ess.el

essd-s3.elc : essd-s+3.el ess.el

essd-s+3.elc : essd-s+3.el ess.el

essd-s4.elc : essd-s4.el ess.el

essd-r.elc : essd-r.el ess.el

essd-xls.elc : essd-xls.el ess.el

#essd-vst.elc : essd-vst.el ess.el
#	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-r.el

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
