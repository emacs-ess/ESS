
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

VERSION = 4.9-b12

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
	ess-inf.el ess-mode.el ess-trns.el ess-help.el # ess-xtra.el

OBJECTS = $(SOURCES:.el=.elc)

AUXFILES = COPYING Makefile NEWS README TODO \
	ESS.texi ChangeLog

### Commands

ESS: $(OBJECTS)

all:	ESS info html

info:	ESS.info

html:	html/ESS_toc.html

install: all
	$(INSTALL) ESS.info* $(INFODIR)
	$(INSTALL) *.elc $(LISPDIR)

clean:
	rm -f $(OBJECTS) ESS.info* ESS.aux ESS.cp ESS.dvi \
	  ESS.ky ESS.log ESS.pg ESS.toc ESS.tp ESS.vr \
	  ESS.fn texput.log

distrib: $(SOURCES) $(AUXFILES)
	chmod a-w *.el
	chmod u+w S-site.el Makefile
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

ESS.info: ESS.texi
	@echo "making Info documentation..."
	$(MAKEINFO) ESS.texi

html/ESS_toc.html: ESS.texi
	@echo "making HTML documentation..."
	-(mkdir -p html; cd html; $(MAKEHTML) ../ESS.texi)
	-@(cd html; ln -s ESS_toc.html index.html)
	@echo "Test by pointing your browser to 'file://localhost/$$PWD/html/'"


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


#### RMH suggests:


##
##* makefile
##ESS:
##	emacs -batch -q  -f batch-byte-compile  ess-vars.el
##	emacs -batch -q  -l ess-vars.elc -l cl -l cl-macs -f batch-byte-compile  ess.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -f batch-byte-compile  essd-s3.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -f batch-byte-compile  essd-s+3.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -f batch-byte-compile  essd-s4.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -f batch-byte-compile  essd-r.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -f batch-byte-compile  essd-xls.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s3.elc -l essd-s+3.elc  -l essd-s4.elc  -l essd-r.elc  -l essd-xls.elc -f batch-byte-compile  ess-site.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s+3.elc -l essd-r.elc -l essd-xls.elc -l ess-site.elc -f batch-byte-compile  ess-comp.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s+3.elc -l essd-r.elc -l essd-xls.elc -l ess-site.elc -f batch-byte-compile  ess-inf.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s+3.elc -l essd-r.elc -l essd-xls.elc -l ess-site.elc -f batch-byte-compile  ess-mode.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s+3.elc -l essd-r.elc -l essd-xls.elc -l ess-site.elc -f batch-byte-compile  ess-trns.el
##	emacs -batch -q  -l ess-vars.elc -l ess.elc -l essd-s+3.elc -l essd-r.elc -l essd-xls.elc -l ess-site.elc -f batch-byte-compile  ess-help.el
##
##
