### Makefile - for DOCUMENTATION (./doc) of ESS distribution.
###
### Maintainer: A.J. Rossini <rossini@stat.sc.edu>
### For ESS Version: 5.0
###
### File: $Revision: 1.9 $.
###
### Edit the makefile, type `make', and follow the instructions.  Based
### on (i.e. Borrowed from) VM 5.70, AUC-TeX 9.7p distributions.  Thanks
### Kyle and Per!

PREFIX=/usr/local

# where the Info file should go -- THIS MIGHT BE WRONG!
INFODIR = $(PREFIX)/info
## XEmacs
#INFODIR = $(PREFIX)/lib/$(EMACS)/info

## For strange setups...
INSTALL = cp

# program to convert .texi{nfo} to .info
MAKEINFO = makeinfo

# program to convert .texi{nfo} to .html
MAKEHTML = texi2html -verbose -iso
MAKEHTMLs = $(MAKEHTML) -split_chapter -menu -glossary

# What GZCAT is called on your system (GNU gzip "cat")
#GZCAT = gzcat
GZCAT = zcat

#### no user servicable parts beyond this point ####

# no csh please
SHELL = /bin/sh

# Rather use ESSVERSION from ../Makefile  (how)?
#unused VERSION = 5.1.7
#unused VERSIONDIR = ESS-$(VERSION)

MYWWWDIR = /none/at/this/time
MYFTPDIR = /none/at/this/time

DOCS =	README README.S README.XLispStat README.SAS \
	ess.texi dir ess.info ess.info-1 ess.info-2 ess.info-3 ess.dvi \
	TODO Why_S-mode_Rocks.DMS \
	README-19.28 README.additions NEWS \
	ajr-talk.tex rmh-talk.tex slverb.sty

DOCDIST = ess.ps ess.pdf \
	ajr-talk.ps ajr-talk.pdf \
	rmh-talk.ps rmh-talk.pdf

# The following works mainly with GNU Make.

# DOCDISTFILES = $(addprefix $(VERSIONDIR)/,$(DOCDIST))

### Targets -- new 'readme.texi' !?

all: ess.info html ess.dvi readme.dvi

# Documentation mentions info, not ess.info

info : ess.info

#
# If indexes don't index, run:	"texindex ess.??" to obtain the sorted indices.
#

html:	html/ess_toc.html html/readme_toc.html

ess.dvi: ess.texi

ess.ps: ess.dvi
	dvips ess.dvi -o ess.ps

## or use pdftex ?
ess.pdf: ess.ps
	rm -f ess.pdf
	ps2pdf ess.ps > ess.pdf

install: all
	mkdir -p $(INFODIR)
	$(INSTALL) ess.info* $(INFODIR)

clean:
	rm -f ess.aux ess.cp ess.ky ess.log ess.pg ess.toc \
	  ess.tp ess.vr ess.cps ess.fns ess.vrs ess.fn texput.log

distclean: clean
	rm -f ess.dvi ess.ps ess.pdf
	rm -fr html

##-- Be careful: to recreate the info files you NEED a newer makeinfo !
realclean: distclean
	rm -f ess.info*

### Targets below here are only for developers.

##dist-doc: $(DOCDISTFILES)
##	@echo "**********************************************************"
##	@echo "** Making distribution of ESS-DOC for release $(VERSION)"
##	@echo "**********************************************************"
##	tar hcovf $(VERSIONDIR)-doc.tar $(DOCDISTFILES)
##	gzip $(VERSIONDIR)-doc.tar
##	cp $(VERSIONDIR)-doc.tar.gz $(MYWWWDIR)
##	cp $(VERSIONDIR)-doc.tar.gz $(MYFTPDIR)

### File Dependencies

ess.info: *.texi
	@echo "making Info documentation..."
	$(MAKEINFO) ess.texi

##-- HTML:
##  TO DO:  ess_toc.html & readme_toc.html should be merged into index.html
##  =====
html/ess_toc.html: *.texi
	@echo "making HTML documentation..."
	-(mkdir -p html; cd html; $(MAKEHTMLs) ../ess.texi)
	-@(cd html; rm -f index.html; ln -s ess_toc.html index.html)
	@echo "Test by pointing your browser to 'file://localhost/$$PWD/html/index.html'"

html/readme_toc.html: *.texi
	-(mkdir -p html; cd html; $(MAKEHTML) ../readme.texi)
