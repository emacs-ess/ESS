### Makefile - for DOCUMENTATION (./doc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

# program to convert .texi{nfo} to .html
TEXI2HTML = LANG=C texi2html -verbose -iso
SPLITCHAP = -split_chapter -menu -glossary

TEXI2DVI = LANG=C texi2dvi
TEXI2PDF = LANG=C texi2pdf

# To obtain sorted indices run:	"texindex ess.??".

#### no user servicable parts beyond this point ####

TEXISRC = announc.texi authors.texi bugrept.texi bugs.texi bugs-ms.texi     \
	credits.texi currfeat.texi ess.texi getting.texi help-s.texi        \
	help-sas.texi inst_cvs.texi inst_tar.texi license.texi mailing.texi \
	newfeat.texi readme.texi requires.texi stabilty.texi help-bugs.texi \
        ../VERSION

### Targets --
PDFs = ess.pdf readme.pdf
TXTs = ../README ../ANNOUNCE

all  : info text html dvi pdf
docs : info text html
info : info/ess.info
text : $(TXTs)
html : html/ess_toc.html html/readme_toc.html
dvi  : ess.dvi readme.dvi
pdf  : $(PDFs)
ps   : ess.ps  readme.ps

ess.dvi : $(TEXISRC); $(TEXI2DVI) ess.texi
ess.pdf : $(TEXISRC); $(TEXI2PDF) ess.texi
ess.ps  : ess.dvi; LANG=C dvips $^ -o $@

readme.dvi : $(TEXISRC); $(TEXI2DVI) readme.texi
readme.pdf : $(TEXISRC); $(TEXI2PDF) readme.texi
readme.ps  : readme.dvi; dvips $^ -o $@

## this shall remove *exactly* those things that are *not* in version control:
clean :
	-@rm -f $(PDFs) *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys \
	  *.log *.out *.pg *.pgs *.ps *.ps-2 *.tmp *.toc *.tp *.vr *.vrs

## this removes also things in VC (when they are remade by "make"):
distclean : clean
	-@rm -f $(TXTs) html/* info/*.info*

../README: $(TEXISRC)
	$(MAKETXT) readme.texi \
	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' \
	> ../README

../ANNOUNCE: $(TEXISRC)
	$(MAKETXT) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

README.Microsoft : README.Microsoft.texi
	$(MAKETXT) README.Microsoft.texi \
	| perl -pe 'last if /^Concept Index/;' > README.Microsoft

### File Dependencies

info/ess.info: $(TEXISRC)
	@echo "making Info documentation..."
	$(MAKEINFO) ess.texi
	mv -f ess.info* info

install : info/ess.info
	-$(INSTALLDIR) $(INFODIR)
	$(INSTALL) info/ess.info* $(INFODIR)
	test -f $(INFODIR)/dir || $(INSTALL) info/dir $(INFODIR)

##  TO DO:  ess_toc.html & readme_toc.html should be merged into index.html
html/ess_toc.html: $(TEXISRC)
	@echo "making HTML documentation..."
	$(TEXI2HTML) $(SPLITCHAP) ess.texi
	test -d ess && cp -p ess/ess_toc.html html/index.html || cp -p ess_toc.html html/index.html
	test -d ess && mv -f ess/*.html html || mv -f *.html html

html/readme_toc.html: $(TEXISRC)
	$(TEXI2HTML) readme.texi
	mv -f readme*.html html
