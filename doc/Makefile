### Makefile - for DOCUMENTATION (./doc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

# program to convert .texi{nfo} to .html
#MM: use makeinfo (i.e. MAKEHTML from above) which is more
#--  standardized than texi2html which exists in
#MM  at least two widely differing versions (TeX vs GNU):
#MM TEXI2HTML = LANG=C texi2html -verbose -iso
#MM SPLITCHAP = -split_chapter -menu -glossary
##

TEXI2DVI = LANG=C texi2dvi
TEXI2PDF = LANG=C texi2dvi --pdf

# To obtain sorted indices run:	"texindex ess.??".

#### no user servicable parts beyond this point ####

TEXISRC = announc.texi authors.texi bugrept.texi bugs.texi bugs-ms.texi     \
	credits.texi currfeat.texi ess.texi ess-defs.texi getting.texi \
	help-s.texi help-sas.texi inst_svn.texi unixlike.texi windows.texi \
	license.texi mailing.texi onewfeat.texi \
	newfeat.texi readme.texi requires.texi stabilty.texi help-bugs.texi \
        help-jags.texi Makefile ../Makeconf

ESSINFONODE1='* ESS: (ess).'
ESSINFONODE2='	        Emacs Speaks Statistics'
ESSINFONODE3='                        (S/S+/R, SAS, BUGS, Stata, XLisp-Stat).'

### Targets --
PDFs = ess.pdf readme.pdf refcard/refcard.pdf
DVIs = ess.dvi readme.dvi
TXTs = ../README ../ANNOUNCE ../NEWS ../ONEWS

all  : info text html dvi pdf
docs : info text html
info : info/ess.info
text : $(TXTs)
html : html/ess.html html/readme.html
dvi  : $(DVIs)
pdf  : $(PDFs)
ps   : ess.ps  readme.ps

ess.dvi : $(TEXISRC); $(TEXI2DVI) ess.texi
ess.pdf : $(TEXISRC); $(TEXI2PDF) ess.texi
ess.ps  : ess.dvi; LANG=C dvips $^ -o $@

readme.dvi : $(TEXISRC); $(TEXI2DVI) readme.texi
readme.pdf : $(TEXISRC); $(TEXI2PDF) readme.texi
readme.ps  : readme.dvi; dvips $^ -o $@

cleanaux:
	-@rm -f *.aux *.cp *.cps *.fn *.fns *.ky *.kys *.log *.out \
	        *.pg *.pgs *.ps *.ps-2 *.tmp *.toc *.tp *.vr *.vrs
## this shall remove *exactly* those things that are *not* in version control:
clean: cleanaux
	-@rm -f $(DVIs) $(TXTs) $(PDFs) info/*.info* html/*

## this removes also things in VC (svn, when they are remade by "make"):
distclean : clean
#	-@rm -f $(PDFs)

../README: $(TEXISRC)
	$(MAKETXT) readme.texi \
	| perl -pe 'last if /^Concept Index/;' \
	> ../README
#	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' \

../ANNOUNCE: $(TEXISRC)
	$(MAKETXT) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

README.Microsoft : README.Microsoft.texi
	$(MAKETXT) README.Microsoft.texi \
	| perl -pe 'last if /^Concept Index/;' > README.Microsoft

../NEWS: Makefile ess-defs.texi newfeat.texi news.texi
	$(MAKETXT) news.texi > ../NEWS

../ONEWS: Makefile ess-defs.texi onewfeat.texi onews.texi
	$(MAKETXT) onews.texi > ../ONEWS

### File Dependencies

info/ess.info: $(TEXISRC) ../VERSION
	@echo "making Info documentation..."
	$(MAKEINFO) ess.texi
	mv -f ess.info info/

install: install-info install-other-docs

install-info : info/ess.info
	-$(INSTALLDIR) $(INFODIR)
	$(INSTALL) info/ess.info $(INFODIR)
	test -f $(INFODIR)/dir || $(INSTALL) dir.txt $(INFODIR)/dir
	grep $(ESSINFONODE1) $(INFODIR)/dir || \
	(echo >> $(INFODIR)/dir; echo 'Emacs' >> $(INFODIR)/dir; \
	echo $(ESSINFONODE1) $(ESSINFONODE2) >> $(INFODIR)/dir; \
	echo $(ESSINFONODE3) >> $(INFODIR)/dir)

install-other-docs: pdf html
	-$(INSTALLDIR) $(DOCDIR)
	$(INSTALL) ess.dvi ess.pdf readme.dvi readme.pdf ../README ../ANNOUNCE $(DOCDIR)
	$(INSTALL) html/ess.html html/readme.html $(DOCDIR)
	$(INSTALL) refcard/refcard.pdf $(DOCDIR)

uninstall :
	-cd $(DOCDIR) && $(UNINSTALL) ess.dvi ess.pdf readme.dvi readme.pdf README ANNOUNCE
	-cd $(DOCDIR) && $(UNINSTALL) ess.html readme.html 
	-cd $(DOCDIR) && $(UNINSTALL) refcard.pdf 

##  TO DO:  ess_toc.html & readme_toc.html should be merged into index.html
html/ess.html: $(TEXISRC)
	@echo "making HTML documentation..."
	mkdir -p html
	$(MAKEHTML) ess.texi > html/ess.html
#MM	$(TEXI2HTML) $(SPLITCHAP) ess.texi
#MM	test -d ess && cp -p ess/ess_toc.html html/index.html || cp -p ess_toc.html html/index.html
#MM	test -d ess && mv -f ess/*.html html || mv -f *.html html

html/readme.html: $(TEXISRC)
	$(MAKEHTML)  --no-validate readme.texi > html/readme.html
##                   ^^^^^^^^^^^^^ (design bug: "FIXME" in ess.texi)
#MM	$(TEXI2HTML) readme.texi
#	mv -f readme.html html

refcard/refcard.pdf: refcard/refcard.tex
	(cd refcard ; pdflatex refcard)
