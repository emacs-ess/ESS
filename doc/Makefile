### Makefile - for DOCUMENTATION (./doc) of ESS distribution.
###
### File: $Revision: 1.50 $.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

# program to convert .texi{nfo} to .html
TEXI2HTML = texi2html -verbose -iso
SPLITCHAP = -split_chapter -menu -glossary

# To obtain sorted indices run:	"texindex ess.??".

#### no user servicable parts beyond this point ####

TEXISRC = announc.texi authors.texi bugrept.texi bugs.texi bugs-ms.texi     \
	credits.texi currfeat.texi ess.texi getting.texi help-s.texi        \
	help-sas.texi inst_cvs.texi inst_tar.texi license.texi mailing.texi \
	newfeat.texi readme.texi requires.texi stabilty.texi help-bugs.texi \
        ../VERSION

### Targets --

all  : info text html dvi pdf
info : info/ess.info
text : ../README ../ANNOUNCE
html : html/ess_toc.html html/readme_toc.html
docs : info text html
dvi  : ess.dvi readme.dvi
pdf  : ess.pdf readme.pdf
ps   : ess.ps  readme.ps

ess.dvi : $(TEXISRC); texi2dvi $<
ess.pdf : $(TEXISRC)
	texi2pdf ess.texi
#ess.pdf : $(TEXISRC); pdftex $<; pdftex $<
ess.ps  : ess.dvi; dvips $< -o $@

readme.dvi : $(TEXISRC); texi2dvi $<
readme.pdf : $(TEXISRC); pdftex $<; pdftex $<
readme.ps  : readme.dvi; dvips $< -o $@

clean :
	rm -f texput.log ess.aux ess.cp ess.ky ess.log ess.pg ess.toc \
	 ess.tp ess.vr ess.cps ess.fns ess.vrs ess.fn \
	 readme.aux readme.cp readme.ky readme.log readme.pg readme.toc \
	 readme.tp readme.vr readme.cps readme.fns readme.vrs readme.fn

distclean : clean
	rm -f ess.dvi ess.ps ess.pdf readme.dvi readme.ps readme.pdf
	rm -fr html

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
