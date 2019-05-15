include ../Makeconf

TEXISRC = announc.texi authors.texi bugrept.texi bugs.texi \
	credits.texi currfeat.texi ess.texi ess-defs.texi \
	help-s.texi help-sas.texi installation.texi \
	license.texi mailing.texi onewfeat.texi news.texi onews.texi \
	newfeat.texi requires.texi help-bugs.texi \
        help-jags.texi allnews.texi Makefile ../Makeconf ../VERSION

PDFs = ess.pdf readme.pdf refcard/refcard.pdf
TXTs = ../README ../ANNOUNCE ../NEWS ../ONEWS

# Don't build pdf/html by default
all  : info text
info : info/ess.info
text : $(TXTs)
html : html/ess.html html/readme.html html/news.html
pdf  : $(PDFs)

ess.pdf : $(TEXISRC)
	$(TEXI2PDF) ess.texi

readme.pdf : $(TEXISRC); $(TEXI2PDF) readme.texi

distclean clean:
	@rm -f *.aux *.cp *.cps *.fn *.fns *.ky *.kys *.log *.out \
	        *.pg *.pgs *.tmp *.toc *.tp *.vr *.vrs
	@rm -f $(TXTs) $(PDFs) info/*.info* \
		html/*  *.info dir

../README: $(TEXISRC)
	-$(MAKETXT) readme.texi \
	| perl -pe 'last if /^Concept Index/;' > $@

../ANNOUNCE: $(TEXISRC)
	-$(MAKETXT) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > $@

../NEWS: Makefile ess-defs.texi newfeat.texi news.texi
	-$(MAKETXT) news.texi > $@

../ONEWS: Makefile ess-defs.texi onewfeat.texi onews.texi
	-$(MAKETXT) onews.texi > $@

### File Dependencies

info/ess.info: $(TEXISRC) ../VERSION
	@echo "making Info documentation..."
	-$(MAKEINFO) ess.texi
	mv -f ess.info info/

install: install-info install-other-docs

install-info : info/ess.info
	-$(INSTALLDIR) $(INFODIR)
	$(INSTALL) info/ess.info $(INFODIR)
	$(INSTALL) info/dir $(INFODIR)
	grep $(ESSINFONODE1) $(INFODIR)/dir || \
	(echo >> $(INFODIR)/dir; echo 'Emacs' >> $(INFODIR)/dir; \
	echo $(ESSINFONODE1) $(ESSINFONODE2) >> $(INFODIR)/dir; \
	echo $(ESSINFONODE3) >> $(INFODIR)/dir)

install-other-docs: pdf html
	-$(INSTALLDIR) $(DOCDIR)
	$(INSTALL) ess.pdf readme.pdf $(DOCDIR)
	$(INSTALL) html/ess.html html/readme.html html/news.html $(DOCDIR)
	$(INSTALL) refcard/refcard.pdf $(DOCDIR)

uninstall :
	-cd $(DOCDIR) && $(UNINSTALL) ess.pdf readme.pdf README ANNOUNCE
	-cd $(DOCDIR) && $(UNINSTALL) ess.html readme.html news.html
	-cd $(DOCDIR) && $(UNINSTALL) refcard.pdf

##  TO DO:  ess_toc.html & readme_toc.html should be merged into index.html
html/ess.html: $(TEXISRC)
	@echo "making HTML documentation..."
	mkdir -p html
	-$(MAKEHTML) html/ess.html ess.texi

html/readme.html: $(TEXISRC)
	-$(MAKEHTML) html/readme.html --no-validate readme.texi

html/news.html: $(TEXISRC)
	mkdir -p html
	-$(MAKEHTML) html/news.html allnews.texi

refcard/refcard.pdf: refcard/refcard.tex
	(cd refcard ; pdflatex refcard)
