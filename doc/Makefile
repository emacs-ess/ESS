### Makefile - for DOCUMENTATION (./doc) of ESS distribution.
###
### Maintainer: A.J. Rossini <rossini@stat.sc.edu>
### For ESS Version: 5.x.y
###
### File: $Revision: 1.29 $.
###
### Edit the makefile, type `make', and follow the instructions.  Based
### on (i.e. Borrowed from) VM 5.70, AUC-TeX 9.7p distributions.  Thanks
### Kyle and Per!

include ../Makeconf

# program to convert .texi{nfo} to .html
MAKEHTML = texi2html -verbose -iso
MAKEHTMLs = $(MAKEHTML) -split_chapter -menu -glossary

#### no user servicable parts beyond this point ####

MYWWWDIR = /none/at/this/time
MYFTPDIR = /none/at/this/time

# Those README.* that are made from *.texi  [well, all should....]
READMEtargets = README.SPLUS4WIN README.Microsoft

## ess.texi and things it @include s (alphabetically but 1st line)
ManSRC = ess.texi version.texi \
	 bugrept.texi bugs.texi credits.texi getting.texi \
	 help-s.texi help-sas.texi inst_tar.texi \
	 mailing.texi newfeat.texi
## readme.texi and things it @include s (alphabetically but 1st line):
ReadmeSRC = readme.texi version.texi \
	 authors.texi bugrept.texi currfeat.texi getting.texi \
	 inst_tar.texi license.texi mailing.texi newfeat.texi \
	 requires.texi stabilty.texi
## others (should these be better organized?)
texiSRC = $(ManSRC) $(ReadmeSRC) announc.texi bugs-ms.texi inst_cvs.texi \
	README.Microsoft.texi

DOCS =	$(READMEtargets) $(texiSRC) \
	README README.S README.XLispStat README.SAS \
	dir ess.info ess.info-1 ess.info-2 ess.info-3 ess.info-4 ess.dvi \
	TODO Why_S-mode_Rocks.DMS \
	README-19.28 README.additions NEWS \
	ajr-talk.tex rmh-talk.tex slverb.sty

DOCDIST = ess.ps ess.pdf \
	ajr-talk.ps ajr-talk.pdf \
	rmh-talk.ps rmh-talk.pdf

# The following works only with GNU Make for most parts.

### Targets --

all  : info txts html dvi pdf

# Documentation mentions info, not ess.info -- NOT readme.info <<< FIXME >>
info : ../info/ess.info ../README ../ANNOUNCE
dvi  :  ess.dvi readme.dvi
pdf  :  ess.pdf readme.pdf
txts : $(READMEtargets)

#
# If indexes don't index, run:	"texindex ess.??" to obtain the sorted indices.
#
html :	html/ess_toc.html html/readme_toc.html

ess.dvi : $(ManSRC); texi2dvi $<
ess.pdf : $(ManSRC); pdftex $<; pdftex $<
ess.ps  : ess.dvi; dvips $< -o $@
## If you don't have pdftex, you won't get the bookmarks, hyperlinks, etc..
# ess.pdf: ess.ps ; rm -f ess.pdf ; ps2pdf ess.ps > ess.pdf

readme.dvi : $(ReadmeSRC); texi2dvi $<
readme.pdf : $(ReadmeSRC); pdftex $<; pdftex $<
readme.ps  : readme.dvi; dvips $< -o $@

install : all
# no longer :  (directly via ../info/..
# 	mkdir -p $(INFODIR)
# 	$(INSTALL) ess.info* $(INFODIR)

clean : ; rm -f texput.log \
	 ess.aux ess.cp ess.ky ess.log ess.pg ess.toc \
	 ess.tp ess.vr ess.cps ess.fns ess.vrs ess.fn \
	 readme.aux readme.cp readme.ky readme.log readme.pg readme.toc \
	 readme.tp readme.vr readme.cps readme.fns readme.vrs readme.fn

distclean : clean
	rm -f ess.dvi ess.ps ess.pdf readme.dvi readme.ps readme.pdf
	rm -fr html

##-- Be careful: to recreate the info files you NEED a newer makeinfo !
realclean : distclean
	rm -f ess.info*

### Targets below here are only for developers. --------------------


### Replace Version numbers (as in ../lisp/Makefile !):
## Use $(shell ..) only here, so only these are GNU make dependent :
README.SPLUS4WIN announc.texi inst_*.texi : ../VERSION
	perl -pi -e 's/$(ESSVER_NR_REGEXP)/$${1}-$(shell cat $^)/go' $@

#readme.texi : ../VERSION $(ReadmeSRC)
#	perl -pi -e 's/($(ESS_REGEXP) \s*[Vv]ersion)\s+$(VER_NR_REGEXP)/$${1} $(shell cat $^)/go' $@

../README: $(ReadmeSRC)
	$(MAKEINFOascii) readme.texi \
	| perl -pe 'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' \
	> ../README

../ANNOUNCE: $(ReadmSRC)
	$(MAKEINFOascii) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

## ess.texi: use the perl for the above two combined:
ess.texi : ../VERSION
	perl -pi -e 's/$(ESSVER_NR_REGEXP)/$${1}-$(shell cat $^)/go;s/($(ESS_REGEXP) \s*[Vv]ersion\s+)$(VER_NR_REGEXP)/$${1}$(shell cat $^)/go' $@


##dist-doc: $(DOCDISTFILES)
##	@echo "**********************************************************"
##	@echo "** Making distribution of ESS-DOC for release $(shell cat $^)"
##	@echo "**********************************************************"
##	tar hcovf $(VERSIONDIR)-doc.tar $(DOCDISTFILES)
##	gzip $(VERSIONDIR)-doc.tar
##	cp $(VERSIONDIR)-doc.tar.gz $(MYWWWDIR)
##	cp $(VERSIONDIR)-doc.tar.gz $(MYFTPDIR)

### File Dependencies

version.texi: ../VERSION
	test -f $@ && rm -f $@ || true
	echo "@macro essver{}" > $@
	echo $(ESSVERSION) >> $@
	echo "@end macro" >> $@

../info/ess.info: $(ManSRC)
	@echo "making Info documentation..."
	$(MAKEINFO) ess.texi
	mv ess.info* ../info

##-- HTML:
##  TO DO:  ess_toc.html & readme_toc.html should be merged into index.html
##  =====
html/ess_toc.html: $(ManSRC)
	@echo "making HTML documentation..."
	-(mkdir -p html; cd html; $(MAKEHTMLs) ../ess.texi)
	-@(cd html; rm -f index.html; ln -s ess_toc.html index.html)
	@echo "Test by pointing your browser to 'file://localhost/$$PWD/html/index.html'"

html/readme_toc.html: $(ReadmeSRC)
	-(mkdir -p html; cd html; $(MAKEHTML) ../readme.texi)

README.Microsoft : README.Microsoft.texi
	$(MAKEINFOascii) README.Microsoft.texi \
	| perl -pe 'last if /^Concept Index/;' > README.Microsoft
