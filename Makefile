## $Id: Makefile,v 5.31 2000/03/30 07:18:48 maechler Exp $
## Top Level Makefile

include ./Makeconf
##      ========== {edit that one if any !}

## Finally using the "ess/VERSION" file -- for everything.
## These only work with GNU make, but are only used by ESS maintainers
ESSVERSION=$(shell cat VERSION)
ESSVERSIONDIR=ess-$(ESSVERSION)
## The following MUST NOT contain "."'s.
ESSVERSIONTAG=ess-$(shell sed 's/\./_/g' VERSION)

Subdirs = lisp doc

INTRO.DEPENDS=doc/credits.texi doc/inst_cvs.texi \
	doc/newfeat.texi  doc/authors.texi  doc/currfeat.texi \
	doc/inst_tar.texi doc/bugrept.texi  doc/license.texi  \
	doc/requires.texi doc/bugs.texi     doc/getting.texi  \
	doc/mailing.texi  doc/stabilty.texi


all install clean distclean realclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

ESS:
	cd lisp; make all

docs:
	cd doc; make all

## This target is used to create a new version of the tar-file.
## prefix'ing with "-" implies that errors are non-critical.

README : doc/readme.texi $(INTRO.DEPENDS)
	cd doc ; \
	$(MAKEINFOascii) readme.texi | perl -pe \
	'last if /^Concept Index/; print "For INSTALLATION, see way below.\n\n" if /^\s*ESS grew out of/' > ../README

ANNOUNCE: doc/announc.texi $(INTRO.DEPENDS)
	cd doc; $(MAKEINFOascii) announc.texi \
	| perl -pe 'last if /^Concept Index/;' > ../ANNOUNCE

dist: README ANNOUNCE docs
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(ESSVERSION),"
	@echo "** from $(ESSVERSIONDIR)"
	@echo "** (must set CVSROOT, etc, prior to checkout for security)"
	@echo "**********************************************************"
	@echo "** Committing README and ANNOUNCE **"
	cvs commit -m "Updating README, ANNOUNCE for new version [make dist]" \
		README ANNOUNCE
	cvs commit -m "Updating docs for new version [make dist]" docs
	@echo "** Adding log-entry to ChangeLog file"
	mv ChangeLog ChangeLog.old
	(echo `date "+%Y-%m-%d "` \
	     " ESS Maintainers <ess@franz.stat.wisc.edu>" ; \
	 echo; echo "  * Version $(ESSVERSION) released."; echo; \
	 cat ChangeLog.old ) > ChangeLog
	@echo "** Tagging the release **"
	cvs tag -R $(ESSVERSIONTAG)
	$(MAKE) tar
	@echo "** Placing tar and zip files **"
	scp ESS-$(ESSVERSION).tar.gz ess@franz.stat.wisc.edu:~/public_html
	scp ESS-$(ESSVERSION).zip    ess@franz.stat.wisc.edu:~/public_html

tar:
	@echo "** Exporting Files **"
	cvs export -D today ess
	@echo "** Creating tar file **"
	ln -s ess $(ESSVERSIONDIR)
	chmod a-w $(ESSVERSIONDIR)/lisp/*.el
	chmod a-w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	chmod u+w $(ESSVERSIONDIR)/doc/ess.info*
	chmod u+w $(ESSVERSIONDIR)/lisp/ess-site.el $(ESSVERSIONDIR)/Make*
	chmod u+w $(ESSVERSIONDIR)/doc/Makefile $(ESSVERSIONDIR)/lisp/Makefile
	tar hcvof ESS-$(ESSVERSION).tar $(ESSVERSIONDIR)
	gzip ESS-$(ESSVERSION).tar
	@echo "** Creating zip file **"
	zip -r ESS-$(ESSVERSION).zip $(ESSVERSIONDIR)
	@echo "** Cleaning up **"
	rm -rf ess $(ESSVERSIONDIR)


## PA's version, infinitely interesting...
#dist:
#        @if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
#        if [ "X$(OLD)" = "X" ]; then echo "No patch"; exit 1; fi
#        @echo "**********************************************************"
#        @echo "** Making distribution of auctex for release $(TAG)"
#        @echo "**********************************************************"
#        if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
#        rm -f $(WWWDIR)/version
#        echo $(TAG) > $(WWWDIR)/version
#        perl -pi.bak -e "s/Version: $(OLD)/Version: $(TAG)/" \
#            $(AUCSRC) $(EXTRAFILES)
#        mv ChangeLog ChangeLog.old
#        echo `date "+%Y-%m-%d "` \
#             " Per Abrahamsen  <abraham@dina.kvl.dk>" > ChangeLog
#        echo >> ChangeLog
#        echo "  * Version" $(TAG) released. >> ChangeLog
#        echo >> ChangeLog
#        cat ChangeLog.old >> ChangeLog
#        cvs commit -m "Release $(OLD)++" tex.el
#        rm -f tex.el.orig
#        mv tex.el tex.el.orig
#        sed -e '/defconst AUC-TeX-date/s/"[^"]*"/"'"`date`"'"/' \
#            -e '/defconst AUC-TeX-version/s/"[^"]*"/"'$(TAG)'"/' \
#            < tex.el.orig > tex.el
#        rm -f $(REMOVE)
#        -cvs remove $(REMOVE)
#        -cvs add $(AUCSRC) $(EXTRAFILES)
#        -(cd doc; cvs add `echo $(DOCFILES) | sed -e s@doc/@@g` )
#        -(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
#        cvs commit -m "Release $(TAG)"
#        cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
#        mkdir auctex-$(TAG)
#        mkdir auctex-$(TAG)/style
#        mkdir auctex-$(TAG)/doc
#        cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
#        cp $(STYLESRC) auctex-$(TAG)/style
#        cp $(DOCFILES)  auctex-$(TAG)/doc
#        (cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
#        (cd doc; cp INSTALLATION README CHANGES ../auctex-$(TAG)/ )
#        cp doc/CHANGES $(FTPDIR)/CHANGES-$(TAG)
#        cp doc/auc-tex.ps $(FTPDIR)
#        cp ChangeLog $(FTPDIR)
#        cp doc/*.html $(WWWDIR)/doc
#        rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
#        rm -f $(FTPDIR)/auctex.tar.Z $(FTPDIR)/auctex.zip
#        tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
#        tar -cf - auctex-$(TAG) | compress > $(FTPDIR)/auctex.tar.Z
#        zip -r $(FTPDIR)/auctex auctex-$(TAG)
#        (cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)
#        cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
#                  -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex \
#                > $(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch ;  exit 0
