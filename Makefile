## $Id: Makefile,v 5.8 1998/11/13 23:47:27 rossini Exp $
## Top Level Makefile
SHELL = /bin/sh

ESSVERSION=5.1.1
ESSVERSIONDIR=ess-$(ESSVERSION)

Subdirs = lisp doc

all install clean distclean:
	@for D in $(Subdirs); do cd $$D; $(MAKE) $@ ; cd .. ; done

ESS:
	cd lisp; make all
docs:
	cd doc; make all

## This target is used to create a new version of the tar-file.
## prefix'ing with "-" implies that errors are non-critical.

## Instead of doing a checkout and exclude, we could do an export,
## which ought to morally be cleaner.
dist: 
	@echo "**********************************************************"
	@echo "** Making distribution of ESS for release $(VERSION),"
	@echo "** from $(ESSVERSIONDIR)"
	@echo "** (must set CVSROOT, etc, prior to checkout for security)"
	@echo "**********************************************************"
	cvs co ess
	ln -s ess $(ESSVERSIONDIR)
	tar hcvof ESS-$(ESSVERSION).tar -X CVS $(ESSVERSIONDIR)
	chmod a-w $(ESSVERSIONDIR)/lisp/*.el
	chmod a-w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	-chmod a-w $(ESSVERSIONDIR)/doc/ess.info* $(ESSVERSIONDIR)/doc/ess.dvi
	chmod u+w $(ESSVERSIONDIR)/lisp/ess-site.el $(ESSVERSIONDIR)/Makefile
	chmod u+w $(ESSVERSIONDIR)/doc/Makefile $(ESSVERSIONDIR)/lisp/Makefile
	gzip ESS-$(ESSVERSION).tar
	-chmod a+w $(ESSVERSIONDIR)/lisp/*.el
	-chmod a+w $(ESSVERSIONDIR)/ChangeLog $(ESSVERSIONDIR)/doc/*
	-chmod a+w $(ESSVERSIONDIR)/doc/ess.info* $(ESSVERSIONDIR)/doc/ess.dvi
	rm $(ESSVERSIONDIR)
	cvs release -d ess
	scp ESS-$(ESSVERSION).tar.gz ess@franz.stat.wisc.edu:~/public_html

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

## Version for 5.0.

##dist:
##	@echo "**********************************************************"
##	@echo "** Making distribution of ESS for release $(VERSION)"
##	@echo "**********************************************************"
##	chmod a-w $(VERSIONDIR)/*.el
##	chmod a-w $(VERSIONDIR)/ChangeLog $(VERSIONDIR)/Doc/*
##	chmod a-w $(VERSIONDIR)/ess.info* $(VERSIONDIR)/ess.dvi
##	chmod u+w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
##	chmod ugo+rx $(VERSIONDIR)/ess-sas-sh-command
##	chmod ugo+x $(DISTSCRIPTS)
##	tar hcovf $(VERSIONDIR).tar $(DISTFILES)
##	gzip $(VERSIONDIR).tar
##	cp $(VERSIONDIR).tar.gz $(MYWWWDIR)
##	cp $(VERSIONDIR).tar.gz $(MYFTPDIR)
##	chmod u+w $(VERSIONDIR)/README
##	cp $(VERSIONDIR)/README $(MYWWWDIR)/$(VERSIONDIR)-README
##	cp $(VERSIONDIR)/README $(MYFTPDIR)/$(VERSIONDIR)-README
##	chmod u-w $(VERSIONDIR)/ess-site.el $(VERSIONDIR)/Makefile
##	chmod u-w $(VERSIONDIR)/README
##	chmod u+w $(VERSIONDIR)/Doc
