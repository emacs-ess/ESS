## $Id: Makefile,v 1.46 1997/07/29 11:27:11 rossini Exp $
##
##
## $Log: Makefile,v $
## Revision 1.46  1997/07/29 11:27:11  rossini
## makefile is cleaner.  Added AucTeX stuff for information.
##
## Revision 1.45  1997/07/29 11:25:41  rossini
## stuff.
##
## Revision 1.44  1997/07/28 12:58:34  rossini
## S-mode -> ESS.
##
## Revision 1.43  1997/07/28 12:09:39  rossini
## updated version number.
##
## Revision 1.42  1997/07/26 01:23:26  rossini
## redid again.
##
## Revision 1.41  1997/07/25 21:13:37  rossini
## don't need ess-xtra yet!
##
## Revision 1.40  1997/07/25 21:06:07  rossini
## added ess-vars, essd-s3, essd-s4 to SOURCES.
##
## Revision 1.39  1997/07/25 15:20:07  rossini
## redid to use ess-comp, rather than ess-mode, as the preload.
##
## Revision 1.38  1997/07/17 21:10:59  rossini
## increased version.
##
## Revision 1.37  1997/07/03 13:32:14  rossini
## redid compile order.
##
## Revision 1.36  1997/06/22 23:36:39  rossini
## added essd files.
##
## Revision 1.35  1997/06/19 20:39:23  rossini
## edited comments.
##
## Revision 1.34  1997/06/19 20:36:43  rossini
## removed spurrious comments.
##
## Revision 1.33  1997/06/19 20:36:11  rossini
## redid dependencies.
##
## Revision 1.32  1997/06/18 18:30:07  rossini
## added $(INSTALL)  (thanks, MM).
##
## Revision 1.31  1997/06/17 02:14:20  rossini
## ess-extras - ess-xtra.el
##
## Revision 1.30  1997/06/17 01:58:45  rossini
## trans -> trns
##
## Revision 1.29  1997/06/17 01:42:01  rossini
## changed preloads, for Emacs (thanks, Martin).
##
## Revision 1.28  1997/06/15 09:02:51  rossini
## reordered essd and ess-site files in dependencies, for now.
##
## Revision 1.27  1997/06/15 08:11:01  rossini
## added variable sub.
##
## Revision 1.26  1997/04/04 17:31:37  rossini
## better variable labels.
##
## Revision 1.25  1997/04/04 16:13:49  rossini
## version changed to 4.9b1
##
## Revision 1.24  1997/04/04 16:08:36  rossini
## changed XEmacs to XE
##
## Revision 1.23  1997/04/02 19:32:13  rossini
## sd
##
## Revision 1.22  1997/04/02 15:35:23  rossini
## more in line with GNU standards.
##
## Revision 1.21  1997/04/02 15:31:15  rossini
## changed BATCHFLAGS.  Removed old preloads.  Made /usr/local the
## default.
##
## Revision 1.20  1997/03/10 16:29:19  rossini
## edited for easier XEmacs distribution.
##
## Revision 1.19  1997/03/08 00:42:04  rossini
## some makes (like the usual Sun) seem to hate lines like:
## 	.el.elc:
##         	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<
## so we won't be cool...
## 
##

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

VERSION = 4.9-b10

LASTVERSION = 4.8.MM6.XE3

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

SOURCES = ess-vars.el ess.el \
	essd-s3.el essd-s4.el essd-s+3.el essd-r.el essd-xls.el \
	ess-site.el \
	ess-inf.el ess-mode.el ess-trns.el ess-help.el # ess-xtra.el

OBJECTS = $(SOURCES:.el=.elc)

AUXFILES = COPYING Makefile NEWS README TODO \
	ESS.texi ChangeLog


### Commands

all:	ESS info html

ESS: $(OBJECTS)

info:   ESS.info

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


#### The next stuff are from the AUCTeX Distribution.
##
##dist : 	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
##	if [ "X$(OLD)" = "X" ]; then echo "No patch"; exit 1; fi
##	@echo "**********************************************************"
##	@echo "** Making distribution of auctex for release $(TAG)"
##	@echo "**********************************************************"
##	if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
##	rm -f $(WWWDIR)/version
##	echo $(TAG) > $(WWWDIR)/version
##	perl -pi.bak -e "s/Version: $(OLD)/Version: $(TAG)/" \
##	    $(AUCSRC) $(EXTRAFILES)
##	mv ChangeLog ChangeLog.old
##	echo `date "+%a %b %d %T %Y "` \
##	     " Per Abrahamsen  <abraham@dina.kvl.dk>" > ChangeLog
##	echo >> ChangeLog
##	echo "	* Version" $(TAG) released. >> ChangeLog
##	echo >> ChangeLog
##	cat ChangeLog.old >> ChangeLog
##	cvs commit -m "Release $(OLD)++" tex.el
##	rm -f tex.el.orig
##	mv tex.el tex.el.orig
##	sed -e '/defconst AUC-TeX-date/s/"[^"]*"/"'"`date`"'"/' \
##	    -e '/defconst AUC-TeX-version/s/"[^"]*"/"'$(TAG)'"/' \
##	    < tex.el.orig > tex.el
##	rm -f $(REMOVE) 
##	-cvs remove $(REMOVE) 
##	-cvs add $(AUCSRC) $(EXTRAFILES)
##	-(cd doc; cvs add `echo $(DOCFILES) | sed -e s@doc/@@g` )
##	-(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
##	cvs commit -m "Release $(TAG)"
##	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
##	mkdir auctex-$(TAG) 
##	mkdir auctex-$(TAG)/style
##	mkdir auctex-$(TAG)/doc 
##	cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
##	cp $(STYLESRC) auctex-$(TAG)/style
##	cp $(DOCFILES)  auctex-$(TAG)/doc
##	(cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
##	(cd doc; cp INSTALLATION README CHANGES ../auctex-$(TAG)/ )
##	cp doc/CHANGES $(FTPDIR)/CHANGES-$(TAG)
##	cp doc/auc-tex.ps $(FTPDIR)
##	cp ChangeLog $(FTPDIR)
##	cp doc/*.html $(WWWDIR)/doc
##	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
##	rm -f $(FTPDIR)/auctex.tar.Z $(FTPDIR)/auctex.zip
##	tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
##	tar -cf - auctex-$(TAG) | compress > $(FTPDIR)/auctex.tar.Z
##	zip -r $(FTPDIR)/auctex auctex-$(TAG)
##	(cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)
##	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
##	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex \
##		> $(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch ;  exit 0
##
##
##patch:
##	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
##	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex
##
##
##autoloads: auto-autoloads.el
##
##auto-autoloads.el: $(AUCSRC) $(CONTRIB)
##	$(EMACS) -batch -q -no-site-file \
##		-eval '(setq autoload-target-directory "'`pwd`'/")' \
##		-l autoload \
##		-f batch-update-autoloads $?


### TARGETS

ESS.info:	ESS.texi
	@echo "making ESS.info..."
	$(MAKEINFO) ESS.texi

html/ESS_toc.html: ESS.texi
	@echo "making  HTML Info documentation"
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
