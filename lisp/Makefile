## $Id: Makefile,v 1.39 1997/07/25 15:20:07 rossini Exp $
##
##
## $Log: Makefile,v $
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

VERSION = 4.9-b8

LASTVERSION = 4.8.MM6.XE3

# have to preload the files that contain macro definitions or the
# byte compiler will compile everything that references them
# incorrectly.  also preload a file that sets byte compiler options.
PRELOADS = -l ./ess-comp.el
# above was ess-site.el

## compile with noninteractive and relatively clean environment
## XEMACS 19.xx and before.
#BATCHFLAGS = -batch -no-site-file -no-init-file
## EMACS and XEMACS 20.x.
BATCHFLAGS = -batch --no-site-file --no-init-file

## files that contain key macro definitions.  almost everything
## depends on them because the byte-compiler inlines macro
## expansions.  everything also depends on the byte compiler
## options file since this might do odd things like turn off
## certain compiler optimizations.
CORE = ess.elc ess-site.elc

## Need both types, UNFORTUNATELY, since we want to BUILD and
## MAINTAIN.  Some one ought to fix this to be more GNUish.

SOURCES = ess.el \
	essd-s+3.el essd-r.el essd-xls.el \
	ess-site.el \
	ess-inf.el ess-mode.el ess-trns.el ess-help.el ess-xtra.el


OBJECTS=$(SOURCES:.el=.elc)

AUXFILES = COPYING Makefile NEWS README README.XE TODO \
	S-mode.texi ChangeLog

##S-mode-$(VERSION)/
SOURCESXE = S-mode-$(VERSION)/ess.el S-mode-$(VERSION)/ess-inf.el \
	S-mode-$(VERSION)/ess-mode.el S-mode-$(VERSION)/ess-trns.el  \
	S-mode-$(VERSION)/ess-help.el S-mode-$(VERSION)/ess-site.el \
	S-mode-$(VERSION)/ess-modeadds.el S-mode-$(VERSION)/ess-xtra.el 

AUXFILESXE = S-mode-$(VERSION)/COPYING S-mode-$(VERSION)/Makefile \
	S-mode-$(VERSION)/NEWS S-mode-$(VERSION)/README \
	S-mode-$(VERSION)/README.XE S-mode-$(VERSION)/TODO \
	S-mode-$(VERSION)/S-mode.texi S-mode-$(VERSION)/ChangeLog


S-mode: $(OBJECTS)

all:	S-mode info html

info:   S-mode.info

html:	html/S-mode_toc.html

install:	all
	$(INSTALL) S-mode.info* $(INFODIR)
	$(INSTALL) *.elc $(LISPDIR)

clean:
	rm -f $(OBJECTS) S-mode.info* S-mode.aux S-mode.cp S-mode.dvi \
	  S-mode.ky S-mode.log S-mode.pg S-mode.toc S-mode.tp S-mode.vr \
	  S-mode.fn texput.log

##distrib: $(SOURCES) $(AUXFILES)
##	chmod a-w *.el
##	chmod u+w S-site.el Makefile
##	tar cvf S-mode-$(VERSION).tar $(SOURCES) $(AUXFILES)
##	gzip S-mode-$(VERSION).tar

## In previous set of commands:  chmod u-w S-site.el Makefile

distrib.XE : $(SOURCESXE) $(AUXFILESXE)
	tar covf S-mode-$(VERSION).tar $(SOURCESXE) $(AUXFILESXE)
	gzip S-mode-$(VERSION).tar

patch: S-mode-$(VERSION).tar.gz S-mode-$(LASTVERSION).tar.gz
	-rm -rf S-mode-$(VERSION)
	mkdir S-mode-$(VERSION)
	(cd S-mode-$(VERSION); $(GZCAT) ../S-mode-$(VERSION).tar.gz | tar xvf -)
	-rm -rf S-mode-$(LASTVERSION)
	mkdir S-mode-$(LASTVERSION)
	(cd S-mode-$(LASTVERSION); $(GZCAT) ../S-mode-$(LASTVERSION).tar.gz|tar xvf -)
	-diff -bBw -C 2 S-mode-$(LASTVERSION) S-mode-$(VERSION) > diff-$(LASTVERSION)-$(VERSION)
	-rm -rf S-mode-$(VERSION)
	-rm -rf S-mode-$(LASTVERSION)

S-mode.info:	S-mode.texi
	@echo "making S-mode.info..."
	$(MAKEINFO) S-mode.texi

html/S-mode_toc.html: S-mode.texi
	@echo "making  HTML Info documentation"
	-(mkdir -p html; cd html; $(MAKEHTML) ../S-mode.texi)
	-@(cd html; ln -s S-mode_toc.html index.html)
	@echo "Test by pointing your browser to 'file://localhost/$$PWD/html/'"

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<

ess.elc:	ess.el
	$(EMACS) $(BATCHFLAGS)  -f batch-byte-compile ess.el

ess-site.elc:  ess-site.el ess.elc
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile ess-site.el

ess-help.elc:  ess-help.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-help.el

ess-inf.elc:  ess-inf.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-inf.el

ess-mode.elc:  ess-mode.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-mode.el

ess-trns.elc:  ess-trns.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile ess-trns.el


essd-r.elc : essd-r.el ess.el
	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-r.el

essd-s+3.elc : essd-s+3.el ess.el
	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-s+3.el

essd-s4.elc : essd-s4.el ess.el
	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-s4.el

essd-xls.elc : essd-xls.el ess.el
	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-xls.el

#essd-vst.elc : essd-vst.el ess.el
#	$(EMACS) $(BATCHFLAGS) -l ess.el -f batch-byte-compile essd-r.el

# Ignore this.
#(defun S-insert-make-rule (file)
#  (interactive "sFile:")
#  (insert (format "%s.elc:  %s.el $(CORE)\n\t@echo compiling %s.el...\n\t@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile %s.el" file file file file)))
