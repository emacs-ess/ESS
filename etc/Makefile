### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.*) \
	ess-sas-sh-command config.guess

ICONS = $(wildcard icons/*.xpm) 

all clean distclean:
	@true

install : 
	-$(INSTALLDIR) $(ETCDIR)
	$(INSTALL) $(ETCFILES) $(ETCDIR)
	-$(INSTALLDIR) $(ETCDIR)/icons
	$(INSTALL) $(ICONS) $(ETCDIR)/icons
