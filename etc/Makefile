### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

#ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.*)
ETCFILES = SVN-REVISION BACKBUG?.BAT backbug? *.S sas-keys.* ess-sas-sh-command config.guess

#ICONS = $(wildcard icons/*.xpm)
ICONS = icons/*.xpm

all:
	@true

install :
	-$(INSTALLDIR) $(ETCDIR)/icons
	$(INSTALL) $(ETCFILES) $(ETCDIR)
	$(INSTALL) $(ICONS) $(ETCDIR)/icons
	chmod +x $(ETCDIR)/ess-sas-sh-command $(ETCDIR)/backbug? $(ETCDIR)/config.guess

uninstall :
	-cd $(ETCDIR) && $(UNINSTALL) $(ETCFILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ICONS)

clean:
	rm -f SVN-REVISION
distclean: clean

