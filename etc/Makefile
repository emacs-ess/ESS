### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

#ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.*)
ETCFILES = ESSR.R ess-developer.R SVN-REVISION *.S sas-keys.* ess-sas-sh-command

#ICONS = $(wildcard icons/*.xpm)
ICONS = icons/*.xpm

all:
	@true

install :
	-$(INSTALLDIR) $(ETCDIR)/icons
	$(INSTALL) $(ETCFILES) $(ETCDIR)
	$(INSTALL) $(ICONS) $(ETCDIR)/icons
	chmod +x $(ETCDIR)/ess-sas-sh-command 

uninstall :
	-cd $(ETCDIR) && $(UNINSTALL) $(ETCFILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ICONS)

clean:
	rm -f SVN-REVISION
distclean: clean

