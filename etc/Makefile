### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.* icons/*.xpm) \
	ess-sas-sh-command config.guess

install : 
	-$(INSTALLDIR) $(ETCDIR)
	$(INSTALL) $(ETCFILES) $(ETCDIR)
