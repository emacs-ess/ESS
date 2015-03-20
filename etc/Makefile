### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

# In ../Makefile we already construct the  ESSR-VERSION  file :
# ESSR_VERSION = $(shell cat ESSR-VERSION)

#ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.*)
#ETCFILES = ESSR.R ess-developer.R SVN-REVISION *.S sas-keys.* ess-sas-sh-command
ETCFILES = SVN-REVISION *.S sas-keys.* ess-sas-sh-command *.jl

#ICONS = $(wildcard icons/*.xpm)
ICONS = icons/*.xpm

ESSR_UTIL_FILES = ESSR/LOADREMOTE ESSR/VERSION
ESSR_CODE_FILES = ESSR/R/*.R ESSR/R/.*.R
# ESSR_tarball = ESSR_$(ESSR_VERSION).tar.gz

all: #ESSR-VERSION $(ESSR_tarball) library/ESSR

## happens "above" as it is need also in ../lisp/ :
# ESSR-VERSION: $(ESSR_FILES)
#	(cd .. ; make etc/ESSR-VERSION)

# $(ESSR_tarball): $(ESSR_FILES)
#	R CMD build ESSR
# library/ESSR: $(ESSR_tarball)
#	R CMD INSTALL -l library ESSR

# rel:  $(ESSR_tarball)
#	[ x$$USER = xmaechler ] || (echo 'must be maechler'; exit 1 )
#	$(INSTALL) $(ESSR_tarball) $(UPLOAD_DIR)/pkgs/src/contrib

install :
	$(INSTALLDIR) $(ETCDIR)/icons
	$(INSTALLDIR) $(ETCDIR)/ESSR/R
	$(INSTALL) $(ETCFILES) $(ETCDIR)
	$(INSTALL) $(ICONS) $(ETCDIR)/icons
	$(INSTALL) $(ESSR_UTIL_FILES) $(ETCDIR)/ESSR
	$(INSTALL) $(ESSR_CODE_FILES) $(ETCDIR)/ESSR/R
	chmod +x $(ETCDIR)/ess-sas-sh-command

uninstall :
	-cd $(ETCDIR) && $(UNINSTALL) $(ETCFILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ICONS)
	-cd $(ETCDIR) && $(UNINSTALL) $(ESSR_UTIL_FILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ESSR_CODE_FILES)



## 'clean'     shall remove *exactly* those things that are *not* in version control
clean:
	rm -rf SVN-REVISION
## 'distclean' removes also things in VC (svn, when they are remade by "make"):
# distclean: clean
#	rm -rf ESSR_*.tar.gz
