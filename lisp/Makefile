### Makefile - for compiled e-lisp of ESS distribution.

install: dist
	-$(INSTALLDIR) $(LISPDIR)
	$(INSTALL) $(ELS) ess-autoloads.el $(LISPDIR)
	$(INSTALL) $(ELC) $(LISPDIR)
	if [ -f /etc/debian_version -a -n "$(SITELISP)" -a ! -f "$(SITELISP)/ess-site.el" ] ; \
	then \
		ln -s $(LISPDIR)/ess-site.el $(SITELISP)/ess-site.el ; \
	fi;

obsolete/%.elc: obsolete/%.el
	$(COMPILE-SIMPLE) $<
