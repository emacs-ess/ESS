.PHONY: default package

default : package

package : 
	cp *.pdf ~/public_html
	cd .. && tar jcvof ~/public_html/useR-ESS-package.tbz useR-ESS
	chmod ugo+rx -R ~/public_html
