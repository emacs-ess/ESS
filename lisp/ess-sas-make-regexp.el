	 ;; SAS execution blocks, DATA/RUN, PROC/RUN, %MACRO statements
(insert (make-regexp 
	'("data" "run" "%macro" "%mend" "%go[ \t]*to" "%global" "%else" "%end")
	 t))
(insert (make-regexp 
	 '("%if" "%to" "%then" "%local" "%let" "%put" "%input" "%include" "%sysexec")
	 t))

	 ;; SAS statements
(insert (make-regexp 
	 '("abort" "array" "attrib" "by" "delete" "display" "dm" "drop")
	 t))
(insert (make-regexp 
	  '("else" "error" "file" "filename" "footnote\\(10?\\|[2-9]\\)?" "format")
	 t))
(insert (make-regexp 
	  '("go[ \t]*to" "goptions" "options" "if" "infile" "informat" "input" "keep" )
	 t))
(insert (make-regexp 
	  '("label" "length" "libname" "link" "merge" "missing" "modify" "note")
	 t))
(insert (make-regexp 
	  '("out" "otherwise" "output" "put" "rename" "retain" "select" "set" "skip")
	 t))
(insert (make-regexp 
	  '("when" "to" "then" "title\\(10?\\|[2-9]\\)?" "where" "window" "update")
	 t))
(insert (make-regexp 
	  '("change" "class" "exchange" "exclude" "freq" "id" "index")
	 t))
(insert (make-regexp 
	  '("plot" "save" "sum" "tables?" "weight" "with")
	 t))
(insert (make-regexp 
	  '("manova" "model" "repeated" "var" "value" "random")
	 t))

	 ;; SAS statements that must be followed by a semi-colon
(insert (make-regexp 
	 '("cards4?" "end" "endsas" "list" "lostcard" "page" "return" "stop")
	 t))

	 ;; SAS/GRAPH statements not handled above
(insert (make-regexp 
	 '("axis" "legend" "pattern" "symbol")
	 t))

	 ;; SAS functions and SAS macro functions
(insert (make-regexp 
	 '("abs" "arcos" "arsin" "atan" "betainv" "byte")
	 t))
(insert (make-regexp 
	'("ceil" "cinv" "collate" "compress" "cosh?" "css" "cv")
	 t))
(insert (make-regexp 
	'("daccdb" "daccdbsl" "daccsl" "daccsyd" "dacctab")
	 t))
(insert (make-regexp 
	'("depdb" "depdbsl" "depsl" "depsyd" "deptab")
	 t))
(insert (make-regexp 
	'("date" "datejul" "datepart" "datetime" "day" "dhms" "dif" "digamma" "dim")
	 t))
(insert (make-regexp 
	'("erfc?" "exp") 
	 t))
(insert (make-regexp 
	'("finv" "fipnamel?" "fipstate" "floor" "fuzz" "gaminv" "gamma")
	 t))
(insert (make-regexp 
	'("hms" "hbound" "hour" "indexc?" "input" "int" "intck" "intnx" "intrr" "irr")
	 t))
(insert (make-regexp 
	'("juldate" "kurtosis" "lag" "lbound" "left" "length" "lgamma" "log" "log10" "log2")
	 t))
(insert (make-regexp 
	'("max" "mdy" "mean" "min" "minute" "mod" "month" "mort" "n" "netpv" "nmiss" "normal" "npv")
	 t))
(insert (make-regexp 
	'("probbeta" "probbnml" "probchi" "probf" "probgam" "probhypr" "probit" "probnegb" "probnorm" "probt")
	 t))
(insert (make-regexp 
	'("ordinal" "poisson" "put" "qtr" "repeat" "reverse" "right" "round")
	 t))
(insert (make-regexp 
	'("range" "rank" "ranbin" "rancau" "ranexp" "rangam" "rannor" "ranpoi" "rantbl" "rantri" "ranuni")
	 t))
(insert (make-regexp 
	'("saving" "scan" "second" "sign" "sinh?" "sqrt" "std" "stderr" "stfips" "stnamel?" "substr" "sum" "symget")
	 t))
(insert (make-regexp 
	'("tanh?" "time" "timepart" "tinv" "today" "translate" "trigamma" "trim" "trunc")
	 t))
(insert (make-regexp 
	'("uniform" "upcase" "uss" "var" "verify")
	 t))
(insert (make-regexp 
	'("weekday" "year" "yyq" "zipfips" "zipnamel?" "zipstate")
	 t))

       ;; SAS functions introduced in Technical Report P-222
(insert (make-regexp 
	'("airy" "band" "blshift" "brshift" "bnot" "bor" "bxor")
	 t))
(insert (make-regexp 
	'("cnonct" "fnonct" "tnonct" "compbl" "dairy" "dequote")
	 t))
(insert (make-regexp 
	'("ibessel" "jbessel" "indexw" "inputc" "inputn" "putc" "putn" )
	 t))
(insert (make-regexp 
	'("lowcase" "quote" "resolve" "soundex" "sysprod" "tranwrd" "trimn")
	 t))

       ;; SCL functions that are known to work with SAS macro function %sysfunc
(insert (make-regexp 
	'("attrc" "attrn" "close" "dclose" "dnum" "dopen" "dread" "fopen" "fclose")
	 t))
(insert (make-regexp 
	'("cexist" "exist" "fetchobs" "fileexist" "finfo" "fput" "fwrite")
	 t))
(insert (make-regexp 
	'("getoption" "getvarc" "getvarn" "libname" "libref" "open" "optgetn" "optsetn")
	 t))
(insert (make-regexp 
	'("pathname" "sysmsg" "varfmt" "varlabel" "varnum" "vartype")
	 t))
