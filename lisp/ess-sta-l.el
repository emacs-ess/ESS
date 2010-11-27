;;; ess-sta-l.el --- Stata customization

;; Copyright (C) 1999--2000, Thomas Lumley, A. J. Rossini, Brendan Halpin.
;; Copyright (C) 1997--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Authors: Thomas Lumley <thomas@biostat.washington.edu>,
;;         	     Brendan Halpin <brendan@essex.ac.uk>
;; Created: 2 Nov 1997
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: start up, configuration.

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This is based upon Version 0.4 of Stata mode.




;;
;; Stata modes.  Emacs modes for using the Stata statistical package
;; Modified from S-mode, comint-mode
;;
;; (c) thomas lumley 1997
;;
;;  version 0.4  20/7/97
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;


(require 'make-regexp)  ; it's now local to the directory.
;;(load-library "make-regexp") ;; this is necessary for
			     ;; ado-set-font-lock-keywords
;; only needed in Emacs >= 22.x and newish Xemacsen:
(unless (boundp 'c-emacs-features)
  (require 'cc-vars));; for syntax-table

;(setq max-lisp-eval-depth 500)
(eval-when-compile
  (setq max-lisp-eval-depth (max 600 max-lisp-eval-depth)))

(defconst ess-help-STA-sec-keys-alist
  '((?d . "Description")
    (?e . "Examples")
    (?o . "Options")
    (?s . "Also see"))
  "Help section keys for S4.
`key' indicates the keystroke to use to search for the section heading
`string' in an Stata help file. `string' is used as part of a
regexp-search, and so specials should be quoted.
")

(defconst ess-help-STA-sec-regex "^[A-Z a-z]+:?\n^[-]+$"
  "Reg(ular) Ex(pression) of section headers in help file.")

(defvar STA-syntax-table nil "Syntax table for Stata code.")
(if STA-syntax-table
    nil
  (setq STA-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." STA-syntax-table) ;nullify escape meaning
  (modify-syntax-entry ?\$ "." STA-syntax-table)
  (modify-syntax-entry ?` "(\'" STA-syntax-table)
  (modify-syntax-entry ?\' ")`" STA-syntax-table)
  ;;--------- begin cut-and-paste from  lisp/progmodes/c-langs.el
  (cond
   ;; XEmacs 19, 20, 21
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" STA-syntax-table)
    (modify-syntax-entry ?*  ". 23"   STA-syntax-table))
   ;; Emacs 19, 20, 21
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" STA-syntax-table)
    (modify-syntax-entry ?*  ". 23"   STA-syntax-table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs"))
   )
  (modify-syntax-entry ?\n "> b"  STA-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" STA-syntax-table)
  ;;--------- end cut-and-paste ------------------
  (modify-syntax-entry ?+ "." STA-syntax-table)
  (modify-syntax-entry ?- "." STA-syntax-table)
  (modify-syntax-entry ?= "." STA-syntax-table)
  (modify-syntax-entry ?% "." STA-syntax-table)
  (modify-syntax-entry ?< "." STA-syntax-table)
  (modify-syntax-entry ?> "." STA-syntax-table)
  (modify-syntax-entry ?& "." STA-syntax-table)
  (modify-syntax-entry ?| "." STA-syntax-table)
  (modify-syntax-entry ?~ "." STA-syntax-table))


(defun ado-set-font-lock-keywords ()
  "Create font lock keywords for Stata syntax. This is from the
ado-mode of Bill Rising <brising@jhsph.edu>, and uses make-regexp."
  ;; (make-local-variable 'ado-font-lock-keywords)
  (interactive)
  (list
   ;; special highlighting
   ;; program definitions
   (eval-when-compile
     (make-regexps
      '(("^\\*!.*") font-lock-keyword-face)
      ))
   (eval-when-compile
     (make-regexps
      "^"
      '((
	 "pr" "pro" "prog" "progr" "progra" "program"
	 ) font-lock-keyword-face)
      "[ \t]+"
      '((
	 "de" "def" "defi" "defin" "define"
	 "di" "dir"
	 "drop"
	 "l" "li" "lis" "list"
	 ) font-lock-type-face nil)
      "[ \t]+"
      '(("[_a-z]+[_a-z0-9]*") font-lock-keyword-face nil)
      ))
   (eval-when-compile
     (make-regexps
      '(("^[ \t]*version") font-lock-reference-face)
      "[ \t]*"
      '(("1.0 2.0 2.1 3.0 3.1 4.0 5.0 6 6.0") font-lock-type-face)
      ))
   (eval-when-compile
     (make-regexps
      "^"
      '(("end" "pause"
         ) font-lock-keyword-face)
      "[ /t]*.*$"
      ))
   ;; delimit command
   (eval-when-compile
     (make-regexps
      '(("^[ \t]*#delimit") font-lock-reference-face)
      "\\s-*"
      '(("\\(cr\\|;\\)\\s-*$") font-lock-type-face nil)
      ))
   ;; set command (with endless options!)
   (eval-when-compile
     (make-regexps
      '(("^[ \t]*set") font-lock-reference-face)
      "[ \t]+"
      '(("adosize" "ANSI"
         "b" "be" "bee" "beep" "checksum" "contents"
         "d" "di" "dis" "disp" "displ" "displa" "display"
         "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
         "help"
         "IBM"
         "l" "le" "lev" "leve" "level"
         "linesize"
         "lo" "log"
         "mat" "mats" "matsi" "matsiz" "matsize"
         "maxobs" "maxvar"
         "mem" "memo" "memor" "memory"
         "mo" "mor" "more"
         "obs"
         "ou" "out" "outp" "outpu" "output"
         "pagesize"
         "r" "rm" "rms" "rmsg"
         "se" "see" "seed" "seed0" "shell"
         "te" "tex" "text" "texts" "textsi" "textsiz" "textsize"
         "tr" "tra" "trac" "trace"
         "t" "ty" "typ" "type" "video"
         "vir" "virt" "virtu" "virtua" "virtual"
         )
        font-lock-reference-face t)
      "[ \t]*"
      '(("[a-zA-Z0-9]*") font-lock-type-face)
      ))
   ;; the constraint commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "cons" "const" "constr" "constra" "constrai" "constrain" "constraint"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "d"
         "de" "def" "defi" "defin" "define"
         "di" "dir"
         "drop"
         "l" "li" "lis" "list"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the confirm commands - could be a mess!
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "conf" "confi" "confir" "confirm"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "e" "ex" "exi" "exis" "exist" "existe" "existen" "existenc" "existence"
         "f" "fi" "fil" "file"
         "n" "nu" "num" "numb" "numbe" "number"
         "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
         ) font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "conf" "confi" "confir" "confirm"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "integer"
         ) font-lock-type-face)
      "[ \t]+"
      '((
         "n" "nu" "num" "numb" "numbe" "number"
         ) font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "conf" "confi" "confir" "confirm"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "n" "ne" "new"
         ) font-lock-type-face)
      "[ \t]+"
      '((
         "f" "fi" "fil" "file"
         "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
         ) font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "conf" "confi" "confir" "confirm"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "byte" "double" "float" "int" "long"
         "numeric"
         "str" "stri" "strin" "string"
         ) font-lock-type-face)
      "[ \t]+"
      '((
         "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
         ) font-lock-type-face)
      "\\b"
      ))
    ;;; the str# won't quite look right, but that's the breaks for using
    ;;; a tool like this...
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "conf" "confi" "confir" "confirm"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "str"
         ) font-lock-type-face)
      "[1-9]+[0-9]*[ \t]+"
      '((
         "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
         ) font-lock-type-face)
      "\\b"
      ))
   ;; the estimates commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "clear"
         "di" "dir" "dis" "disp" "displ" "displa" "display"
         "drop"
         "h" "ho" "hol" "hold"
         "li" "lis" "list"
         "loc" "loca" "local"
         "mat" "matr" "matri" "matrix"
         "post"
         "repost"
         "sca" "scal" "scala" "scalar"
         "u" "un" "unh" "unho" "unhol" "unhold"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the gph commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "gph"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "arc"
         "box"
         "clear" "close"
         "font"
         "line"
         "open"
         "pen" "point"
         "text"
         "vline" "vpoint" "vpoly" "vtext"
         )
        font-lock-type-face)
      "\\b"
      ))

   ;; some of the matrix commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("mat" "matr" "matri" "matrix") font-lock-reference-face)
      "[ \t]+"
      '(("ac" "acc" "accu" "accum"
         "cole" "coleq"
         "coln" "colna" "colnam" "cloname" "colnames"
         "d" "def" "defi" "defin" "define"
         "di" "dir" "dispCns" "drop" "drop _all"
         "glsa" "glsac" "glsacc" "glsaccu" "glsaccum"
         "l" "li" "lis" "list" "makeCns" "mlou" "mlout" "post"
         "rowe" "roweq"
         "rown" "rowna" "rownam" "rowname" "rownames"
         "sco" "scor" "score"
         "sub" "subs" "subst" "substi" "substit" "substitu" "substitut" "substitute"
         "svd" "syme" "symei" "symeig" "symeige" "symeigen"
         "veca" "vecac" "vecacc" "vecaccu" "vecaccum"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the ml commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("ml") font-lock-reference-face)
      "[ \t]+"
      '(("b" "be" "beg" "begi" "begin"
         "check" "count"
         "de" "dep" "depn" "depna" "depnam" "depname" "depnames"
         "di" "dis" "disp" "displ" "displa" "display"
         "f" "fu" "fun" "func" "funct" "functi" "functio" "function"
         "gr" "gra" "grap" "graph"
         "init"
         "max" "maxi" "maxim" "maximi" "maximiz" "maximize"
         "me" "met" "meth" "metho" "method"
         "ml" "mlo" "mlou" "mlout"
         "mo" "mod" "mode" "model"
         "pl" "plo" "plot"
         "po" "pos" "post"
         "q" "qu" "que" "quer" "query"
         "re" "rep" "repo" "repor" "report"
         "sa" "sam" "samp" "sampl" "sample"
         "se" "sea" "sear" "searc" "search"
         "trace")
        font-lock-type-face)
      "\\b"
      ))
   ;; the net commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("net") font-lock-reference-face)
      "[ \t]+"
      '((
         "cd"
         "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
         "from" "get" "install"
         "link"
         "q" "qu" "que" "quer" "query")
        font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("net") font-lock-reference-face)
      "[ \t]+"
      '(("set") font-lock-reference-face)
      "[ \t]+"
      '(("ado" "other") font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("ado") font-lock-reference-face)
      "[ \t]+"
      '(("d" "de" "des" "desc" "descr" "descri" "describ" "describe"
         "dir"
         "uninstall")
        font-lock-type-face)
      "\\b"
      ))
   ;; the reshape commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("reshape") font-lock-keyword-face)
      "[ \t]+"
      '((
         "clear"
         "error"
         "i" "j"
         "long"
         "wide"
         "xi" "xij")
        font-lock-type-face)
      "\\b"
      ))
   ;; the return commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("ret" "retu" "retur" "return") font-lock-reference-face)
      "[ \t]+"
      '(("add" "clear" "local" "matrix" "scalar") font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("sret" "sretu" "sretur" "sreturn") font-lock-reference-face)
      "[ \t]+"
      '(("clear" "local") font-lock-type-face)
      "\\b"
      ))
   ;; the sts commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("sts") font-lock-reference-face)
      "[ \t]+"
      '((
         "g"
         "gen" "gene" "gener" "genera" "generat" "generate"
         "gr" "gra" "grap" "graph"
         "l" "li" "lis" "list"
         "t" "te" "tes" "test"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the sw commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("sw") font-lock-reference-face)
      "[ \t]+"
      '((
	 "cloglog" "cnreg" "cox" "ereg" "gamma" "glm" "gompertz" "hetprob"
	 "llogist" "lnormal" "logistic" "logit" "ologit" "oprobit"
	 "poisson" "probit" "qreg" "reg" "regr" "regre" "regres" "regress"
	 "scobit" "tobit" "weibull"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the window commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "win" "wind" "windo" "window"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "d"
         "di" "dia" "dial" "dialo" "dialog"
         "dir" "drop"
         "fo" "fop" "fope" "fopen"
         "fs" "fsa" "fsav" "fsave"
         "l" "list"
         "push"
         "stop" "stopb" "stopbo" "stopbox"
         ) font-lock-type-face)
      "\\b"
      ))
   ;; the window controls
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "win" "wind" "windo" "window"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "c" "co" "con" "cont" "contr" "contro" "control"
         ) font-lock-reference-face)
      '((
         "button" "check" "clear"
         "edit"
         "mcombo" "msimple"
         "radbegin"
         "radend"
         "radio"
         "scombo"
         "ssimple"
         "static"
         ) font-lock-type-face)
      "\\b"
      ))
   ;; the window manage commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "win" "wind" "windo" "window"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "man" "mana" "manag" "manage"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "forward"
         "minimize"
         "prefs load"
         "prefs save"
         "prefs default"
         "print graph"
         "print log"
         "restore"
         "update variable"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the window menu commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "win" "wind" "windo" "window"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "m" "me" "men" "menu"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "append popout"
         "append string"
         "append separator"
         "clear"
         "popout"
         "set"
         )
        font-lock-type-face)
      "\\b"
      ))
   ;; the xwindow commands
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "xwin" "xwind" "xwindo" "xwindow"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "de" "def" "defi" "defin" "define"
         "di" "dir"
         "drop"
         "l" "li" "lis" "list"
         )
        font-lock-type-face)
      "\\b"
      ))

   ;; all the endless Stata keywords (not in a good order)
   ;; first those keywords which must start line
   ;; note that these will look like text if preceded by a comment
   ;; (but comments shouldn't be before the command, anyway)

   (eval-when-compile
     (make-regexps
      "^[ \t]+"
      '((
         "cap" "capt" "captu" "captur" "capture"
         "char" "err" "erro" "error" "e" "ex" "exi" "exit"
         "par" "pars" "parse"
         "set"
         ) font-lock-reference-face)
      "\\b"
      ))
   ;; here are some keywords which appear in the middle of lines
   ;; note that the really short abbreviations could make a mess of things
   ;;
   ;; These are split to allow compiling!
   (eval-when-compile
     (make-regexps
      "\\b"
      '((
         "_huber" "_qreg" "_robust"
         "acprplot" "adjust"
         "adopath" "alpha"
         "an" "ano" "anov" "anova" "arch"
         "areg" "arima"
         "as" "ass" "asse" "asser" "assert"
         "avplot" "avplots"
         "bcskew0"
         "be" "bee" "beep"
         "biprobit" "bitest" "bitesti" "blogit"
         "boxcox" "bprobit" "br" "break" "brier"
         "bro" "brow" "brows" "browse"
         "bsqreg" "bstat" "by"
         "canon" "cat" "cc" "cci" "cchart" "centile" "cf" "ci" "cii"
         "clogi" "clogit" "clogitp" "cloglog"
         "close" "cmdtool"
         "cnr" "cnre" "cnreg" "cnsreg" "codebook" "compare"
         "copy"
         "cor" "corc" "corr" "corre" "correl" "correla" "correlat" "correlate"
         "corrgram"
         "cou" "coun" "count"
         "cox"	"cprplot" "_crcswxx" "cs" "csi"
         "ct" "ctset" "cttost"
         "cumul" "cusum")
        font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
         "dfbeta" "dfuller" "di"
         "dir" "dis" "disp" "disp_res" "disp_s"
         "displ" "displa" "display"
         "do" "dotplot"
         "dprobit" "ds" "dstdize" "dwstat"
         "eivreg" "eq" "ereg"
         "fac" "fact" "facto" "factor"
         "fit" "for" "fpredict"
         "fracplot" "fracpoly" "fsl"
         ) font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "gettoken" "gladder" "glm" "glmpred" "glogit" "gnbreg" "gompertz"
         "gphdot" "gphpen" "graph" "gprobit" "greigen" "grmeanby"
         "hadimvo" "hausman" "heckman" "heckprob" "hetprob" "hettest" "hilite"
         "hist" "hlu" "hotel"
         "iqreg" "istdize" "iis"
         "ins" "insp" "inspe" "inspec" "inspect"
         "integ" "intreg" "ir" "iri" "ivreg"
         "kap" "kappa" "kapwgt" "kdensity" "ksm" "ksmirnov" "ktau"
         "kwallis"
         ) font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "l" "ladder" "lfit" "lincom" "linktest"
         "li" "lis" "list"
         "log"
         "logistic"
         "logi" "logit"
         "loneway" "lookfor"
         "lo" "loo" "look" "looku" "lookup"
         "lpredict" "lroc" "lrtest" "ls" "lsens" "lstat" "ltable" "lv" "lvr2plot"
         "man" "matcproc" "mcc" "mcci"
         "means"
         "mlog" "mlogi" "mlogit"
         "mor" "more"
         "mvreg" "mx_param"
         "n" "nbreg" "newey" "news"
         "nl" "nlinit"
         "no" "noi" "nois" "noisi" "noisil" "noisily"
         "note" "notes"
         "nptrend" "numlist"
         "olog" "ologi" "ologit"
         "ologitp"
         "on" "one" "onew" "onewa" "oneway"
         "oprob" "oprobi" "oprobit"
         "oprobitp"
         "orthog" "orthpoly"
         "ovtest")
        font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("pac" "pchart" "pchi" "pcorr" "pergram"
         "pl" "plo" "plot"
         "pnorm" "poisgof" "poisson" "pperron"
         "prais"
         "prob" "probi" "probit"
         "prtest" "prtesti"
         "pwcorr" "pwd"
         "q" "qchi" "qnorm" "qqplot" "qreg" "quadchk" "quantile"
         "qu" "que" "quer" "query"
         "qui" "quie" "quiet" "quietl" "quietly"
         "ranksum" "rchart" "regdw" "regph"
         "reg" "reg3" "regr" "regre" "regres" "regress" "reshape"
         "rot" "rota" "rotat" "rotate"
         "rreg"
         "run" "runtest" "rvfplot" "rvpplot"
         ) font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "sampsi" "sconfirm"
         "sco" "scobit" "scor" "score"
         "sdtest" "sdtesti" "search" "serrbar"
         "sfrancia" "shell" "shelltool" "shewhart" "signrank" "signtest"
         "sktest" "slog" "spearman" "spikeplt" "sqreg"
         "st" "st_is" "st_show" "st_ct"
         "stcox" "stcoxkm" "stcurv" "stdes"
         "stem"
         "stereg" "stir" "stmc" "stmh" "stphplot" "stphtest"
         "strate" "streg"
         "sts" "stse" "stset" "stsum" "stvary" "stweib"
         "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
         "sureg"
         "svydes" "svyintrg" "svyivreg" "svylc" "svylogit"
         "svymean" "svymean" "svymlog" "svyolog" "svyoprob" "svypois" "svyprobt"
         "svyprop" "svyratio" "svyreg" "svyset" "svytab" "svytest" "svytotal"
         "swilk" "symmetry" "symmi" "symplot" "syntax" "sysdir"
         ) font-lock-reference-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "ta" "tab"
         "tab1" "tab2"
         "tabdisp"
         "tabi"
         "table"
         "tabu" "tabul" "tabula" "tabulat" "tabulate"
         "te" "tes" "test"
         "testnl" "testparm" "tis"
         "tob" "tobi" "tobit"
         "token" "tokeni" "tokeniz" "tokenize"
         "touch" "tsreport" "tsset" "tsunab" "ttest" "ttesti"
         "ty" "typ" "type"
         "unab" "using"
         "vce"
         "verinst" "vif" "vwls"
         "weibull" "which" "who" "wntestb" "wntestq"
         "xchart" "xcorr"
         "xtclog" "xtdes" "xtgee" "xtgls" "xthaus" "xtintreg"
         "xtlogit" "xtnbreg" "xtpois" "xtprobit"
         "xtrchh" "xtreg" "xtsum" "xttab" "xttest0" "xttobit" "xttrans"
         "zip" "zinb"
         ) font-lock-reference-face)
      "\\b"
      ))

   ;; conditional statements
   ;; if might not work right ('cuz it is also a keyword)
   (eval-when-compile
     (make-regexps
      "^[ \t]*\\sw+[ \t]*"
      '(("if"
         ) font-lock-reference-face t t)
      "\\b"
      ))

   (eval-when-compile
     (make-regexps
      "^[ \t]*"
      '(("if" "while"
         ) font-lock-reference-face t t)
      "[ \t]+.*{"
      ))
   ;; else statement (which must just have a {)
   (eval-when-compile
     (make-regexps
      "^[ \t]*"
      '(("else"
         ) font-lock-reference-face)
      "[ \t]*{"
      ))

   ;; short version of list --- which can get fooled if used as a var
   (eval-when-compile
     (make-regexps
      '(("^[ \t]*l\\b"
         ) font-lock-reference-face)
      ))

   ;; all the Stata options
   ;; commonly used options
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("byte" "int" "long" "str[1-9]+[0-9]?" "float" "double"
         "width" "maxobs" "maxvar"
         ) font-lock-type-face)
      "[ \t]+"
      ))
   ;; special local variables (used in parsing)
   (eval-when-compile
     (make-regexps
      "^[ \t]+\\(local\\)+[ \t]+"
      '(("varlist" "exp" "weight" "if" "in" "using" "options"
         ) font-lock-type-face nil t t)
      "\\b"
      ))

   ;; things used with display
   ;; since these are often split across lines, and Stata commands are hard
   ;; to delimit, this will highlight even if out of context

   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "_c" "_co" "_con" "_cont" "_conti" "_contin" "_continu" "_continue"
         "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline"
         "_quote"
         "_r" "_re" "_req" "_requ" "_reque" "_reques" "_request"
         )
        font-lock-type-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '((
         "_col" "_colu" "_colum" "_column"
         "_d" "_du" "_dup"
         "_s" "_sk" "_ski" "_skip"
         )
        font-lock-type-face)
      "([1-9]+[0-9]*)\\b"
      ))
   (eval-when-compile
     (make-regexps
      "\\bin[ \t]+"
      '((
         "b" "bl" "blu" "blue"
         "g" "gr" "gre" "gree" "green"
         "r" "re" "red"
         "w" "wh" "whi" "whit" "white"
         "y" "ye" "yel" "yell" "yello" "yellow"
         ) font-lock-type-face)
      "\\b"
      ))

   ;; labels
   (eval-when-compile
     (make-regexps
      "[ \t]+"
      '(("lab" "labe" "label"
         ) font-lock-reference-face t)
      "[ \t]+"
      '((
         "da" "dat" "data"
         "de" "def" "defi" "defin" "define"
         "di" "dir"
         "drop"
         "l" "li" "lis" "list"
         "save"
         "val" "valu" "value" "values"
         "var" "vari" "varia" "variab" "variabl" "variable"
         ) font-lock-type-face nil t t)
      "[ \t]"
      ))

   ;; all Stata data-altering stuff
   (eval-when-compile
     (make-regexps
      "\\b"
      '((
         "_pctile" "_predict"
         "aorder" "append"
         "bcskew0" "bsample" "bs" "bstrap"
         "cd" "chdir" "clear" "compress"
         "contract" "convert" "cross"
         "dec" "deco" "decod" "decode"
         "discard" "drop" "dydx"
         "ed" "edi" "edit" "egen"
         "en" "enc" "enco" "encod" "encode"
         "erase"
         "expand"
         "fillin"
         "form" "forma" "format"
         "fracgen" "fracpred"
         "g" "ge" "gen" "gene" "gener" "genera" "generat" "generate"
         "gsort"
         "impute"
         "infile" "infix" "input" "insheet" "integ" "ipolate"
         "joinby"
         "keep"
         "lnskew0"
         ) font-lock-keyword-face)
      "\\b"
      ))
   (eval-when-compile
     (make-regexps
      "\\b"
      '((
         "mark" "markout" "marksample"
         "matname"
         "mer" "merg" "merge"
         "mkdir" "mkmat" "mkspline"
         "mleval" "mlmatsum" "mlsum""mlvecsum"
         "modify" "mov" "move"
         "mvdecode" "mvencode" "nlpred" "nobreak" "order"
         "ou" "out" "outf" "outfi" "outfil" "outfile"
         "outs" "outsh" "outshe" "outshee" "outsheet"
         "pctile"
         "post" "postclose" "postfile"
         "pre" "pred" "predi" "predic" "predict"
         "preserve" "range"
         "recast" "recode"
         "ren" "rena" "renam" "rename"
         "renpfix" "replace" "restore" "rm"
         "sappend"
         "sa" "sav" "save"
         "sample" "sdrop"
         "separate"
         "simul" "sinfile" "smerge"
         "smooth" "snapspan"
         "so" "sor" "sort"
         "ssave" "ssort" "stack"
         "stbase" "stfill" "stgen" "stjoin" "stsplit" "sttocc" "sttoct"
         "suse" "svmat"
         "tsfill" "tsrevar"
         "u" "us" "use"
         "xi" "xi:" "xtile" "xpose"
         "xtdata" "xtpred"
         ) font-lock-keyword-face)
      "\\b"
      ))

   ;; assignment of macros
   (eval-when-compile
     (make-regexps
      "^[ \t]*"
      '(("global" "local" "scalar"
         ) font-lock-reference-face)
      '(("\\([ \t]+[a-zA-Z_]+[a-zA-Z_0-9]*\\b\\)?"
         ) font-lock-variable-name-face t)
      ))
   ;; choosing temp names
   (eval-when-compile
     (make-regexps
      "^[ \t]*"
      '(("tempname" "tempfile" "tempvar"
         ) font-lock-reference-face)
      '(("\\([ \t]+[a-zA-Z_]+[a-zA-Z_0-9`']*\\)+"
         ) font-lock-type-face t)
      ))
   ;; all variable/macro stuff (put late so it will override)
   ;; internal constants
   (eval-when-compile
     (make-regexps
      "[^a-zA-Z]"
      '(("_merge" "_n" "_pi" "_rc" "_N"
         ) font-lock-variable-name-face)
      "[^a-zA-Z]"
      ))
   ;; some generated vars
   (eval-when-compile
     (make-regexps
      '(("_result([1-9]+)"
         ) font-lock-variable-name-face)
      ))
   ;; global macros
   (eval-when-compile
     (make-regexps
      '(("\\$[a-zA-Z_*]+[a-zA-Z_0-9]*"
         ) font-lock-variable-name-face t)
      ))
   ;; local macros
   (eval-when-compile
     (make-regexps
      "`+"
      '(("[a-zA-Z_`*]+[a-zA-Z_0-9]*"	;has glitch interior ` is highlighted
         ) font-lock-variable-name-face t)
      "'+"
      ))
   ;; other macro commands
   (eval-when-compile
     (make-regexps
      "[ \t]*"
      '((
         "ma" "mac" "macr" "macro"
         ) font-lock-reference-face)
      "[ \t]+"
      '((
         "de" "def" "define"
         "di" "dir"
         "drop"
         "l" "li" "lis" "list"
         "s" "sh" "shi" "shif" "shift"
         )
        font-lock-type-face)
      "[ \t]+"
      ))
   ;; stata 'functions' i.e. things which require () after them
   (eval-when-compile
     (make-regexps
      "\\b"
      '(("_caller"
         "abs" "acos" "asin" "atan" "autocode"
         "Binomial"
         "binorm"
         "chiprob" "comb" "cond" "cos"
         "d" "date" "digamma" "day"
         "dofh" "dofm" "dofq" "dofw" "dofy" "dow" "doy"
         "e" "exp"
         "float" "fprob" "gammap" "get" "group"
         "h" "halfyear" "halfyearly" "hofd"
         "ibeta" "index" "int"
         "invbinomial" "invchi" "invfprob" "invgammap" "invnchi" "invnorm" "invt"
         "length" "ln" "lnfact" "lngamma" "log" "log10" "lower" "ltrim"
         "m" "matrix" "max" "mdy" "min" "missing" "mod" "mofd" "month" "monthly"
         "nchi" "normd" "normprob" "npnchi"
         "q" "qofd" "quarter" "quarterly"
         "r" "real" "recode" "reldif" "replay" "return" "round" "rtrim"
         "s" "scalar" "sign" "sin" "sqrt" "string" "substr" "sum"
         "tan" "tprob" "trigamma" "trim"
         "uniform" "uniform0" "upper"
         "w" "week" "weekly" "wofd"
         "y" "year" "yearly" "yh" "ym" "yofd" "yq" "yw"
         )
        font-lock-reference-face t)
      "("
      ))
   ;; stata 'functions' i.e. things which require [] after them
   (eval-when-compile
     (make-regexps
      "\\b"
      '(("_b" "_coef" "_se")
        font-lock-reference-face t)
      "\\["
      ))
   ;; common Stata options which require a () after them
   (eval-when-compile
     (make-regexps
      "[, \t]+"
      '(("bands" "by" "connect" "density" "gap" "iterate" "ltolerance" "margin"
         "psize" "saving" "tlabel" "tolerance"
         "xlabel" "xscale" "ylabel" "yscale")
        font-lock-type-face t)
      "("
      ))
   ;; egen 'function' options
   (eval-when-compile
     (make-regexps
      "[ \t]*egen[ \t]+.*=[ \t]*"
      '(("count" "diff" "fill" "group" "iqr"
         "ma" "max" "mean" "median" "min" "mtr" "pctile"
         "rank" "rfirst" "rlast" "rmax" "rmean" "rmin" "rmiss" "robs" "rsd" "rsum"
         "sd" "std" "sum")
        font-lock-reference-face t)
      "(.*)"
      ))
   ;; All Custom ado files which are 'reliable' and which are not file killers
   ;; this might be a useless endeavor --- but I cannot generate tag files
   ;; all the s-extensions are listed under Stata's name (since they alter
   ;; data and will be moved tot he utils directory
   (eval-when-compile
     (make-regexps
      "[ \t]*"
      '(("addnote" "anypath" "autolab" "checkvar" "ck1icd9" "ckicd9"
         "datetoe" "dattomdy" "den2dem" "dishis" "dtapath" "dupclean" "echo"
         "exdupbil" "ezip2hsa" "getdate" "getlbl" "getnames" "getobs" "gplur"
         "icd9" "issorted" "isfile" "jultoe" "jultof" "jultomdy" "knowndup"
         "labeldir" "linker"
         "markit" "makewide" "missize" "mpcounts"
         "nodups" "notefile" "prov2zip"
         "qcolsum" "qorder"
         "random" "readraw" "readzip" "repart"
         "setup" "stdrate"
         "timeslot"
         "wdatetoe" "wdatomdy" "zip2ezip"
         "_addext" "_brclean" "_brckado" "_brdlog"
         "_ckbad" "_ckdunno" "_ckdupl" "_ckmiss" "_ckok" "_ckwarn"
         "_delimit" "_filenm" "_lookup" "_mk_ck"
         ) font-lock-function-name-face)
      "\\b"
      ))
   ))


(defvar ess-STA-mode-font-lock-keywords (ado-set-font-lock-keywords)
  "Set the Stata mode font-lock keywords to Bill Rising's ado-mode keywords.")

(defvar STA-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline        . t)
    (comment-start                . "/\* ")
    (comment-end                  . " \*/")
    (comment-start-skip           . "/\\*+ *")
    (comment-column               . 40)
    ;;(comment-indent-function      . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-indent-line              . 'S-indent-line)
    ;;(ess-calculate-indent         . 'S-calculate-indent)
    (indent-line-function         . 'S-indent-line)
    (parse-sexp-ignore-comments   . t)
    (ess-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . STA-syntax-table)
    (font-lock-defaults           . '(ess-STA-mode-font-lock-keywords
				      nil nil ((?\. . "w")))))
  "General options for editing Stata do and ado source files.")

;; YOU USED TO HAVE TO (with Thomas's version):
;;;;; Add the following to your .emacs file
;;
;;(autoload 'stata "~/ess-sta-l.el" "inferior stata mode" t )
;;(autoload 'stata-help "stata" "stata help mode" t)
;;(autoload 'stata-mode "~/ess-sta-l.el" "stata mode" t)
;;
;;(if (assoc "\\.do$" auto-mode-alist) nil
;;  (setq auto-mode-alist
;;	(append
;;	 '(("\\.do$" . stata-mode)
;;	   ("\\.ado$" . stata-mode))
;;	 auto-mode-alist)))
;;


;; QUESTIONS TO ASK THOMAS:
;; 1 - are 'help' and 'lookup' the same?
;; 2 - what is the point of the review buffer?
;; 3 - how to quit?

;;
;; NOTE: all of Thomas's functions have been left here, to be removed
;; or merged into real locations as we work on this.
;;


;;;;;;;;; Things to change

(defvar stata-switches "-q"
  "*Switches to apply to stata invocation.")

(defvar stata-profile "~/.stataprofile"
  "File to read on startup (nil for no file).")

;;;;;;;;;;;;;;;

;;(require 'comint)

(defun stata-help (the-subject)
  "Stata help in other buffer."
  (interactive "sHelp on: ")
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (process-buffer stata-process))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
      (save-excursion
	(set-process-buffer stata-process (get-buffer-create "*stata help*"))
	(set-buffer "*stata help*")
	(setq buffer-read-only nil)
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(erase-buffer)
	(process-send-string stata-process "help ")
	(process-send-string stata-process the-subject)
	(process-send-string stata-process "\n")
	(stata-prompt-wait stata-process)
	;;(stata-help-mode)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-process-filter stata-process oldpf)
	(set-marker (process-mark stata-process) oldpm)))
    (display-buffer "*stata help*")))

(defun stata-lookup (the-subject) "Stata lookup in other buffer"
  (interactive "sLook up: ")
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (process-buffer stata-process))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
      (save-excursion
	(set-process-buffer stata-process (get-buffer-create "*stata help*"))
	(set-buffer "*stata help*")
	(setq buffer-read-only nil)
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(erase-buffer)
	(process-send-string stata-process "lookup ")
	(process-send-string stata-process the-subject)
	(process-send-string stata-process "\n")
	(stata-prompt-wait stata-process)
	(stata-help-mode)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-process-filter stata-process oldpf)
	(set-marker (process-mark stata-process) oldpm)))
    (display-buffer "*stata help*")))

(defun stata-variables ()
  "Stata variable list in other buffer."
  (interactive)
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (if stata-process
			   (process-buffer stata-process)
			 (error "Stata is not running.")))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
       (save-excursion
	(set-process-buffer stata-process
			    (get-buffer-create "*stata variables*"))
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(set-buffer "*stata variables*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(process-send-string stata-process "desc \n ")
	(stata-prompt-wait stata-process)
	(setq buffer-read-only t)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-marker (process-mark stata-process) oldpm)
	(set-process-filter stata-process oldpf)))
    (display-buffer "*stata variables*")
    (goto-char (point-max))))

(defun stata-review-window ()
  (interactive)
  (display-buffer "*stata review*"))

(defun stata-rehelp ()
  (interactive)
  (stata-help (current-word)))

;;;; <IGNORE>
;;; This doesn't do anything at the moment.  I have vague plans of
;;; implementing a menu interface using emacs
;;;
(defun stata-watch-for-menu-filter (proc string)
  (if (string-match "^!!!window!!!" string)
      (stata-handle-menu-code proc string)
    (comint-output-filter proc string)))

(defun stata-handle-menu-code (proc string)
   (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point)
			  (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert "Handling menu code\n")
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

;;;; </IGNORE>

(defun stata-add-to-review-buffer (string)
  "Adds input to review buffer."
  (save-excursion
    (set-buffer (get-buffer-create "*stata review*"))
    (goto-char (point-max))
    (insert string)))

(defun stata-prompt-wait (proc &optional start-of-output)
  "Wait for a prompt to appear at BOL of current buffer.
PROC is the stata process. Does not change point."
  (if start-of-output nil (setq start-of-output (point-min)))
  (save-excursion
    (while (progn
	     ;; get output if there is some ready
	     (accept-process-output proc 0 50)
	     (goto-char (marker-position (process-mark proc)))
	     (beginning-of-line)
	     (if (< (point) start-of-output) (goto-char start-of-output))
	     (not (looking-at "^. "))))))

;;(defvar inferior-stata-mode-map nil
;;  "Keymap for Stata mode")

;;(setq inferior-stata-mode-map (cons 'keymap comint-mode-map))
;;(define-key inferior-stata-mode-map "\M-\t" 'comint-replace-by-expanded-filename)
;;(define-key inferior-stata-mode-map "\C-c\C-v" 'stata-variables)
;;(define-key inferior-stata-mode-map "\C-c\C-h" 'stata-help)
;;(define-key inferior-stata-mode-map "\C-c\C-u" 'stata-lookup)
;;(define-key inferior-stata-mode-map "\C-c\C-r"   'stata-review-window)
;;(define-key inferior-stata-mode-map [menu-bar stata]
;;  (cons "Stata" (make-sparse-keymap "Stata")))
;;(define-key inferior-stata-mode-map [menu-bar stata statahelp]
;;  '("Help on..." . stata-help))
;;(define-key inferior-stata-mode-map [menu-bar stata lookup]
;;  '("Look up..." . stata-lookup))
;;(define-key inferior-stata-mode-map [menu-bar stata variables]
;;  '("Variables" . stata-variables))
;;(define-key inferior-stata-mode-map [menu-bar stata review]
;;  '("Review" . stata-review-window))


;;(defvar stata-mode-map nil
;;  "Keymap for Stata mode")

;;(setq stata-mode-map (make-sparse-keymap))
;;(define-key stata-mode-map "\C-c\C-r"    'stata-eval-region)
;;(define-key stata-mode-map "\C-c\M-r" 'stata-eval-region-and-go)
;;(define-key stata-mode-map "\C-c\C-b"    'stata-eval-buffer)
;;(define-key stata-mode-map "\C-c\M-b" 'stata-eval-buffer-and-go)
;;(define-key stata-mode-map "\C-c\C-f"    'stata-eval-function)
;;(define-key stata-mode-map "\C-c\C-n"     'stata-eval-line-and-next-line)
;;(define-key stata-mode-map "\C-c\C-j"    'stata-eval-line)
;;(define-key stata-mode-map "\C-c\C-r"   'stata-review-window)
;;(define-key stata-mode-map "\C-c\M-j" 'stata-eval-line-and-go)
;;(define-key stata-mode-map "\C-c\C-y"    'stata-switch-to-stata)
;;(define-key stata-mode-map "\C-c\C-z" 'stata-switch-to-end-of-stata)
;;;;(define-key stata-mode-map "\C-c\C-l"    'stata-load-file)
;;(define-key stata-mode-map "\C-c\C-h"    'stata-help)
;;(define-key stata-mode-map "\C-c\C-v"    'stata-variables)
;;(define-key stata-mode-map "\M-\t" 'comint-replace-by-expanded-filename)
;;(define-key stata-mode-map "\177" 'backward-delete-char-untabify)
;;(define-key stata-mode-map "\C-c\C-u" 'stata-lookup)
;;(define-key stata-mode-map [menu-bar stata]
;;  (cons "Stata" (make-sparse-keymap "Stata")))
;;(define-key stata-mode-map [menu-bar stata lookup]
;;  '("Look up..." . stata-lookup))
;;(define-key stata-mode-map [menu-bar stata statahelp]
;;  '("Help on..." . stata-help))
;;(define-key stata-mode-map [menu-bar stata variables]
;;  '("Variables" . stata-variables))
;;(define-key stata-mode-map [menu-bar stata review]
;;  '("Review" . stata-review-window))
;;(define-key stata-mode-map [menu-bar stata eval-line]
;;  '("Eval line" . stata-eval-line))
;;(define-key stata-mode-map [menu-bar stata eval-next]
;;  '("Eval line and next line" . stata-eval-line-and-next-line))
;;(define-key stata-mode-map [menu-bar stata eval-go]
;;  '("Eval line and go" . stata-eval-line-and-go))
;;(define-key stata-mode-map [menu-bar stata eval-buff]
;;  '("Eval buffer" . stata-eval-buffer))
;;(define-key stata-mode-map [menu-bar stata eval-buff-go]
;;  '("Eval buffer and go" . stata-eval-buffer-and-go))
;;(define-key stata-mode-map [menu-bar stata to-stata]
;;  '("Switch to stata" . stata-switch-to-stata))
;;
;;
;;

;(defvar stata-help-mode-map nil)
;(setq stata-help-mode-map (cons 'keymap help-mode-map))
;(define-key stata-help-mode-map [mouse-2] 'stata-rehelp)
;(define-key stata-help-mode-map "\C-c\C-r" 'stata-rehelp)
;(define-key stata-help-mode-map "\C-c\C-h" 'stata-help)
;(define-key stata-help-mode-map [menu-bar stata]
;  (cons "Stata" (make-sparse-keymap "Stata")))
;(define-key stata-help-mode-map [menu-bar stata statahelp]
;  '("Help on..." . stata-help))
;(define-key stata-help-mode-map [menu-bar stata rehelp]
;  '("rehelp (hyperlink)" . stata-rehelp))
;;


;;(defun inferior-stata-mode ()
;;"Major mode for running Stata. Based on comint-mode.
;;Features include Help (\\[stata-help]), Review (\\[stata-review-window]) and
;;Variables (\\[stata-variables]) mimicking the help, review and
;;variables windows of Stata for Windows
;;\\{inferior-stata-mode-map}"
;;  (interactive)
;;  (make-comint "stata" "stata"
;;	       (and stata-profile
;;		    (or (file-exists-p stata-profile)
;;			(null (message "Startup file %s not found."
;;				       stata-profile))) stata-profile)
;;	       stata-switches)
;;  (switch-to-buffer "*stata*" )
;;  (setq comint-process-echoes t)
;;  (set-process-filter (get-process "stata") 'stata-watch-for-menu-filter)
;;  (setq comint-input-filter-functions
;;	(cons 'stata-add-to-review-buffer comint-input-filter-functions))
;;  (save-excursion
;;    (set-buffer (get-buffer-create "*stata review*"))
;;    (stata-mode))
;;  (setq major-mode 'inferior-stata-mode)
;;  (setq mode-name "inferior Stata")
;;  (use-local-map inferior-stata-mode-map))
;;
;;(defun stata ()
;;  (interactive)
;;  (inferior-stata-mode))
;;

(defun stata-help-mode ()
  "Major mode for displaying Stata help in a read-only buffer.
Active commands are Help (\\[stata-help]) and hyperlink
(\\[stata-rehelp] or mouse-2)."
  (interactive)
  (setq major-mode 'stata-help-mode)
  (setq mode-name "Stata help")
  ;;(use-local-map stata-help-mode-map)
  (setq buffer-read-only t))

;;
;;
;;(defun stata-mode ()
;;"Major mode for editing Stata files. Commands for sending lines to
;;Stata (\\[stata-eval-line], \\[stata-eval-line-and-go],
;;\\[stata-eval-line-and-next-line])
;;and for displaying Stata help (\\[stata-help]), variables (\\[stata-variables])
;; and review window (\\[stata-review-window])
;;\\{stata-mode-map}"
;;  (interactive)
;;  (kill-all-local-variables)
;;  (setq major-mode 'stata-mode)
;;  (setq mode-name "Stata")
;;  (use-local-map stata-mode-map))
;;
;;
;;(defun stata-eval-region (start end)
;;  "Send the current region to the inferior stata process."
;;  (interactive "r")
;;  (process-send-region "stata" start end)
;;  (process-send-string "stata" "\n"))



;;(defun stata-eval-buffer ()
;;  "Send the current buffer to the inferior stata process."
;;  (interactive)
;;  (stata-eval-region (point-min) (point-max)))

;;(defun stata-eval-line ()
;;  "Send the current line to the inferior stata process."
;;  (interactive)
;;  (save-excursion
;;    (end-of-line)
;;    (let ((end (point)))
;;      (beginning-of-line)
;;      (stata-eval-region (point) end))))

;;(defun stata-eval-line-and-next-line ()
;;  "Evaluate the current line  and move to the next line."
;;  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
;;  (interactive)
;;  (display-buffer (process-buffer (get-process "stata")))
;;  (save-excursion
;;    (end-of-line)
;;    (let ((end (point)))
;;      (beginning-of-line)
;;      ;; RDB modified to go to end of S buffer so user can see result
;;      ;;(stata-eval-visibly (buffer-substring (point) end) nil t)))
;;      (stata-eval-region (point) end)))
;;  (next-line 1))


;;(defun stata-eval-region-and-go (start end )
;;  "Send the current region to the inferior S and switch to the process buffer."
;;  (interactive "r\nP")
;;  (stata-eval-region start end)
;;  (stata-switch-to-stata t))

;;(defun stata-eval-buffer-and-go ()
;;  "Send the current buffer to the inferior stata and switch to the process buffer."
;;  (interactive)
;;  (stata-eval-buffer)
;;  (stata-switch-to-stata t))


;;(defun stata-eval-line-and-go ()
;;  "Send the current line to the inferior stata process and switch to the
;;process buffer."
;;  (interactive)
;;  (stata-eval-line)
;;  (stata-switch-to-stata t))


;;(defun stata-switch-to-stata (eob-p)
;;  "Switch to the current inferior stata process buffer.
;;With argument, positions cursor at end of buffer."
;;  (interactive "P")
;;  (let (stata-process (get-process "stata"))
;;    (if stata-process
;;	(progn
;;	  (switch-to-buffer (process-buffer stata-process))
;;	  (if eob-p (goto-char (point-max))))
;;      (progn
;;	(message "No inferior stata process")
;;	(ding)))))

;;(defun stata-switch-to-end-of-stata nil
;;  "Switch to the end of the inferior stata process buffer."
;;  (interactive)
;;  (stata-switch-to-stata t))

;;; Suggested function from Brendan Halpin:
(defvar ess-STA-delimit-do-file "delimit-do.do")

(defun ess-STA-delimit-do ()
  (save-excursion
    (let ((commands (buffer-substring-no-properties (region-beginning)
                                                    (region-end))))
      (set-buffer (get-buffer-create ess-STA-delimit-do-file))
      (delete-region (point-min) (point-max))
      (insert "#delimit ;\n"
              commands
              "\n#delimit cr\n")
      (write-file ess-STA-delimit-do-file nil)
      (comint-send-string "Stata"
			  (format "do %s \n" ess-STA-delimit-do-file)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ess-sta-l)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-sta-l.el ends here



