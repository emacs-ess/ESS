(TeX-add-style-hook "Sweave-Example"
 (function
  (lambda ()
    (LaTeX-add-labels
     "CHA:tables"
     "CHA:figures"
     "fig:control")
    (TeX-run-style-hooks
     "graphicx"
     "natbib"
     "authoryear"
     "round"
     "url"
     "hyperref"
     "dvips"
     "graphicx"
     "dvips"
     "hyperref"
     "pdftex"
     "graphicx"
     "pdftex"
     "latex2e"
     "rep12"
     "report"
     "12pt"))))

