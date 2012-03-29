(TeX-add-style-hook "cph-1-lab"
 (function
  (lambda ()
    (LaTeX-add-labels
     "sec:emacs"
     "sec:commandline"
     "sec:cutpaste"
     "sec:cutpaste:eff1"
     "sec:cutpaste:eff2"
     "sec:edit"
     "sec:example"
     "start"
     "sec:using-sas"
     "sec:emacs:uwbiostat")
    (TeX-run-style-hooks
     "html"
     "palatino"
     "hyperref"
     "latex2e"
     "art10"
     "article"))))

