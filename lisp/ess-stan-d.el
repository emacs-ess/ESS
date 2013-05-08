;;; ess-stan-d.el --- ESS[STAN] dialect

;; Copyright (C) 2008-2013 ESS Core Team

;; Author: Ahmadou H. Dicko (based on ess-bugs-d.el)
;; Created: 10 March 2013
;; Maintainer: ESS-help <ess-help@r-project.org>

;; This file is part of ESS

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

;;; Code:

(require 'ess-bugs-l)
(require 'ess-utils)
(require 'ess-inf)

(setq auto-mode-alist
      (append '(("\\.[sS][tT][aA][nN]\\'" . ess-stan-mode)) auto-mode-alist))

(defvar ess-stan-command "stan" "Default STAN program in PATH.")
(make-local-variable 'ess-stan-command)

(defvar ess-stan-monitor '("") "Default list of variables to monitor.")
(make-local-variable 'ess-stan-monitor)

(defvar ess-stan-thin 1 "Default thinning parameter.")
(make-local-variable 'ess-stan-thin)

(defvar ess-stan-chains 1 "Default number of chains.")
(make-local-variable 'ess-stan-chains)

(defvar ess-stan-burnin 10000 "Default burn-in.")
(make-local-variable 'ess-stan-burnin)

(defvar ess-stan-update 10000 "Default number of updates after burnin.")
(make-local-variable 'ess-stan-update)

(defvar ess-stan-system t "Default whether STAN recognizes the system command.")

(defvar stan-blocks
  '("data" "transformed data" 
    "parameters" "transformed parameters" 
    "model" "generated quantities")
  "Model blocks in Stan")
(defvar stan-types
  '("int" "real" "vector" "ordered" "positive_ordered" "simplex" "row_vector" 
    "matrix" "corr_matrix" "cov_matrix")
  "Types in Stan")
(defvar stan-type-bounds
  '("lower" "upper")
  "Types in Stan")
(defvar stan-distributions
  '("bernoulli" "bernoulli_logit" "beta_binomial" 
    "beta" "binomial" "categorical" "cauchy" "chi_square" "dirichlet"
    "double_exponential" "exponential" "gamma" "hypergeometric" 
    "inv_chi_square" "inv_gamma" 
    "inv_wishart" "lkj_corr_cholesky" "lkj_corr" "lkj_cov"
    "logistic" "lognormal" 
    "multi_normal_cholesky" "multi_normal" "multi_student_t"
    "multinomial" "neg_binomial" "normal" 
    "ordered_logistic"
    "pareto" "poisson" "poisson_log" "scaled_inv_chi_square" 
    "student_t" "uniform"
    "weibull" "wishart")
  "Distributions in Stan (see index)")
(defvar stan-cdfs
  '("bernoulli_cdf" "beta_binomial_cdf" "beta_cdf" "binomial_cdf" 
    "exponential_cdf" "inv_chi_square_cdf" "inv_gamma_cdf" "logistic_cdf" 
    "lognormal_cdf" "neg_binomial_cdf" "normal_cdf" "pareto_cdf" 
    "poisson_cdf" "scaled_inv_chi_square_cdf" "student_t_cdf")
  "CDFs in Stan (see index)")
(defvar stan-functions
  '("Phi" "Phi_approx" "abs" "acos" "acosh" "asin"
    "asinh" "atan" "atan2" "atanh" "binary_log_loss"
    "binomial_coefficient_log" "block" "cbrt" "ceil" "cholesky_decompose" 
    "col" "cols" "cos" "cosh" "crossprod" "cumulative_sum" "determinant" 
    "diag_matrix" "diag_post_multiply" "diag_pre_multiply" "diagonal" 
    "dims" "dot_product" "dot_self" "e" 
    "eigenvalues_sym" "eigenvectors_sym" "epsilon"
    "erf" "erfc" "exp" "exp2" "expm1" 
    "fabs" "fdim" "floor" "fma" "fmax" 
    "fmin" "fmod" "hypot" "if_else" 
    "int_step" "inv_cloglog" "inv_logit" "inverse" "lbeta" 
    "lgamma" "lmgamma" "log" "log10" "log1m" "log1m_inv_logit"
    "log1p" "log1p_exp" "log2" "log_determinant" "log_inv_logit"
    "log_sum_exp" "logit" 
    "max" "mdivide_left_tri_low" "mdivide_right_tri_low" "mean" 
    "min" "multiply_log" "multiply_lower_tri_self_transpose" 
    "negative_epsilon" "negative_infinity"
    "not_a_number" "pi" "positive_infinity" "pow" "prod" 
    "round" "row" "rows" "sd" "sin" "singular_values" 
    "sinh" "size" "softmax" "sqrt" "sqrt2" 
    "square" "step" "sum" "tan" "tanh" 
    "tcrossprod" "tgamma" "trace" "trunc" "variance")
  "Functions in Stan (excluding distributions and cdfs)")
(defvar stan-keywords
  '("for")
  "Keywords in Stan")

(defvar ess-stan-font-lock-keywords
 (list
     (cons "#.*\n"                        font-lock-comment-face)  
     ;; Stan model blocks. Look for start of line before and open brace after.
     (cons (concat "^[[:space:]]*\\(" (regexp-opt stan-blocks 'words) "\\)\\s-*{") 1 font-lock-keyword-face)
     ;; Stan types. Look for it to come after the start of a line or semicolon.
     (cons (concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words)) 2 font-lock-constant-face)
     ;; Stan type bounds. Look for them after either '<' or ','
     (cons (concat "\\(<\\|,\\)\\s-*" (regexp-opt stan-type-bounds 'words)) 2 font-lock-constant-face)
     ;; Stan variables. Look for it to come after the types, anything, then '>'
     (cons (concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words) "\\s-*\\(\\w*\\)") 3 font-lock-variable-name-face)
     (cons (concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words) "<.*>\\s-*\\(\\w*\\)") 3 font-lock-variable-name-face)
     ;; Stan functions.
     (cons (regexp-opt stan-functions 'words) . font-lock-function-name-face)
     ;; Stan distributions. Look for distribution after '~'
     (cons (concat "~[[:space:]]*" (regexp-opt stan-distributions 'words)) 1 font-lock-function-constant-face)
     ;; Stan distributions. Look for distribution_log after '<-'
     (cons (concat "<-\\s-*\\(\\<" (regexp-opt stan-distributions) "_log\\>\\)") 1 font-lock-constant-face)
     ;; Stan distributions. Look for cdfs after '<-'
     (cons (concat "<-[[:space:]]*" (regexp-opt stan-cdfs 'words)) 1 font-lock-constant-face)
     ;; Stan keywords.
     (cons (regexp-opt stan-keywords 'words) . font-lock-keyword-face)
     )
  "Stan-mode font lock defaults"
  )



(defun ess-stan-mode ()
  "ESS[STAN]: Major mode for STAN."
  (interactive)
  (kill-all-local-variables)
  (ess-setq-vars-local '((comment-start . "#")))
  (setq major-mode 'ess-stan-mode)
  (setq mode-name "ESS[STAN]")
  (use-local-map ess-bugs-mode-map)
  (setq font-lock-auto-fontify t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ess-stan-font-lock-keywords nil t))
  (setq ess-language "S") ; mimic S for ess-smart-underscore
  (run-hooks 'ess-bugs-mode-hook)

  (if (not (w32-shell-dos-semantics))
      (add-hook 'comint-output-filter-functions 'ess-bugs-exit-notify-sh))
  )

(setq features (delete 'ess-bugs-d features))
(provide 'ess-stan-d)

;;; ess-stan-d.el ends here



;; ;; Stan mode for v1.2.0 of the language


;; (define-derived-mode 
;;   stan-mode   ; variant
;;   c++-mode    ; parent
;;   "stan-mode" ; name
;;   "Stan mode is a mode for editing Stan models" ; doc string
;;   ;; keyword-args
;;   ;; body
;;   ;; comments: '#', '//', or '/*' '*/' pair
;;   (modify-syntax-entry ?# "< b" stan-mode-syntax-table)
;;   (modify-syntax-entry ?_ "w" stan-mode-syntax-table)
;;   (modify-syntax-entry ?\' "_" stan-mode-syntax-table)
;;   (setq font-lock-defaults stan-font-lock-defaults)   ; stan fonts
;;   )


;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.stan\\'" . stan-mode))

;; (provide 'stan-mode)
