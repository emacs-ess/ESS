;;; travis-get-dependencies.el --- Install ESS dependencies for Travis  -*- lexical-binding: t; -*-

;;; Commentary:
;; Install ESS dependencies for travis

(require 'package)
;;; Code:

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

;; Get julia-mode and install
(package-refresh-contents)
(package-install 'julia-mode)

;;; travis-get-dependencies.el ends here
