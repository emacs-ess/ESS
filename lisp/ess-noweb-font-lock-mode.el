;;; noweb-font-lock-mode.el --- edit noweb files with GNU Emacs

;; Copyright (C) 1999 by  Adnan Yaqub (AYaqub@orga.com)
;;                    and Mark Lunt (mark.lunt@mrc-bsu.cam.ac.uk
;; Copyright (C) 2002 by A.J. Rossini <rossini@u.washington.edu>
;; Copyright (C) 2003--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;

;;; Commentary:

;; Code-dependent highlighting
;;  *****
;;
;;  Adding highlighting to noweb-mode.el
;;
;;  Here is a description of how one can add highlighting via the
;;  font-lock package to noweb buffers.  It uses the hooks provided by
;;  noweb-mode.el.  The solution provides the following features:
;;  1) The documentation chunks are highlighted in the noweb-doc-mode
;;  (e.g., LaTeX).
;;  2) The code chunks without mode comments (-*- mode -*-) are
;;  highlighted in the noweb-code-mode.
;;  3) The code chunks with mode comments (-*- mode -*-) on the first
;;  line of the first chunk with this name are highlighted in the mode
;;  in the comment.
;;
;;  For example, given the file:
;;
;;    % -*- mode: Noweb; noweb-code-mode: c-mode -*-
;;
;;    \begin{itemize}
;;    \item a main routine written in C,
;;    \item a log configuration file parser written in YACC, and
;;    \item a lexical analyzer written in Lex.
;;    \end{itemize}
;;
;;    <<warning c comment>>=
;;    /* DO NOT EDIT ME! */
;;    /* This file was automatically generated from %W% (%G%). */
;;    @
;;
;;    <<warning nroff comment>>=
;;    .\" -*- nroff -*-
;;    .\" DO NOT EDIT ME!
;;    .\" This file was automatically generated from %W% (%G%).
;;    @
;;
;;  The LaTeX list is highlighted in latex-mode (the default noweb doc
;;  mode), the chunk <<warning c comment>> is highlighted in c-mode (the
;;  default noweb code mode), and the chunk <<warning nroff comment>> is
;;  highlighted in nroff-mode due to the "-*- nroff -*-" comment.
;;
;;  Chunks are highlighted each time point moves into them from a
;;  different mode. They are also fontified 'on the fly', but this is
;;  less reliable, since the syntax can depend on the context. It's as
;;  good as you would get outside noweb-mode, though.
;;
;;  To use it, you must add
;;  (require 'noweb-font-lock-mode) to your .emacs file.
;;  Then, if you use either global-font-lock or turn-on-font-lock
;;  statements, any noweb-mode buffers will be fontified
;;  appropriately. (We have to redefine turn-on-font-lock, but it
;;  saves breaking other packages (in particular ESS, which I use a
;;  lot), that assume that turn-on-font-lock is the way to turn on
;;  font locking.

;;  Alternatively, you can turn noweb-font-lock-mode on and off by
;;  using M-x noweb-font-lock-mode. However, turning
;;  noweb-font-lock-mode off when global-font-lock-mode is t makes it
;;  impossible to use font-locking in that buffer subsequently, other
;;  than by turning noweb-font-lock-mode back on.

;;  2) The highlighting sometimes get confused, but this is no longer
;;  a noweb problem. Highlighting should work as well within a chunk
;;  as it does without noweb-mode.
;;  There are some problems with, for example latex-mode: a `$' in a
;;  verbatim environment with throw the font-locking out.
;;  One slight blemish is that code-quotes are highlighted as comments
;;  as they are being entered. They are only highlighted correctly
;;  after `noweb-font-lock-fontify-chunk' has been run, either as a
;;  command or through changing to a different chunk and back again
;;  (unless they lie on a single line, in which case they are
;;  fontified correctly once they are completed).

;;; Code:

(require 'noweb-mode)
(require 'font-lock)

(defvar noweb-font-lock-mode nil
  "Buffer local variable, t iff this buffer is using noweb-font-lock-mode.")

(defvar noweb-use-font-lock-mode t
  "DO NOT CHANGE THIS VARIABLE
If you use nw-turn-on-font-lock to turn on font-locking, then turn it
off again, it would come back on again of its own accord when you
changed major-mode. This variable is used internally to stop it.")

(defvar noweb-font-lock-mode-hook nil
  "Hook that is run after entering noweb-font-lock mode.")

(defvar noweb-font-lock-max-initial-chunks 2
  "Maximum number of chunks to fontify initially.
If nil, will fontify the entire buffer when
noweb-font-lock-initial-fontify-buffer is called" )

(defvar old-beginning-of-syntax nil
  "Stores the function used to find the beginning of syntax in the
current major mode. noweb-font-lock-mode needs a different one." )

;; (AJR) the next two lines were originally font-lock-warning-face
;; methods; XEmacs 20.4 doesn't define this, sigh...  -- KLUDGE --.

(defvar noweb-font-lock-doc-start-face font-lock-reference-face
  "Face to use to highlight the `@' at the start of each doc chunk")

(defvar noweb-font-lock-brackets-face font-lock-reference-face
  "Face to use to highlight `<<', `>>' `[[' and `]]' ")

(defvar noweb-font-lock-chunk-name-face font-lock-keyword-face
  "Face to use to highlight the between `<<' and `>>'")

(defvar noweb-font-lock-code-quote-face font-lock-keyword-face
  "Face to use to highlight the between `[[' and `]]'")

;; Now we add [[noweb-font-lock-mode]] to the list of existing minor
;; modes. The string ``NWFL'' will be added to the mode-line: ugly, but
;; brief.

(if (not (assq 'noweb-font-lock-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(noweb-font-lock-mode " NWFL")))))

;; An ugly kludge to get around problems with global-font-lock, which
;; fontifies the entire buffer in the new major mode every time you
;; change mode, which is time-consuming and makes a pigs trotters of
;; it. Trying to stop it looks tricky, but using this function as your
;; `font-lock-fontify-buffer' function stops it wasting your time

(defun nwfl-donowt()
  "This function does nothing at all")

;; The following function is just a wrapper for noweb-font-lock-mode,
;; enabling it to be called as noweb-font-lock-minor-mode instead.

(defun noweb-font-lock-minor-mode ( &optional arg)
  "Minor meta mode for managing syntax highlighting in noweb files.
See NOWEB-FONT-LOCK-MODE."
  (interactive)
  (noweb-font-lock-mode arg))

;; Here we get to the meat of the problem

(defun noweb-font-lock-mode ( &optional arg)
  "Minor mode for syntax highlighting when using noweb-mode to edit noweb files.
Each chunk is fontified in accordance with its own mode"
  (interactive "P")
  (if (or noweb-mode noweb-font-lock-mode)
      (progn
        ;; This bit is tricky: copied almost verbatim from bib-cite-mode.el
        ;; It seems to ensure that the variable noweb-font-lock-mode is made
        ;; local to this buffer. It then sets noweb-font-lock-mode to `t' if
        ;;     1) It was called with a prefix argument greater than 0
        ;; or  2) It was called with no argument, and noweb-font-lock-mode is
        ;;        currently nil
        ;; noweb-font-lock-mode is nil if the prefix argument was <= 0 or there
        ;; was no prefix argument and noweb-font-lock-mode is currently `t'
        (set (make-local-variable 'noweb-font-lock-mode)
             (if arg
                 (> (prefix-numeric-value arg) 0)
               (not noweb-font-lock-mode)))
        ;; Now, if noweb-font-lock-mode is true, we want to turn
        ;; noweb-font-lock-mode on
        (cond
          (noweb-font-lock-mode                 ;Setup the minor-mode
           (when (and (boundp 'global-font-lock-mode) global-font-lock-mode)
             (mapcar 'noweb-make-variable-permanent-local
                     '(font-lock-fontify-buffer-function
                       font-lock-unfontify-buffer-function))
             (setq font-lock-fontify-buffer-function 'nwfl-donowt)
             (setq font-lock-unfontify-buffer-function 'nwfl-donowt))
           (mapcar 'noweb-make-variable-permanent-local
                   '(noweb-font-lock-mode
                     font-lock-beginning-of-syntax-function
                     noweb-use-font-lock-mode
                     after-change-functions))
           (setq noweb-font-lock-mode t)
           (when (< emacs-major-version 21) ; needed for emacs < 21.1 only :
             (make-local-hook 'after-change-functions))
           (add-hook 'after-change-functions
             'font-lock-after-change-function nil t)
           (add-hook 'noweb-font-lock-mode-hook 'noweb-font-lock-mode-fn)
           (add-hook 'noweb-changed-chunk-hook
             'noweb-font-lock-fontify-this-chunk)
           (run-hooks 'noweb-font-lock-mode-hook)
           (message "noweb-font-lock mode: use `M-x noweb-font-lock-describe-mode' for more info"))
         ;; If we didn't do the above, then we want to turn noweb-font-lock-mode
         ;; off, no matter what (hence the condition `t')
          (t
           (when (and (boundp 'global-font-lock-mode) global-font-lock-mode)
             ;; (setq font-lock-fontify-buffer-function
             ;;       'font-lock-default-fontify-buffer)
             ;; Get back our unfontify buffer function
             (setq font-lock-unfontify-buffer-function
                   'font-lock-default-unfontify-buffer))
           (remove-hook 'noweb-font-lock-mode-hook 'noweb-font-lock-mode-fn)
           (remove-hook 'noweb-changed-chunk-hook
                        'noweb-font-lock-fontify-this-chunk)
           (remove-hook 'after-change-functions
                        'font-lock-after-change-function )
           (font-lock-default-unfontify-buffer)
           (setq noweb-use-font-lock-mode nil)
           (message "noweb-font-lock-mode removed"))))
    (message "noweb-font-lock-mode can only be used with noweb-mode")))

(defun noweb-start-of-syntax ()
  "Go to the place to start fontifying from"
  (interactive)
  (goto-char (car (noweb-chunk-region))))

(defun noweb-font-lock-fontify-chunk-by-number ( chunk-num )
  "Fontify chunk chunk-num based on the current major mode."
  (save-excursion
    (font-lock-set-defaults)
    (setq old-beginning-of-syntax font-lock-beginning-of-syntax-function)
    (setq font-lock-beginning-of-syntax-function 'noweb-start-of-syntax)
    (setq font-lock-keywords
          ;;         (append font-lock-keywords
          ;;                 '(("\\(\\[\\[\\)\\([^]]*\\]*\\)\\(\\]\\]\\|\\$\\)"
          ;;                    (1 noweb-font-lock-brackets-face prepend )
          ;;                    (2 noweb-font-lock-code-quote-face prepend)
          ;;                    (3 noweb-font-lock-brackets-face prepend))
          ;;                   ("^[ \t\n]*\\(<<\\)\\([^>]*\\)\\(>>=?\\)"
          ;;                    (1 noweb-font-lock-brackets-face  prepend )
          ;;                    (2 noweb-font-lock-chunk-name-face prepend)
          ;;                    (3 noweb-font-lock-brackets-face prepend))
          ;;                   ("^@[ \t\n]+"
          ;;                    (0 noweb-font-lock-doc-start-face prepend )))))
          (append font-lock-keywords
                  '(("\\(\\[\\[\\)\\([^]]*\\]*\\)\\(\\]\\]\\|\\$\\)"
                     (1 font-lock-reference-face prepend )
                     (2 font-lock-keyword-face prepend)
                     (3 font-lock-reference-face prepend))
                    ("^[ \t\n]*\\(<<\\)\\([^>]*\\)\\(>>=?\\)"
                     (1 font-lock-reference-face  prepend )
                     (2 font-lock-keyword-face prepend)
                     (3 font-lock-reference-face prepend))
                    ("^@[ \t\n]+"
                     (0 font-lock-reference-face prepend )))))


    (let ((r (cons (marker-position (cdr (aref noweb-chunk-vector
                                               chunk-num)))
                   (marker-position (cdr (aref noweb-chunk-vector
                                               (1+ chunk-num)))))))
      (font-lock-fontify-region (car r) (cdr r))
      t)))

(defun noweb-font-lock-fontify-this-chunk ()
  "Fontify this chunk according to its own major mode.
Since we are in the chunk, the major mode will already have been set
by noweb-mode.el"
  (interactive)
  (noweb-font-lock-fontify-chunk-by-number (noweb-find-chunk-index-buffer)))

(defun noweb-font-lock-initial-fontify-buffer ()
  "Applies syntax highlighting to some or all chunks in a noweb buffer.
The number of chunks is set by noweb-font-lock-max-initial-chunks: if
this is nil, the entire buffer is fontified.
It is intended to be called when first entering noweb-font-lock-mode.
For other purposes, use noweb-font-lock-fontify-chunks."
  (interactive)
  ;; This will be tricky. It will be very slow to go throught the chunks
  ;; in order, switching major modes all the time.
  ;; So, we will do the documentation in one pass, the code in a second
  ;; pass. This could still be a little slow if we have to swap between
  ;; different code modes regularly, but it should be bearable. It should
  ;; only happen when the file is first read in, anyway
  (save-excursion
    (let (start-chunk end-chunk this-chunk chunk-counter)
      (setq this-chunk (noweb-find-chunk-index-buffer))
      (if noweb-font-lock-max-initial-chunks
          (progn
            (setq start-chunk
                  (max 0
                       (- this-chunk
                          (/ noweb-font-lock-max-initial-chunks 2))))
            ;; Don't you just love hairy lisp syntax ? The above means set the
            ;; starting chunk to the current chunk minus half of
            ;; noweb-font-lock-max-initial-chunks, unless that is negative in
            ;; which case set it to 0
            (setq end-chunk (+ start-chunk noweb-font-lock-max-initial-chunks))
            (if (> end-chunk (- (length noweb-chunk-vector) 2))
                (setq end-chunk (- (length noweb-chunk-vector) 2))))
        ;; If noweb-font-lock-max-initial-chunks is nil, do the whole buffer
        (progn
          (setq start-chunk 0)
          (setq end-chunk (- (length noweb-chunk-vector) 2))))
      (noweb-font-lock-fontify-chunks start-chunk end-chunk))))

(defun noweb-font-lock-fontify-buffer ()
  "This function will fontify each chunk in the buffer appropriately."
  (interactive)
  (let ((start-chunk 0)
        (end-chunk (- (length noweb-chunk-vector) 2)))
    (noweb-font-lock-fontify-chunks start-chunk end-chunk)))

(defun noweb-font-lock-fontify-chunks (start-chunk end-chunk)
  "Fontify a noweb file from start-chunk to end-chunk"
  (interactive)
  (let (chunk-counter)
    (save-excursion
      (message "Fontifying from %d to %d" start-chunk end-chunk)
      ;; Want to set DOC mode for the first Doc chunk, not for the others
      (setq chunk-counter start-chunk)
      (while  (stringp (car (aref noweb-chunk-vector chunk-counter)))
        (setq chunk-counter (+ chunk-counter 1)))
      (goto-char (cdr (aref noweb-chunk-vector chunk-counter)))
      (noweb-select-mode)
      ;; Now go through the chunks, fontifying the documentation ones.
      (while (<= chunk-counter end-chunk)
        (if  (not (stringp (car (aref noweb-chunk-vector chunk-counter))))
            (noweb-font-lock-fontify-chunk-by-number chunk-counter))
        (message "Fontifying documentation chunks: chunk %d" chunk-counter)
        (setq chunk-counter (+ 1 chunk-counter)))
      ;; Go back to the start and go through the chunks, fontifying the code ones.
      (setq chunk-counter start-chunk)
      (message "About to do code chunks")
      (while (<= chunk-counter end-chunk)
        (when (stringp (car (aref noweb-chunk-vector chunk-counter)))
          ;; It's a code chunk: goto it to set the correct code mode, then
          ;; fontify it.
          (message "Fontifying code chunks: chunk %d" chunk-counter)
          (goto-char (cdr (aref noweb-chunk-vector chunk-counter)))
          (noweb-select-mode)
          (noweb-font-lock-fontify-this-chunk))
        (setq chunk-counter (1+ chunk-counter))))
    (noweb-select-mode)))

(defun noweb-font-lock-mode-fn()
  "Function that is intended to be attached to noweb-font-lock-mode-hook."
  (noweb-font-lock-initial-fontify-buffer))

;; This is a wee bit of a hack. If people attach `turn-on-font-lock'
;; to their major mode hook, it will play hell with
;; noweb-font-lock-mode. I had hoped that providing a replacement
;; `nw-turn-on-font-lock' would solve the problem, but it didn't
;; (sometimes turn-on-font-lock appears in places other than
;; `.emacs', such as in ESS). So rather than have it fall over if
;; turn-on-lock was around, I redefined turn-on-font-lock to do the
;; right thing.

(defvar noweb-old-turn-on-font-lock nil)

(defun nw-turn-on-font-lock ()
  "Turn on font-lock mode, with due regard to whether we are in noweb-mode"
  (if (not noweb-mode)
      (noweb-old-turn-on-font-lock)
    (if (and (not noweb-font-lock-mode) noweb-use-font-lock-mode)
        (noweb-font-lock-mode ))))

(unless (functionp 'noweb-old-turn-on-font-lock)
  (fset 'noweb-old-turn-on-font-lock (symbol-function 'turn-on-font-lock))
  (fset 'turn-on-font-lock (symbol-function 'nw-turn-on-font-lock)))

(provide 'noweb-font-lock-mode)
;;  *****
;;
;;  Adnan Yaqub (AYaqub@orga.com)
;;  ORGA Kartensysteme GmbH // An der Kapelle 2 // D-33104 Paderborn // Germany
;;  Tel. +49 5254 991-823 //Fax. +49 5254 991-749



;; Local Variables:
;; mode:emacs-lisp
;; End:

;;; noweb-font-lock-mode.el ends here
