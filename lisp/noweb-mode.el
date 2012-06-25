;;; noweb-mode.el --- edit noweb files with GNU Emacs

;; Copyright (C) 1995 by Thorsten.Ohl @ Physik.TH-Darmstadt.de
;;     with a little help from Norman Ramsey <norman@bellcore.com>
;;                         and Mark Lunt <mark.lunt@mrc-bsu.cam.ac.uk>
;;                         and A.J. Rossini <rossini@biostat.washington.edu>
;; Copyright (C) 1999--2010 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; ESS-related Changes first added by Mark Lunt and A.J. Rossini, March, 1999.

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
;; See bottom of this file for information on language-dependent
;; highlighting, and recent changes.
;;

;; BASED ON: (from Mark Lunt).
;; -- Id: noweb-mode.el,v 1.11 1999/03/21 20:14:41 root Exp --


;; Put this into your ~/.emacs to use this mode automagically.
;;
;; (autoload 'noweb-mode "noweb-mode" "Editing noweb files." t)
;; (setq auto-mode-alist (append (list (cons "\\.nw$" 'noweb-mode))
;;                            auto-mode-alist))

;;; NEWS:

;;   * [tho] M-n q, aka: M-x noweb-fill-chunk
;;
;;   * [tho] `M-n TAB', aka: `M-x noweb-complete-chunk'
;;
;;   * [tho] noweb-occur
;;
;;   * [nr] use `M-n' instead of `C-c n' as default command prefix
;;
;;   * [nr] don't be fooled by
;;
;;         @
;;         <<foo>>=
;;         int foo;
;;         @ %def foo
;;         Here starts a new documentation chunk!
;;         <<bar>>=
;;         int bar;
;;
;;  * [nr] switch mode changing commands off during isearch-mode
;;
;;  * [tho] noweb-goto-chunk proposes a default
;;
;;   * commands for tangling, weaving,.. for Sweave: --> ./ess-swv.el
;;


;;; TODO:

;;   * _maybe_ replace our `noweb-chunk-vector' by text properties.  We
;;     could then use highlighting to jazz up the visual appearance.
;;     (Highlighting is sorted: `noweb-chunk-vector' can be
;;     ditched. It is simple to determine if we are in a doc or code
;;     chunk.)
;;
;;   * wrapped `noweb-goto-next' and `noweb-goto-previous'
;;
;;   * more range checks and error exits
;;
;;   * `noweb-hide-code-quotes' should be superfluous now, and could
;;     be removed. For ESS 5.3.10, we disable these, using the new variable
;;     noweb-code-quote-handling.  If nobody misses that code-protecting
;;     behavior, all that should be removed entirely.

;;; Code:

;; Want to use these now in order to cater for all obscure kinds of emacsen
(eval-and-compile
  (require 'ess-compat))



;;; Variables

(defconst noweb-mode-RCS-Id
  "Imported to ESS Subversion repository and RCS ids not maintained.")

(defconst noweb-mode-RCS-Name
  " ")

(defvar noweb-mode-prefix "\M-n"
  "*Prefix key to use for noweb mode commands.
The value of this variable is checked as part of loading noweb mode.
After that, changing the prefix key requires manipulating keymaps.")

(defvar noweb-mode-load-hook nil
  "Hook that is run after noweb mode is loaded.")

(defvar noweb-mode-hook nil
  "Hook that is run after entering noweb mode.")

(defvar noweb-select-code-mode-hook nil
  "Hook that is run after the code mode is selected.
This is the place to overwrite keybindings of the NOWEB-CODE-MODE.")

(defvar noweb-select-doc-mode-hook nil
  "Hook that is run after the documentation mode is selected.
This is the place to overwrite keybindings of the NOWEB-DOC-MODE.")

(defvar noweb-select-mode-hook nil
  "Hook that is run after the documentation or the code mode is selected.
This is the place to overwrite keybindings of the other modes.")

(defvar noweb-changed-chunk-hook nil
  "Hook that is run every time point moves from one chunk to another.
It will be run whether or not the major-mode changes.")

(defvar noweb-default-code-mode 'fundamental-mode
  "Default major mode for editing code chunks.
This is set to FUNDAMENTAL-MODE by default, but you might want to
change this in the Local Variables section of your file to something
more appropriate, like C-MODE, FORTRAN-MODE, or even
INDENTED-TEXT-MODE.")

(defvar noweb-code-mode 'c-mode
  "Major mode for editing this particular code chunk.
It defaults to noweb-default-code-mode, but can be reset by a comment
on the first line of the chunk containing the string
\"-*- NEWMODE -*-\" or
\"-*- NEWMODE-mode -*-\" or
\"-*- mode: NEWMODE -*- \"  or
\"-*- mode: NEWMODE-mode -*- \"
Option three is recommended, as it is the closest to standard emacs usage.")

(defvar noweb-default-doc-mode 'latex-mode
  "Major mode for editing documentation chunks.
Sensible choices would be tex-mode, latex-mode, sgml-mode, or
html-mode.  Maybe others will exist someday.")

(defvar noweb-doc-mode-syntax-table nil
  "A syntax-table syntax table that makes quoted code in doc chunks to
behave.")

(defvar noweb-last-chunk-index 0
  "This keeps track of the chunk we have just been in. If this is not
the same as the current chunk, we have to check if we need to change
major mode.")

(defvar noweb-chunk-vector nil
  "Vector of the chunks in this buffer.")

(defvar noweb-narrowing nil
  "If not NIL, the display will always be narrowed to the
current chunk pair.")

(defvar noweb-electric-@-and-< t
  "If not nil, the keys `@' and `<' will be bound to NOWEB-ELECTRIC-@
and NOWEB-ELECTRIC-<, respectively.")

(defvar noweb-use-mouse-navigation t
  "If not nil, enables moving between chunks using mouse-1.
Clicking on the '<<' at the beginning of a chunk name takes you to the
previous occurence of that chunk name, clicking on the '>>' takes you
to the next.
Assumes mouse-1 is bound to mouse-set-point, so if you have rebound
mouse-1, this will override your binding.")

(defvar noweb-code-quotes-handling nil
  "If not nil, the function pair  \\[noweb-hide-code-quotes] and
\\[noweb-restore-code-quotes] are used to \"protect\" code inside
\"[[\" .. \"]]\" pairs.  Note that rarely this has been found to be buggy
with the \"catastrophic\" consequence of whole parts of your document being
replaced by sequences of '*'.")

;; The following is apparently broken -- dangling code that was
;; commented out.  Need to see if we can get it working?

(defvar noweb-weave-options "-delay")
(defvar noweb-latex-viewer "xdvi")
(defvar noweb-html-viewer "netscape")

(defun noweb-weave (&optional name)
  (interactive)
  (let ((buffer (get-buffer-create "Weave Buffer")))
    (if (not name)
        (progn
          ;; Assume latex documentation, but set to html if appropriate
          (if (eq noweb-doc-mode html-mode)
              (setq name (concat (substring (buffer-file-name) 0
                                            (string-match ".nw" name))
                                 ".html"))
            (setq name (concat (substring (buffer-file-name) 0
                                          (string-match ".nw" name))
                               ".tex")))))
    (setq name (concat "> " name))
    (setq noweb-weave-options (concat noweb-weave-options name))
    (start-process weave-process buffer "noweave" noweb-weave-options)))
;;(defun noweb-view  ())


;;; Setup
(defvar noweb-mode nil
  "Buffer local variable, T iff this buffer is edited in noweb mode.")

;; For some reason that I do not understand, `newline' does not do the
;; right thing in quoted code. If point is not preceded by whitespace,
;; it moves to the beginning of the current line, not the beginning of
;; the new line. `newline 1' works fine, hence the kludge. I'd love to
;; understand what's going on, though. Try running M-x newline in the
;; middle of a code quote in a doc chunk to see
;; what I mean: its odd.

(defun noweb-newline (&optional arg)
  "A kludge to get round very odd behaviour of newline in quoted code."
  (interactive "p")
  (if arg (newline arg) (newline 1))
  (noweb-indent-line))

(defvar noweb-mode-prefix-map
  (let ((map (if (featurep 'xemacs)
                 (make-keymap) ;; XEmacs/Emacs problems...
               (make-sparse-keymap))))
    (define-key map "\C-n" 'noweb-next-chunk)
    (define-key map "\C-p" 'noweb-previous-chunk)
    (define-key map "\M-n" 'noweb-goto-next)
    (define-key map "\M-m" 'noweb-insert-default-mode-line)
    (define-key map "\M-p" 'noweb-goto-previous)
    (define-key map "c" 'noweb-next-code-chunk)
    (define-key map "C" 'noweb-previous-code-chunk)
    (define-key map "d" 'noweb-next-doc-chunk)
    (define-key map "D" 'noweb-previous-doc-chunk)
    (define-key map "g" 'noweb-goto-chunk)
    (define-key map "\C-l" 'noweb-update-chunk-vector)
    (define-key map "\M-l" 'noweb-update-chunk-vector)
    (define-key map "w" 'noweb-copy-chunk-as-kill)
    (define-key map "W" 'noweb-copy-chunk-pair-as-kill)
    (define-key map "k" 'noweb-kill-chunk)
    (define-key map "K" 'noweb-kill-chunk-pair)
    (define-key map "m" 'noweb-mark-chunk)
    (define-key map "M" 'noweb-mark-chunk-pair)
    (define-key map "n" 'noweb-narrow-to-chunk)
    (define-key map "N" 'noweb-narrow-to-chunk-pair)
    (define-key map "t" 'noweb-toggle-narrowing)
    (define-key map "\t" 'noweb-complete-chunk)
    (define-key map "q" 'noweb-fill-chunk)
    (define-key map "i" 'noweb-new-chunk)
    (define-key map "o" 'noweb-occur)
    (define-key map "v" 'noweb-mode-version)
    (define-key map "h" 'noweb-describe-mode)
    ;; do *NOT* override C-h (give all keybindings startings with M-n!
    map)
  "noweb minor-mode prefix keymap")

(defvar noweb-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (if noweb-electric-@-and-<
        (progn
          (define-key map "@" 'noweb-electric-@)
          (define-key map "<" 'noweb-electric-<)))
    (define-key map "\M-q" 'noweb-fill-paragraph-chunk)
    ;;(define-key map "\C-c\C-n" 'noweb-indent-line) ; Override TeX-normal!
    (define-key map "\t" 'noweb-indent-line)
    ;; (define-key map [tab] 'noweb-indent-line) ;; interferes with ac
    (define-key map "\r" 'noweb-newline)
    ;; (define-key map [return] 'noweb-newline) ;; interferes with ac
    (define-key map [mouse-1] 'noweb-mouse-first-button)
    (define-key map noweb-mode-prefix noweb-mode-prefix-map)
    map)
  "Noweb minor mode keymap")

(easy-menu-define
  noweb-minor-mode-menu noweb-minor-mode-map
  "Menu keymap for noweb."
  '("Noweb"
    ("Movement"
     ["Previous chunk" noweb-previous-chunk t]
     ["Next chunk" noweb-next-chunk t]
     ["Previous chunk of same name" noweb-goto-previous t]
     ["Next chunk of same name" noweb-goto-next t]
     ["Goto chunk" noweb-goto-chunk t]
     ["Previous code chunk" noweb-previous-code-chunk t]
     ["Next code chunk" noweb-next-code-chunk t]
     ["Previous documentation chunk" noweb-previous-doc-chunk t]
     ["Next documentation chunk" noweb-next-doc-chunk t])
    ("Editing"
     ["Copy chunk" noweb-copy-chunk-as-kill t]
     ["Copy chunk pair" noweb-copy-chunk-pair-as-kill t]
     ["Kill chunk" noweb-kill-chunk t]
     ["Kill chunk pair" noweb-kill-chunk-pair t]
     ["Mark chunk" noweb-mark-chunk t]
     ["Mark chunk pair" noweb-mark-chunk-pair t])
    ("Narrowing"
     ["Narrow to chunk" noweb-narrow-to-chunk t]
     ["Narrow to chunk pair" noweb-narrow-to-chunk-pair t]
     ["Toggle auto narrowing" noweb-toggle-narrowing t]
     ["Widen" widen t])
    ("Modes"
     ["Set documentation mode" noweb-set-doc-mode t]
     ["Set default code mode" noweb-set-code-mode t]
     ["Set code mode for this chunk" noweb-set-this-code-mode t]
     ["Insert default mode line" noweb-insert-default-mode-line t])
    ("Tangling"
     ["Tangle current chunk" noweb-tangle-chunk t]
     ["Tangle current thread" noweb-tangle-current-thread t]
     ["Tangle named thread" noweb-tangle-thread t])
    ("Miscellaneous"
     ["Complete chunk name" noweb-complete-chunk t]
     ["Fill current chunk" noweb-fill-chunk t]
     ["Insert new chunk" noweb-new-chunk t]
     ["Update the chunk vector" noweb-update-chunk-vector t]
     ["Chunk occurrences" noweb-occur t])
    "--"
    ["Help" noweb-describe-mode t]
    ["Version" noweb-mode-version t]))

;; Add noweb-mode to the list of minor modes
(if (not (assq 'noweb-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(noweb-mode " Noweb")))))
;; Add noweb-minor-mode-map to the list of minor-mode keymaps
;; available. Then, whenever noweb-mode is activated, the keymap is
;; automatically activated
(if (not (assq 'noweb-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'noweb-mode noweb-minor-mode-map)
                minor-mode-map-alist)))

;; Old XEmacs hacks.
(defun noweb-mode-xemacs-menu ()
  "Hook to install noweb-mode menu for XEmacs (w/ easymenu)."
  (if 'noweb-mode
      (easy-menu-add noweb-minor-mode-menu)
    (easy-menu-remove noweb-minor-mode-menu)
    ))

(if (string-match "XEmacs" emacs-version)
    (progn
      (add-hook 'noweb-select-mode-hook 'noweb-mode-xemacs-menu)
      ;; Next line handles some random problems...
      (easy-menu-add noweb-minor-mode-menu)))

(defun noweb-minor-mode (&optional arg)
  "Minor meta mode for editing noweb files. See NOWEB-MODE."
  (interactive)
  (noweb-mode arg)) ; this was noweb-minor-mode???  (truly recursive)

(defun noweb-mode ( &optional arg )
  "Minor meta mode for editing noweb files.
`Meta' refers to the fact that this minor mode is switching major
modes depending on the location of point.

The following special keystrokes are available in noweb mode:

Movement:
\\[noweb-next-chunk] \tgoto the next chunk
\\[noweb-previous-chunk] \tgoto the previous chunk
\\[noweb-goto-previous] \tgoto the previous chunk of the same name
\\[noweb-goto-next] \tgoto the next chunk of the same name
\\[noweb-goto-chunk] \t\tgoto a chunk
\\[noweb-next-code-chunk] \t\tgoto the next code chunk
\\[noweb-previous-code-chunk] \t\tgoto the previous code chunk
\\[noweb-next-doc-chunk] \t\tgoto the next documentation chunk
\\[noweb-previous-doc-chunk] \t\tgoto the previous documentation chunk

Copying/Killing/Marking/Narrowing:
\\[noweb-copy-chunk-as-kill] \t\tcopy the chunk the point is in into the kill ring
\\[noweb-copy-chunk-pair-as-kill] \t\tcopy the pair of doc/code chunks the point is in
\\[noweb-kill-chunk] \t\tkill the chunk the point is in
\\[noweb-kill-chunk-pair] \t\tkill the pair of doc/code chunks the point is in
\\[noweb-mark-chunk] \t\tmark the chunk the point is in
\\[noweb-mark-chunk-pair] \t\tmark the pair of doc/code chunks the point is in
\\[noweb-narrow-to-chunk] \t\tnarrow to the chunk the point is in
\\[noweb-narrow-to-chunk-pair] \t\tnarrow to the pair of doc/code chunks the point is in
\\[widen] \twiden
\\[noweb-toggle-narrowing] \t\ttoggle auto narrowing

Filling and Indenting:
\\[noweb-fill-chunk] \tfill (or indent) the chunk at point according to mode
\\[noweb-fill-paragraph-chunk] \tfill the paragraph at point, restricted to chunk
\\[noweb-indent-line] \tindent the line at point according to mode

Insertion:
\\[noweb-insert-default-mode-line] \tinsert a line to set this file's code mode
\\[noweb-new-chunk] \t\tinsert a new chunk at point
\\[noweb-complete-chunk] \tcomplete the chunk name before point
\\[noweb-electric-@] \t\tinsert a `@' or start a new doc chunk
\\[noweb-electric-<] \t\tinsert a `<' or start a new code chunk

Modes:
\\[noweb-set-doc-mode] \t\tset the major mode for editing doc chunks
\\[noweb-set-code-mode] \tset the major mode for editing code chunks
\\[noweb-set-this-code-mode] \tset the major mode for editing this code chunk

Misc:
\\[noweb-occur] \t\tfind all occurrences of the current chunk
\\[noweb-update-chunk-vector] \tupdate the markers for chunks
\\[noweb-describe-mode] \tdescribe noweb-mode
\\[noweb-mode-version] \t\tshow noweb-mode's version in the minibuffer
"  (interactive "P")
;; This bit is tricky: copied almost verbatim from bib-cite-mode.el
;; It seems to ensure that the variable noweb-mode is made
;; local to this buffer. It then sets noweb-mode to `t' if
;;     1) It was called with an argument greater than 0
;; or  2) It was called with no argument, and noweb-mode is
;;        currently nil
;; noweb-mode is nil if the argument was <= 0 or there
;; was no argument and noweb-mode is currently `t'
(kill-all-local-variables)
(set (make-local-variable 'noweb-mode)
     (if arg
         (> (prefix-numeric-value arg) 0)
       (not noweb-mode)))
;; Now, if noweb-mode is true, we want to turn
;; noweb-mode on
(cond
 (noweb-mode                            ;Setup the minor-mode
  (mapcar 'noweb-make-variable-permanent-local
          '(noweb-mode
            after-change-functions
            before-change-functions
            noweb-narrowing
            noweb-chunk-vector
            post-command-hook
            isearch-mode-hook
            isearch-mode-end-hook
            noweb-doc-mode
            noweb-code-mode
            noweb-default-code-mode
            noweb-last-chunk-index))
  (noweb-update-chunk-vector)
  (if (equal 0 (noweb-find-chunk-index-buffer))
      (setq noweb-last-chunk-index 1)
    (setq noweb-last-chunk-index 0))
  (if font-lock-mode
      (progn
        (font-lock-mode -1)
        (require 'noweb-font-lock-mode); which requires noweb-mode .. hmm..
        (noweb-font-lock-mode 1)))
  (add-hook 'post-command-hook 'noweb-post-command-function)

  (when (or (<= emacs-major-version 20)
            (featurep 'xemacs)) ;; Xemacs or very old GNU Emacs
    (make-local-hook 'after-change-functions)
    (make-local-hook 'before-change-functions))
  (add-hook 'after-change-functions 'noweb-after-change-function nil t)
  (add-hook 'before-change-functions 'noweb-before-change-function nil t)

  (add-hook 'noweb-select-doc-mode-hook 'noweb-auto-fill-doc-mode)
  (add-hook 'noweb-select-code-mode-hook 'noweb-auto-fill-code-mode)
  (add-hook 'isearch-mode-hook 'noweb-note-isearch-mode)
  (add-hook 'isearch-mode-end-hook 'noweb-note-isearch-mode-end)
  (setq noweb-doc-mode-syntax-table nil)
  (run-hooks 'noweb-mode-hook)
  (message
   "noweb mode: use `M-x noweb-describe-mode' for further information"))
 ;; If we didn't do the above, then we want to turn noweb-mode
 ;; off, no matter what (hence the condition `t')
 (t
  (remove-hook 'post-command-hook 'noweb-post-command-function)

  (if (fboundp 'remove-local-hook)
      (progn
        (remove-local-hook 'after-change-functions 'noweb-after-change-function)
        (remove-local-hook 'before-change-functions 'noweb-before-change-function))
    (remove-hook 'after-change-functions 'noweb-after-change-function t)
    (remove-hook 'before-change-functions 'noweb-before-change-function t))

  (remove-hook 'noweb-select-doc-mode-hook 'noweb-auto-fill-doc-mode)
  (remove-hook 'noweb-select-code-mode-hook 'noweb-auto-fill-code-mode)
  (remove-hook 'isearch-mode-hook 'noweb-note-isearch-mode)
  (remove-hook 'isearch-mode-end-hook 'noweb-note-isearch-mode-end)
  (if (and (boundp 'noweb-font-lock-mode)
           noweb-font-lock-mode)
      (progn
        (noweb-font-lock-mode -1)
        (message "Noweb and Noweb-Font-Lock Modes Removed"))
    (message "Noweb mode removed")))))

(defun noweb-make-variable-permanent-local (var)
  "Declare VAR buffer local, but protect it from beeing killed
by major mode changes."
  (make-variable-buffer-local var)
  (put var 'permanent-local 't))

(defun noweb-note-isearch-mode ()
  "Take note of an incremental search in progress"
  (remove-hook 'post-command-hook 'noweb-post-command-function))

(defun noweb-note-isearch-mode-end ()
  "Take note of an incremental search having ended"
  (add-hook 'post-command-hook 'noweb-post-command-function))

(defun noweb-post-command-function ()
  "The hook being run after each command in noweb mode."
  (noweb-select-mode))

(defvar noweb-chunk-boundary-changed nil
  "Whether the current change affects a chunk boundary.")

(defvar noweb-chunk-boundary-regexp "^\\(@[^@]\\)\\|\\(<<\\)")

(defun noweb-before-change-function (begin end)
  "Record changes to chunk boundaries."
  (save-excursion
    (goto-char begin)
    (setq noweb-chunk-boundary-changed
          (re-search-forward noweb-chunk-boundary-regexp end t))))

(defun noweb-after-change-function (begin end length)
  "Function to run after every change in a noweb buffer.
If the changed region contains a chunk boundary, it will update
the chunk vector"
  (save-excursion
    (goto-char begin)
    (when (or noweb-chunk-boundary-changed
              (re-search-forward noweb-chunk-boundary-regexp end t))
      (noweb-update-chunk-vector)
      (setq noweb-chunk-boundary-changed nil))))


;;; Chunks

(defun noweb-update-chunk-vector ()
  "Scan the whole buffer and place a marker at each \"^@\" and \"^<<\".
Record them in NOWEB-CHUNK-VECTOR."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((chunk-list (list (cons 'doc (point-marker)))))
      (while (re-search-forward "^\\(@\\( \\|$\\|\\( %def\\)\\)\\|<<\\(.*\\)>>=\\)" nil t)
        (goto-char (match-beginning 0))
        ;; If the 3rd subexpression matched @ %def, we're still in a code
        ;; chunk (sort of), so don't place a marker here.
        (if (not (match-beginning 3))
            (setq chunk-list
                  ;; If the 4th subexpression matched inside <<...>>,
                  ;; we're seeing a new code chunk.
                  (cons (cons (if (match-beginning 4)
                                  ;;buffer-substring-no-properties better
                                  ;;than buffer-substring if highlighting
                                  ;;may be used
                                  (buffer-substring-no-properties
                                   (match-beginning 4) (match-end 4))
                                'doc)
                              (point-marker))
                        chunk-list))
          ;; Scan forward either to !/^@ %def/, which will start a docs chunk,
          ;; or to /^<<.*>>=$/, which will start a code chunk.
          (progn
            (forward-line 1)
            (while (looking-at "@ %def")
              (forward-line 1))
            (setq chunk-list
                  ;; Now we can tell code vs docs
                  (cons (cons (if (looking-at "<<\\(.*\\)>>=")
                                  (buffer-substring-no-properties
                                   (match-beginning 1) (match-end 1))
                                'doc)
                              (point-marker))
                        chunk-list))))
        (forward-line 1))
      (setq chunk-list (cons (cons 'doc (point-max-marker)) chunk-list))
      (setq noweb-chunk-vector (vconcat (reverse chunk-list))))))

(defun noweb-find-chunk ()
  "Return a pair consisting of the name (or 'DOC) and the
marker of the current chunk."
  (if (not noweb-chunk-vector)
      (noweb-update-chunk-vector))
  (aref noweb-chunk-vector (noweb-find-chunk-index-buffer)))

(defun noweb-chunk-is-code (index)
  "Return t if the chunk 'index' is a code chunk, nil otherwise"
  (interactive)
  (stringp (car (noweb-chunk-vector-aref index))))

(defun noweb-in-code-chunk ()
  "Return t if we are in a code chunk, nil otherwise."
  (interactive)
  (noweb-chunk-is-code (noweb-find-chunk-index-buffer)))

(defun noweb-in-mode-line ()
  "Return the name of the mode to use if we are in a mode line, nil
otherwise."
  (interactive)
  (let (beg end mode)
    (save-excursion
      (beginning-of-line 1)
      (and (progn
             (ess-write-to-dribble-buffer
              (format "(n-i-m-l: 1)"))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (ess-write-to-dribble-buffer
              (format "(n-i-m-l: 2)"))
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (ess-write-to-dribble-buffer
              (format "(n-i-m-l: 3)"))
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (setq mode (concat
                         (downcase (buffer-substring beg end))
                         "-mode"))
             (if (and (>= (length mode) 11))
                 (progn
                   (if
                       (equal (substring mode -10 -5) "-mode")
                       (setq mode (substring mode 0 -5)))
                   (if
                       (equal (substring mode 0 5) "mode:")
                       (setq mode (substring mode 6))))))
           (progn
             (ess-write-to-dribble-buffer
              (format "(n-i-m-l: 3) mode=%s" mode))
             (intern mode))))))

(defun noweb-find-chunk-index-buffer ()
  "Return the index of the current chunk in NOWEB-CHUNK-VECTOR."
  (noweb-find-chunk-index 0 (1- (length noweb-chunk-vector))))

(defun noweb-find-chunk-index (low hi)
  (if (= hi (1+ low))
      low
    (let ((med (/ (+ low hi) 2)))
      (if (< (point) (cdr (aref noweb-chunk-vector med)))
          (noweb-find-chunk-index low med)
        (noweb-find-chunk-index med hi)))))

(defun noweb-chunk-region ()
  "Return a pair consisting of the beginning and end of the current chunk."
  (interactive)
  (let ((start (noweb-find-chunk-index-buffer)))
    (cons (marker-position (cdr (aref noweb-chunk-vector start)))
          (marker-position (cdr (aref noweb-chunk-vector (1+ start)))))))

(defun noweb-copy-code-chunk ()
  "Copy the current code chunk to the kill ring, excluding the chunk name.
This will be particularly useful when interfacing with ESS."
  (interactive)
  (let ((r (noweb-chunk-region)))
    (save-excursion
      (goto-char (car r))
      (if (noweb-in-code-chunk)
          (progn
            (beginning-of-line 2)
            (copy-region-as-kill (point) (cdr r)))))))

(defun noweb-extract-code-chunk ()
  "Create a new buffer with the same name as the current code chunk,
and copy all code  from chunks of the same name to it."
  (interactive)
  (save-excursion
    (if (noweb-in-code-chunk)
        (progn
          (let ((chunk-name (car (noweb-find-chunk)))
                (chunk-counter 0)
                (copy-counter 0)
                (this-chunk) (oldbuf (current-buffer)))
            (if (get-buffer chunk-name)
                (progn
                  (set-buffer-modified-p nil)
                  (kill-buffer chunk-name)))
            (get-buffer-create chunk-name)
            (message "Created buffer %s" chunk-name)
            (while (< chunk-counter (- (length noweb-chunk-vector) 2))
              (setq this-chunk (noweb-chunk-vector-aref
                                chunk-counter))
              (message "Current buffer is %s" (car this-chunk))
              (if (equal chunk-name (car this-chunk))
                  (progn
                    (setq copy-counter (+ copy-counter 1))
                    (goto-char (cdr this-chunk))
                    (noweb-copy-code-chunk)
                    (set-buffer chunk-name)
                    (goto-char (point-max))
                    (yank)
                    (set-buffer oldbuf)))
              (setq chunk-counter (+ chunk-counter 1)))
            (message "Copied %d bits" copy-counter)
            (set-buffer chunk-name)
            (copy-region-as-kill (point-min)(point-max)))))))

(defun noweb-chunk-pair-region ()
  "Return a pair consisting of the beginning and end of the current pair of
documentation and code chunks."
  (interactive)
  (let* ((start (noweb-find-chunk-index-buffer))
         (end (1+ start)))
    (if (noweb-chunk-is-code start)
        (cons (marker-position (cdr (aref noweb-chunk-vector (1- start))))
              (marker-position (cdr (aref noweb-chunk-vector end))))
      (while (not (noweb-chunk-is-code  end))
        (setq end (1+ end)))
      (cons (marker-position (cdr (aref noweb-chunk-vector start)))
            (marker-position (cdr (aref noweb-chunk-vector (1+ end))))))))

(defun noweb-chunk-vector-aref (i)
  (if (< i 0)
      (error "Before first chunk."))
  (if (not noweb-chunk-vector)
      (noweb-update-chunk-vector))
  (if (>= i (length noweb-chunk-vector))
      (error "Beyond last chunk."))
  (aref noweb-chunk-vector i))

(defun noweb-complete-chunk ()
  "Complete the chunk name before point, if any."
  (interactive)
  (if (noweb-in-code-chunk)
      (let ((end (point))
            (beg (save-excursion
                   (if (re-search-backward "<<"
                                           (save-excursion
                                             (beginning-of-line)
                                             (point))
                                           t)
                       (match-end 0)
                     nil))))
        (if beg
            (let* ((pattern (buffer-substring beg end))
                   (alist (noweb-build-chunk-alist))
                   (completion (try-completion pattern alist)))
              (cond ((eq completion t))
                    ((null completion)
                     (message "Can't find completion for \"%s\"" pattern)
                     (ding))
                    ((not (string= pattern completion))
                     (delete-region beg end)
                     (insert completion)
                     (if (not (looking-at ">>"))
                         (insert ">>")))
                    (t
                     (message "Making completion list...")
                     (with-output-to-temp-buffer "*Completions*"
                       (display-completion-list (all-completions pattern alist)))
                     (message "Making completion list...%s" "done"))))
          (message "Not at chunk name...")))
    (message "Not in code chunk...")))


;;; Filling, etc

(defun noweb-hide-code-quotes ()
  "Replace all non blank characters in [[...]] code quotes
in the current buffer (you might want to narrow to the interesting
region first) by `*'.  Return a list of pairs with the position and
value of the original strings."
  (save-excursion
    (let ((quote-list nil))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[" nil 'move)
        (let ((beg (match-end 0))
              (end (if (re-search-forward "\\]\\]" nil t)
                       (match-beginning 0)
                     (point-max))))
          (goto-char beg)
          (while (< (point) end)
            ;; Move on to the next word:
            (let ((b (progn
                       (skip-chars-forward " \t\n" end)
                       (point)))
                  (e (progn
                       (skip-chars-forward "^ \t\n" end)
                       (point))))
              (if (> e b)
                  ;; Save the string and a marker to the end of the
                  ;; replacement text.  A marker to the beginning is
                  ;; useless.  See NOWEB-RESTORE-CODE-QUOTES.
                  (save-excursion
                    (setq quote-list (cons (cons (copy-marker e)
                                                 (buffer-substring b e))
                                           quote-list))
                    (goto-char b)
                    (insert-char ?* (- e b) t)
                    (delete-char (- e b))))))))
      (reverse quote-list))))

(defun noweb-restore-code-quotes (quote-list)
  "Reinsert the strings modified by `noweb-hide-code-quotes'."
  (save-excursion
    (mapcar (lambda (q)
              (let* ((e (marker-position (car q)))
                     ;; Slightly inefficient, but correct way to find
                     ;; the beginning of the word to be replaced.
                     ;; Using the marker at the beginning will loose
                     ;; if whitespace has been rearranged
                     (b (save-excursion
                          (goto-char e)
                          (skip-chars-backward "*")
                          (point))))
                (delete-region b e)
                (goto-char b)
                (insert (cdr q))))
            quote-list)))

(defun noweb-fill-chunk ()
  "Fill the current chunk according to mode.
Run `fill-region' on documentation chunks and `indent-region' on code
chunks."
  (interactive)
  (save-excursion
    (save-restriction
      (noweb-narrow-to-chunk)
      (if (noweb-in-code-chunk)
          (progn
            ;; Narrow to the code section proper; w/o the first and any
            ;; index declaration lines.
            (narrow-to-region (progn
                                (goto-char (point-min))
                                (forward-line 1)
                                (point))
                              (progn
                                (goto-char (point-max))
                                (forward-line -1)
                                (while (looking-at "@")
                                  (forward-line -1))
                                (forward-line 1)
                                (point)))
            (if (or indent-region-function indent-line-function)
                (indent-region (point-min) (point-max) nil)
              (error "No indentation functions defined in %s!" major-mode)))
        (if noweb-code-quotes-handling
            (let ((quote-list (noweb-hide-code-quotes)))
              (fill-region (point-min) (point-max))
              (noweb-restore-code-quotes quote-list))
          (fill-region (point-min) (point-max)))))))

(defun noweb-indent-line ()
  "Indent the current line according to mode, after narrowing to this chunk."
  (interactive)
  (noweb-update-chunk-vector)
  (save-restriction
    (noweb-narrow-to-chunk)
    (if (noweb-in-code-chunk)
        (progn
          ;; Narrow to the code section proper; w/o the first and any
          ;; index declaration lines.
          (save-excursion
            (narrow-to-region (progn
                                (goto-char (point-min))
                                (forward-line 1)
                                (point))
                              (progn
                                (goto-char (point-max))
                                (forward-line -1)
                                (while (looking-at "@")
                                  (forward-line -1))
                                (forward-line 1)
                                (point))))))
    (indent-according-to-mode)))

(defun noweb-fill-paragraph-chunk (&optional justify)
  "Fill a paragraph in the current chunk."
  (interactive "P")
  (noweb-update-chunk-vector)
  (save-excursion
    (save-restriction
      (noweb-narrow-to-chunk)
      (if (noweb-in-code-chunk)
          (progn
            ;; Narrow to the code section proper; w/o the first and any
            ;; index declaration lines.
            (narrow-to-region (progn
                                (goto-char (point-min))
                                (forward-line 1)
                                (point))
                              (progn
                                (goto-char (point-max))
                                (forward-line -1)
                                (while (looking-at "@")
                                  (forward-line -1))
                                (forward-line 1)
                                (point)))
            (fill-paragraph justify))
        (if noweb-code-quotes-handling
            (let ((quote-list (noweb-hide-code-quotes)))
              (fill-paragraph justify)
              (noweb-restore-code-quotes quote-list))
          (fill-paragraph justify))))))

(defun noweb-auto-fill-doc-chunk ()
  "Replacement for `do-auto-fill'."
  (save-restriction
    (narrow-to-region (car (noweb-chunk-region))
                      (save-excursion
                        (end-of-line)
                        (point)))
    (if noweb-code-quotes-handling
        (let ((quote-list (noweb-hide-code-quotes)))
          (do-auto-fill)
          (noweb-restore-code-quotes quote-list))
      (do-auto-fill))))

(defun noweb-auto-fill-doc-mode ()
  "Install the improved auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'noweb-auto-fill-doc-chunk)))

(defun noweb-auto-fill-code-mode ()
  "Install the default auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'do-auto-fill)))

;;; Marking

(defun noweb-mark-chunk ()
  "Mark the current chunk."
  (interactive)
  (let ((r (noweb-chunk-region)))
    (goto-char (car r))
    (push-mark (cdr r) nil t)))

(defun noweb-mark-chunk-pair ()
  "Mark the current pair of documentation and code chunks."
  (interactive)
  (let ((r (noweb-chunk-pair-region)))
    (goto-char (car r))
    (push-mark (cdr r) nil t)))


;;; Narrowing

(defun noweb-toggle-narrowing (&optional arg)
  "Toggle if we should narrow the display to the current pair of
documentation and code chunks after each movement.  With argument:
switch narrowing on."
  (interactive "P")
  (if (or arg (not noweb-narrowing))
      (progn
        (setq noweb-narrowing t)
        (noweb-narrow-to-chunk-pair))
    (setq noweb-narrowing nil)
    (widen)))

(defun noweb-narrow-to-chunk ()
  "Narrow the display to the current chunk."
  (interactive)
  (let ((r (noweb-chunk-region)))
    (narrow-to-region (car r) (cdr r))))

(defun noweb-narrow-to-chunk-pair ()
  "Narrow the display to the current pair of documentation and code chunks."
  (interactive)
  (let ((r (noweb-chunk-pair-region)))
    (narrow-to-region (car r) (cdr r))))


;;; Killing

(defun noweb-kill-chunk ()
  "Kill the current chunk."
  (interactive)
  (let ((r (noweb-chunk-region)))
    (kill-region (car r) (cdr r))))

(defun noweb-kill-chunk-pair ()
  "Kill the current pair of chunks."
  (interactive)
  (let ((r (noweb-chunk-pair-region)))
    (kill-region (car r) (cdr r))))

(defun noweb-copy-chunk-as-kill ()
  "Place the current chunk on the kill ring."
  (interactive)
  (let ((r (noweb-chunk-region)))
    (copy-region-as-kill (car r) (cdr r))))

(defun noweb-copy-chunk-pair-as-kill ()
  "Place the current pair of chunks on the kill ring."
  (interactive)
  (let ((r (noweb-chunk-pair-region)))
    (copy-region-as-kill (car r) (cdr r))))


;;; Movement

(defun noweb-sign (n)
  "Return the sign of N."
  (if (< n 0) -1 1))

(defun noweb-next-doc-chunk (&optional cnt)
  "Goto to the Nth documentation chunk from point."
  (interactive "p")
  (widen)
  (let ((start (noweb-find-chunk-index-buffer))
        (i 1))
    (while (<= i (abs cnt))
      (setq start (+ (noweb-sign cnt) start))
      (while (noweb-chunk-is-code start)
        (setq start (+ (noweb-sign cnt) start)))
      (setq i (1+ i)))
    (goto-char (marker-position (cdr (noweb-chunk-vector-aref start))))
    (forward-char 1))
  (if noweb-narrowing
      (noweb-narrow-to-chunk-pair)))

(defun noweb-previous-doc-chunk (&optional n)
  "Goto to the -Nth documentation chunk from point."
  (interactive "p")
  (noweb-next-doc-chunk (- n)))

(defun noweb-next-code-chunk (&optional cnt)
  "Goto to the Nth code chunk from point."
  (interactive "p")
  (widen)
  (let ((start (noweb-find-chunk-index-buffer))
        (i 1))
    (while (<= i (abs cnt))
      (setq start (+ (noweb-sign cnt) start))
      (while (not (noweb-chunk-is-code start))
        (setq start (+ (noweb-sign cnt) start)))
      (setq i (1+ i)))
    (goto-char (marker-position (cdr (noweb-chunk-vector-aref start))))
    (forward-line 1))
  (if noweb-narrowing
      (noweb-narrow-to-chunk-pair)))

(defun noweb-previous-code-chunk (&optional n)
  "Goto to the -Nth code chunk from point."
  (interactive "p")
  (noweb-next-code-chunk (- n)))

(defun noweb-next-chunk (&optional n)
  "If in a documentation chunk, goto to the Nth documentation
chunk from point, else goto to the Nth code chunk from point."
  (interactive "p")
  (if (noweb-in-code-chunk)
      (noweb-next-code-chunk n)
    (noweb-next-doc-chunk n)))

(defun noweb-previous-chunk (&optional n)
  "If in a documentation chunk, goto to the -Nth documentation
chunk from point, else goto to the -Nth code chunk from point."
  (interactive "p")
  (noweb-next-chunk (- n)))

(defvar noweb-chunk-history nil
  "")

(defun noweb-goto-chunk ()
  "Goto the named chunk."
  (interactive)
  (widen)
  (let* ((completion-ignore-case t)
         (alist (noweb-build-chunk-alist))
         (chunk (ess-completing-read
                 "Chunk" (delete "" (mapcar 'car alist)) nil t nil
                 noweb-chunk-history (noweb-goto-chunk-default))))
    (goto-char (cdr (assoc chunk alist))))
  (if noweb-narrowing
      (noweb-narrow-to-chunk-pair)))

(defun noweb-goto-chunk-default ()
  (save-excursion
    (if (re-search-backward "<<"
                            (save-excursion
                              (beginning-of-line)
                              (point))
                            'move)
        (goto-char (match-beginning 0)))
    (if (re-search-forward "<<\\(.*\\)>>"
                           (save-excursion
                             (end-of-line)
                             (point))
                           t)
        (buffer-substring (match-beginning 1) (match-end 1))
      nil)))

(defun noweb-build-chunk-alist ()
  (if (not noweb-chunk-vector)
      (noweb-update-chunk-vector))
  ;; The naive recursive solution will exceed MAX-LISP-EVAL-DEPTH in
  ;; buffers w/ many chunks.  Maybe there is a tail recursivce solution,
  ;; but iterative solutions should be acceptable for dealing with vectors.
  (let ((alist nil)
        (i (1- (length noweb-chunk-vector))))
    (while (>= i 0)
      (let* ((chunk (aref noweb-chunk-vector i))
             (name (car chunk))
             (marker (cdr chunk)))
        (if (and (stringp name)
                 (not (assoc name alist)))
            (setq alist (cons (cons name marker) alist))))
      (setq i (1- i)))
    alist))

(defun noweb-goto-next (&optional cnt)
  "Goto the continuation of the current chunk."
  (interactive "p")
  (widen)
  (if (not noweb-chunk-vector)
      (noweb-update-chunk-vector))
  (let ((start (noweb-find-chunk-index-buffer)))
    (if (not (noweb-chunk-is-code  start))
        (setq start (1+ start)))
    (if (noweb-chunk-is-code start)
        (let ((name (car (noweb-chunk-vector-aref start)))
              (i 1))
          (while (<= i (abs cnt))
            (setq start (+ (noweb-sign cnt) start))
            (while (not (equal (car (noweb-chunk-vector-aref start))
                               name))
              (setq start (+ (noweb-sign cnt) start)))
            (setq i (1+ i)))
          (goto-char (marker-position
                      (cdr (noweb-chunk-vector-aref start))))
          (forward-line 1))))
  (if noweb-narrowing
      (noweb-narrow-to-chunk-pair)))

(defun noweb-goto-previous (&optional cnt)
  "Goto the previous chunk."
  (interactive "p")
  (noweb-goto-next (- cnt)))

(defun noweb-occur (arg)
  "Find all occurences of the current chunk.
This function simply runs OCCUR on \"<<NAME>>\"."
  (interactive "P")
  (let ((n (if (and arg
                    (numberp arg))
               arg
             0))
        (idx (noweb-find-chunk-index-buffer)))
    (if (noweb-chunk-is-code idx)
        (occur (regexp-quote (concat "<<"
                                     (car (aref noweb-chunk-vector idx))
                                     ">>"))
               n)
      (setq idx (1+ idx))
      (while (not (noweb-chunk-is-code idx))
        (setq idx (1+ idx)))
      (occur (regexp-quote (concat "<<"
                                   (car (aref noweb-chunk-vector idx))
                                   ">>"))
             n))))


;;; Insertion

(defun noweb-new-chunk (name)
  "Insert a new chunk."
  (interactive "sChunk name: ")
  (insert "@ \n<<" name ">>=\n")
  (save-excursion
    (insert "@ %def \n"))
  (noweb-update-chunk-vector))

(defun noweb-at-beginning-of-line ()
  (equal (save-excursion
           (beginning-of-line)
           (point))
         (point)))

(defun noweb-electric-@ (arg)
  "Smart incarnation of `@', starting a new documentation chunk, maybe.
If given an numerical argument, it will act just like the dumb `@'.
Otherwise and if at the beginning of a line in a code chunk:
insert \"@ \" and update the chunk vector."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (noweb-at-beginning-of-line)
             (noweb-in-code-chunk))
        (progn
          (insert "@ ")
          (noweb-update-chunk-vector))
      (self-insert-command 1))))

(defun noweb-electric-< (arg)
  "Smart incarnation of `<', starting a new code chunk, maybe.
If given an numerical argument, it will act just like the dumb `<'.
Otherwise and if at the beginning of a line in a documentation chunk:
insert \"<<>>=\", a closing \"@\" and a newline if necessary.  Leave point
in the middle and and update the chunk vector."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (noweb-at-beginning-of-line)
             (not (noweb-in-code-chunk)))
        (progn
          (insert "<<")
          (save-excursion
            (insert ">>=\n@ ")
            (if (not (looking-at "\\s *$"))
                (newline)))
          (noweb-update-chunk-vector))
      (self-insert-command 1))))


;;; Modes

(defun noweb-set-chunk-code-mode ()
  "Set the noweb-code-mode for the current chunk"
  (interactive)
  (if (noweb-in-code-chunk)
      (progn
        ;; Reset code-mode to default and then check for a mode comment.
        (setq noweb-code-mode noweb-default-code-mode)
        (let (mode chunk-name)
          (save-excursion
            (save-restriction
              (end-of-line)
              (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
              (setq chunk-name (match-string 1))
              (widen)
              (goto-char (point-min))
              (re-search-forward (concat "^<<" (regexp-quote chunk-name) ">>=") nil t)
              (beginning-of-line 2)
              (setq mode (noweb-in-mode-line))
              (if (functionp mode)
                  (setq noweb-code-mode mode))))))
    (error "This only makes sense in a code chunk")))

(defun noweb-set-doc-syntax-table ()
  "Sets the doc-mode syntax-table to treat code quotes as comments."
  (interactive)
  (let ((square-bracket-string (char-to-string (char-syntax ?\[))))
    (if (string= square-bracket-string "(")
        (progn
          (modify-syntax-entry ?\[ "(]12b" noweb-doc-mode-syntax-table)
          (modify-syntax-entry ?\] ")[34b" noweb-doc-mode-syntax-table))
      (progn
        (modify-syntax-entry  ?\[
                              (concat square-bracket-string " 12b")
                              noweb-doc-mode-syntax-table)
        (modify-syntax-entry  ?\]
                              (concat square-bracket-string " 34b")
                              noweb-doc-mode-syntax-table)))))

(defun noweb-select-mode ()
  "Select NOWEB-DOC-MODE or NOWEB-CODE-MODE, as appropriate."
  (interactive)
  (let ((this-chunk-index (noweb-find-chunk-index-buffer)))
    ;; Has the last change to the buffer taken us into a different
    ;; chunk ?
    (if (not (equal this-chunk-index noweb-last-chunk-index))
        (progn
          (setq noweb-last-chunk-index this-chunk-index)
          (if (noweb-in-code-chunk)
              ;; Inside a code chunk
              (progn
                ;; Find out which code mode to use
                (noweb-set-chunk-code-mode)
                ;; If we aren't already using it, use it.
                (if (not (equal major-mode noweb-code-mode))
                    (progn
                      (funcall noweb-code-mode)
                      (run-hooks 'noweb-select-mode-hook)
                      (run-hooks 'noweb-select-code-mode-hook))))
            ;; Inside a documentation chunk
            (progn
              (if (not (equal major-mode noweb-doc-mode))
                  (progn
                    (funcall noweb-doc-mode)))
              (if (not noweb-doc-mode-syntax-table)
                  (progn
                    (message "Setting up syntax table")
                    (setq noweb-doc-mode-syntax-table
                          (make-syntax-table (syntax-table)))
                    (noweb-set-doc-syntax-table)))
              (set-syntax-table noweb-doc-mode-syntax-table)
              (run-hooks 'noweb-select-mode-hook)
              (run-hooks 'noweb-select-doc-mode-hook)))
          (run-hooks 'noweb-changed-chunk-hook)))))

(defvar noweb-doc-mode noweb-default-doc-mode
  "Default major mode for editing noweb documentation chunks.
It is not possible to have more than one doc-mode in a file.
However, this variable is used to determine whether the doc-mode needs
to by added to the mode-line")

(defun noweb-set-doc-mode (mode)
  "Change the major mode for editing documentation chunks."
  (interactive "CNew major mode for documentation chunks: ")
  (setq noweb-doc-mode mode)
  (setq noweb-doc-mode-syntax-table nil)
  ;;Pretend we've changed chunk, so the mode will be reset if necessary
  (setq noweb-last-chunk-index (1- noweb-last-chunk-index))
  (noweb-select-mode))

(defun noweb-set-code-mode (mode)
  "Change the major mode for editing all code chunks."
  (interactive "CNew major mode for all code chunks: ")
  (setq noweb-default-code-mode mode)
  ;;Pretend we've changed chunk, so the mode will be reset if necessary
  (setq noweb-last-chunk-index (1- noweb-last-chunk-index))
  (noweb-select-mode))

(defun noweb-set-this-code-mode (mode)
  "Change the major mode for editing this code chunk.
The only sensible way to do this is to add a mode line to the chunk"
  (interactive "CNew major mode for this code chunk: ")
  (if (noweb-in-code-chunk)
      (progn
        (setq noweb-code-mode mode)
        (save-excursion
          (save-restriction
            (let (chunk-name)
              (widen)
              (end-of-line)
              (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
              (setq chunk-name (match-string 1))
              (goto-char (point-min))
              (re-search-forward (concat "^<<" (regexp-quote chunk-name) ">>=") nil t)
              (beginning-of-line 2))
            ;; remove mode-line, if there is one
            (if (noweb-in-mode-line)
                (progn
                  (kill-line)
                  (kill-line)))
            (if (not (equal noweb-code-mode noweb-default-code-mode))
                (progn
                  (setq mode (substring (symbol-name mode) 0 -5))
                  ;; Need to set major mode so that we can comment out
                  ;; the mode line
                  (funcall noweb-code-mode)
                  (if (not (boundp 'comment-start))
                      (setq comment-start "#"))
                  (insert comment-start
                          " -*- " mode
                          " -*- " comment-end "\n")))
            (setq noweb-last-chunk-index (1- noweb-last-chunk-index)))))
    (message "This only makes sense in a code chunk.")))

;;; Misc

(defun noweb-mode-version ()
  "Echo the RCS identification of noweb mode."
  (interactive)
  (message "Thorsten's noweb-mode (PRERELEASE). RCS: %s"
           noweb-mode-RCS-Id))

(defun noweb-describe-mode ()
  "Describe noweb mode."
  (interactive)
  (describe-function 'noweb-mode))

(defun noweb-insert-default-mode-line ()
  "Insert line that will set the noweb mode of this file in emacs.
The file is set to use the current doc and default-code modes, so
ensure they are set correctly (with noweb-set-code-mode and
noweb-set-doc-mode) before calling this function"
  (interactive)
  (save-excursion
    (goto-char 1)
    (if (noweb-in-mode-line)
        (progn
          (kill-line)
          (kill-line)))
    (if (not (eq major-mode noweb-doc-mode))
        (noweb-select-mode))
    (insert comment-start " -*- mode: noweb; noweb-default-code-mode: "
            (symbol-name noweb-default-code-mode)
            (if (not (eq noweb-doc-mode noweb-default-doc-mode))
                (concat "; noweb-doc-mode: " (symbol-name
                                              noweb-doc-mode) ";")
              ";")
            " -*-" comment-end "\n"))
  (noweb-select-mode))

(defun noweb-mouse-first-button (event)
  (interactive "e")
  (mouse-set-point event)
  (if (and noweb-use-mouse-navigation
           (eq (save-excursion
                 (end-of-line)
                 (re-search-backward "^[\t ]*\\(<<\\)\\(.*\\)\\(>>\\)" nil t))
               (save-excursion
                 (beginning-of-line) (point))))
      (progn
        (if (< (point) (match-beginning 2))
            (let ((chunk-name (buffer-substring-no-properties
                               (match-beginning 2)
                               (match-end 2))))
              (re-search-backward (concat "<<" (regexp-quote chunk-name) ">>") nil t))
          (if (and (<= (match-end 2) (point))
                   (>  (+ 2 (match-end 2)) (point)))
              (let ((chunk-name (buffer-substring-no-properties
                                 (match-beginning 2)
                                 (match-end 2))))
                (re-search-forward (concat "<<" (regexp-quote chunk-name) ">>") nil t)))))))


;;; Debugging

(defun noweb-log (s)
  (let ((b (current-buffer)))
    (switch-to-buffer (get-buffer-create "*noweb-log*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert s)
    (setq buffer-read-only t)
    (switch-to-buffer b)))





(defvar noweb-thread-alist nil
  "A list of threads in the current buffer.
Each entry in the list contains 5 elements:
1) The name of the threads
2) The name of the immdiate parent thread in which it is used (nil if
   it is a \"top-level\" thread which is not used anywhere).
3) The name of the top-level parent thread in which it is used (i.e. a
   thread in which it is used but which is not itself used anywhere:
   nil if this thread is not used anywhere.
4) The format string to use to define line numbers in the output
   file of this thread. Should only be set if this thread is not used
   anywhere: if a thread is used as part of another thread, the parent
   thread's format string should be used.
5) If this is nil, tabs are converted to spaces in the tangled
   file. If it is a number, tabs are copied to the tangled file
   unchanged, and tabs are also used for indentation, with the number
   of spaces per tab defined by this number. This MUST be set in order
   to tangle makefiles, which depend on tabs.Should only be set if
   this thread is not used anywhere. otherwise set to nil. "
  )

(defun noweb-update-thread-alist ()
  "Updates the list of threads in the current buffer.
Each entry in the list contains 5 elements:
1) The name of the thread
2) The name of the immdiate parent thread in which it is used (nil if
   it is a \"top-level\" thread which is not used anywhere).
3) The name of the top-level parent thread in which it is used (i.e. a
   thread in which it is used but which is not itself used anywhere:
   nil if this thread is not used anywhere.
4) The format string to use to define line numbers in the output
   file of this thread. Should only be set if this thread is not used
   anywhere: if a thread is used as part of another thread, the parent
   thread's format string should be used.
5) If this is nil, tabs are converted to spaces in the tangled
   file. If it is a number, tabs are copied to the tangled file
   unchanged, and tabs are also used for indentation, with the number
   of spaces per tab defined by this number. This MUST be set in order
   to tangle makefiles, which depend on tabs.Should only be set if
   this thread is not used anywhere. otherwise set to nil. "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((thread-alist) (thread-list-entry) (chunk-use-name)
          (current-thread) (new-thread-alist))
      (while (re-search-forward
              "^[ \t]*<<\\(.*\\)>>\\(=\\)?" nil t)
        (goto-char (match-beginning 0))
        ;; Is this the definition of a chunk ?
        (if (match-beginning 2)
            ;;We have a chunk definition
            (progn
              ;; Get the thread name
              (setq current-thread
                    (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1)))
              ;; Is this thread already in our list ?
              (if (assoc current-thread thread-alist)
                  nil
                (progn
                  ;; If not, create an entry with 4 nils at the end
                  (setq thread-list-entry
                        (list (cons current-thread
                                    (make-list 4 nil))))
                  ;; And add it to the list
                  (setq thread-alist
                        (append thread-alist thread-list-entry)))))

          ;; Not a definition but a use
          (progn
            ;; Get the thread name
            (setq chunk-use-name
                  (buffer-substring-no-properties (match-beginning 1)
                                                  (match-end 1)))
            ;; Has the thread already been defined before being used ?
            (if (setq thread-list-entry (assoc chunk-use-name
                                               thread-alist))
                ;; If it has, set its parent to be the thread we are in at the moment
                (setcar (cdr thread-list-entry) current-thread)
              ;; If not, add it to the list, with its parent name and 3 nils
              (progn
                (setq thread-list-entry
                      (list (cons chunk-use-name
                                  (cons current-thread
                                        (make-list 3 nil)))))
                (setq thread-alist (append thread-alist thread-list-entry)))))
          )
        ;;Go to the next line
        (beginning-of-line 2))
      ;; Now, the second element of each entry points to that thread's
      ;; immediate parent. Need to set it to the thread's ultimate
      ;; parent.
      (let ((thread-counter 0)
            (this-thread)
            (this-thread-parent))
        (while (<= thread-counter (1- (length thread-alist)))
          (setq this-thread (nth thread-counter thread-alist))
          (setq this-thread-parent (assoc
                                    (car (cdr this-thread))
                                    thread-alist))
          (while (not (equal nil (car (cdr this-thread-parent))))
            (setq this-thread-parent (assoc
                                      (car (cdr this-thread-parent))
                                      thread-alist)))
          (setq this-thread (cons (car this-thread)
                                  (cons (car (cdr this-thread))
                                        (cons (car this-thread-parent)
                                              (nthcdr 2 this-thread)))))
          (setq new-thread-alist (append new-thread-alist (list this-thread)))
          (setq thread-counter (1+ thread-counter))))

      (setq noweb-thread-alist new-thread-alist))))


                                        ; Option setting functions to go here

(defun noweb-set-thread-line-format ())

(defun noweb-set-thread-tabs ())


(defvar noweb-default-line-number-format nil
  "The format string to use to  define line numbers in this thread.
If nil, do  not use line numbers.")

(defvar noweb-default-line-number-skip-lines 0
  "The number of initial lines to output before the line number.
This may be useful in shell scripts, where the first line (or two) must have a
  specific form.")

(defvar noweb-default-tab-width 8
  "If a number, convert tabs to  that number of spaces in the output. If nil, let tabs through to the output unaltered.")

(defvar noweb-line-number-format  noweb-default-line-number-format
  "The format string to use to  define line numbers in this thread.
If nil, do  not use line numbers.")

(defvar noweb-line-number-skip-lines noweb-default-line-number-skip-lines
  "The number of initial lines to output before the line number.
This may be useful in shell scripts, where the first line (or two) must have a
  specific form.")

(defvar noweb-tab-width  noweb-default-tab-width
  "If a number, convert tabs to  that number of spaces in the output. If nil, let tabs through to the output unaltered.")

(defun noweb-get-thread-local-variables ()
  "Get the values of the variables that are local to a thread."
  (interactive)
  (save-excursion
    (save-restriction
      (end-of-line)
      (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
      (let ((chunk-name (match-string 1)))
        (widen)
        (goto-char (point-min))
        (re-search-forward (concat "^<<" (regexp-quote chunk-name) ">>=") nil t)
        (beginning-of-line 2)
        (while (looking-at ".*-\*-.*-\*-")
          (let ((this-line (buffer-substring-no-properties
                            (point)
                            (progn (end-of-line) (point)))))
            (if (string-match
                 "mode:[ \t]*\\([^\t ]*\\)" this-line)
                (setq noweb-code-mode
                      (if (featurep 'xemacs)
                          (match-string 1 this-line)
                        (match-string-no-properties 1 this-line))
                      ))
            (if (string-match
                 "noweb-line-number-format:[ \t]*\"\\([^\"]*\\)\"" this-line)
                (setq noweb-line-number-format
                      (if (featurep 'xemacs)
                          (match-string 1 this-line)
                        (match-string-no-properties 1 this-line))
                      ))
            (if (string-match
                 "noweb-line-number-skip-lines:[ \t]*\\([^\t ]*\\)" this-line)
                (setq noweb-line-number-skip-lines
                      (string-to-number
                       (if (featurep 'xemacs)
                           (match-string 1 this-line)
                         (match-string-no-properties 1 this-line)))))
            (if (string-match
                 "noweb-tab-width:[ \t]*\\([^\t ]*\\)" this-line)
                (setq noweb-tab-width
                      (string-to-number
                       (if (featurep 'xemacs)
                           (match-string 1 this-line)
                         (match-string-no-properties 1 this-line)))))
            (beginning-of-line 2)))))))

(defun noweb-reset-thread-local-variables ()
  "Resets the thread-local variables to their default values"
  (setq noweb-tab-width noweb-default-tab-width)
  (setq noweb-line-number-format noweb-default-line-number-format)
  (setq noweb-line-number-skip-lines noweb-default-line-number-skip-lines))

(defun noweb-write-line-number (line-number-format buffer)
  (if line-number-format
      (progn
        (let ((this-line (count-lines (point-min)(point))))
          (while (string-match ".*\\(%L\\).*" line-number-format)
            (setq line-number-format
                  (replace-match
                   (format "%d" this-line) t t line-number-format 1)))
          (while (string-match ".*\\(%F\\).*" line-number-format)
            (setq line-number-format
                  (replace-match
                   (format "%s" (buffer-file-name)) t t line-number-format 1)))
          (while (string-match ".*\\(%N\\).*" line-number-format)
            (setq line-number-format
                  (replace-match "\n" t t line-number-format 1)))
          (save-excursion
            (set-buffer buffer)
            (insert line-number-format))))))


(defun noweb-tangle-chunk ( &optional buffer prefix-string)
  "Generate the code produced by this chunk, & any threads used in this chunk."
  (interactive)
  (save-excursion
    (noweb-reset-thread-local-variables)
    (noweb-get-thread-local-variables)
    (noweb-update-chunk-vector)
    (let*
        ((chunk-end (progn
                      (end-of-line)
                      (re-search-forward "^@" nil t)
                      (beginning-of-line)
                      (point)))
         ;;get name and start point of this chunk
         (chunk-start (progn
                        (re-search-backward "^<<\\([^>]*\\)>>=$" nil t)
                        (beginning-of-line 2)
                        (point)))
         (chunk-name (buffer-substring-no-properties
                      (match-end 1)
                      (match-beginning 1)))
         ;; get end of this chunk
         ;; Get information we need about this thread
         (thread-info (assoc chunk-name noweb-thread-alist))
         (thread-tabs (nth 4 thread-info))
         (line-number-format (nth 3 thread-info))
         (thread-name-re) (post-chunk) (pre-chunk)
         (first-line t)
         (tangle-buffer (generate-new-buffer "Tangle Buffer")))

      (progn
        (goto-char chunk-start)
        ;; If this is a mode-line, ignore it
        (while (looking-at ".*-\\*-.*-\\*-")
          (beginning-of-line 2))
        ;; If we want to include line numbers, write one
        (if line-number-format
            (while (> noweb-line-number-skip-lines 0)
              (append-to-buffer tangle-buffer
                                (point)
                                (save-excursion
                                  (progn
                                    (end-of-line)
                                    (point))))
              (beginning-of-line 2)
              (1- noweb-line-number-skip-lines))
          (noweb-write-line-number line-number-format buffer))
        (message "Now at %d" (point))

        (while (< (point) chunk-end)
          (untabify (point) (save-excursion (beginning-of-line 2)(point)))
          ;; This RE gave me trouble. Without the `\"', it
          ;; recognised itself and so could not copy itself
          ;; correctly.
          (if (looking-at
               "\\([^\n\"@]*\\)<<\\(.*\\)\\(>>\\)\\([^\n\"]*\\)$")
              (progn
                (save-excursion
                  (save-restriction
                    (setq thread-name-re
                          (concat "<<"
                                  (regexp-quote (match-string 2))
                                  ">>="))
                    (setq pre-chunk (match-string 1))
                    (if prefix-string
                        (setq pre-chunk (concat prefix-string
                                                pre-chunk)))
                    (setq post-chunk (match-string 4))
                    (widen)
                    (goto-char (point-min))
                    (while (re-search-forward thread-name-re nil t)
                      (noweb-tangle-chunk tangle-buffer pre-chunk)
                      (forward-line 1)))
                  (if post-chunk
                      (save-excursion
                        (set-buffer tangle-buffer)
                        (backward-char)
                        (insert post-chunk)
                        (beginning-of-line 2)))))

            ;; Otherwise, just copy this line
            (setq pre-chunk
                  (buffer-substring
                   (point)
                   (save-excursion
                     (beginning-of-line 2)
                     (point))))
            ;; Add a prefix if necessary
            (if (and prefix-string
                     (> (length pre-chunk) 1))
                (setq pre-chunk (concat prefix-string
                                        pre-chunk)))
            ;; And copy it to the buffer
            (save-excursion
              (set-buffer tangle-buffer)
              (insert pre-chunk)))
          ;; If this is the first line of the chunk, we need to change
          ;; prefix-string to consist solely of spaces
          (if (and first-line
                   prefix-string)
              (progn
                (setq prefix-string
                      (make-string (length prefix-string) ?\  ))
                (setq first-line nil)))
          ;; Either way, go to the next line
          (beginning-of-line 2))

        (save-excursion
          (set-buffer tangle-buffer)
          (goto-char (point-min))
          (while (re-search-forward "\@\<<" nil t)
            (replace-match "<<" nil nil)
            (forward-char 3))
          (if thread-tabs
              (progn
                (setq tab-width thread-tabs)
                (tabify (point-min)(point-max)))
            (untabify (point-min)(point-max))))

        (if buffer
            (save-excursion
              (set-buffer buffer)
              (insert-buffer-substring tangle-buffer)
              (kill-buffer tangle-buffer)))
        ))))

(defun noweb-tangle-thread ( name &optional buffer)
  "Given the name of a thread, tangles the thread to buffer.
If no buffer is given, create a new one with the same name as the
thread."
  (interactive "sWhich thread ? ")
  (if (not buffer)
      (progn
        (setq buffer (get-buffer-create name))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer))))
  (save-excursion
    (goto-char (point-min))
    (let ((chunk-counter 0))
      (while (re-search-forward
              "^<<\\(.*\\)>>=[\t ]*" nil t)
        (if (string= (match-string 1)
                     name)
            (progn
              (setq chunk-counter (1+ chunk-counter))
              (message "Found %d chunks" chunk-counter)
              (noweb-tangle-chunk buffer)))))))

(defun noweb-tangle-current-thread ( &optional buffer)
  (interactive)
  (save-excursion
    (let* ((chunk-start
            (progn
              (re-search-backward "^<<\\([^>]*\\)>>=[\t ]*$"
                                  nil t)
              (beginning-of-line 2)
              (point)))
           (chunk-name (buffer-substring-no-properties
                        (match-end 1)
                        (match-beginning 1))))
      (noweb-tangle-thread chunk-name buffer))))
                                        ;menu functions


;;; Finale

(run-hooks 'noweb-mode-load-hook)
(provide 'noweb-mode)

;; Changes made by Mark Lunt (mark.lunt@mrc-bsu.cam.ac.uk) 22/03/1999

;; The possibility of having code chunks using more than one language
;; was added. This was first developed by Adnan Yaqub
;; (AYaqub@orga.com) for syntax highlighting, but even people who hate
;; highlighting may like to maintain their Makefile with their code,
;; or test-scripts with their programs, or even user documentation as
;; latex-mode code chunks.
;; This required quite a few changes to noweb-mode:
;; 1) A new variable `noweb-default-code-mode' was create to do the job
;;    `noweb-code-mode' used to.
;; 2) noweb-code-mode now contains the code-mode of the current chunk
;; 3) Each chunk can now have its own mode-line to tell emacs what
;;    mode to use to edit it. The function `noweb-in-mode-line'
;;    recognises such mode-lines, and the function
;;    `noweb-set-this-code-mode' sets the code mode for the current
;;    chunk and adds a mode-line if necessary. If several chunks have
;;    the same name, the mode-line must appear in the first chunk with
;;    that name.
;; 4) The mechanism for deciding whether to change mode was altered,
;;    since the old method assumed a single code mode. Now,
;;    `noweb-last-chunk-index' keeps track of which chunk we were in
;;    last. If we have moved to a different chunk, we have to check
;;    which mode we should be in, and change if necessary.

;; The keymap and menu-map handling was changed. Easymenu was used to
;; define the menu, and it the keymap was attached to the 'official'
;; minor-modes-keymaps list. This means that
;; 1) It was automatically loaded when noweb-mode was active and
;;    unloaded when it was inactive.
;; 2) There was no need to worry about the major mode map clobbering
;;    it , since it takes precedence over the major mode
;;    map. `noweb-setup-keymap' is therefore now superfluous
;; The menu was also reorganised to make it less cluttered, so there
;; would be room for adding tangling and weaving commands (one day).

;; Mouse navigation (at least under Emacs (AJR)) is supported, in so
;; far as clicking mouse-1 on the '<<' of a chunk name moves to the
;; previous instance of that chunk name, and clicking in the '>>'
;; moves to the next instance. They are not mouse-hightlighted,
;; though: too much hassle for zero added functionality.

;; noweb-doc-mode has been given its own syntax-table. It is the same
;; as the current doc-mode syntax-table, except that [[ is a comment
;; start and ]] a comment end. Fixes some ugliness in LaTeX-mode if
;; `$' or `%' appear in quoted code (or even `<<', which happens often
;; in C++).
;; (This should make noweb-hide-code-quotes and
;; noweb-restore-code-quotes unnecessary, but I have not yet removed
;; them, nor the calls to them).

;; A new function `noweb-indent-line' was defined and bound by default
;; to the tab key. This should indent the current line correctly in
;; whichever mode we are currently in. Previously, c-mode in
;; particular did not behave well with indentation (although
;; `noweb-fill-chunk' worked fine). Indentation is only accurate
;; within the chunk: it does not know the syntax at the end of the
;; previous chunk, so it does not know where to start indenting in
;; this chunk. However, provided the indentation within each chunk is correct,
;; notangle will correctly indented code.

;; (I think it would be good to separate filling and indenting,
;; though, since `indent-region' and `fill-region' have completely
;; different meanings in LaTeX-mode (and both are useful))

;; noweb-mode and noweb-minor-mode were given an optional argument, so
;; that (noweb-mode -1) turns it off, (noweb-mode 1) turns it on, and
;; (noweb-mode) toggles it. This is considered normal for minor modes.

;; buffer-substring changed to buffer-substring-no-properties:
;; comparisons with buffer-substring can be unreliable if highlighting
;; is used.

;; New functions `noweb-in-code-chunk' & `noweb-chunk-is-code' created
;; to replace (if (stringp (car (noweb-find-chunk)))) and
;; (if (stringp (car (noweb-chunk-vector-aref index)))).

;; `noweb-insert-mode-line' was renamed
;; `noweb-insert-default-mode-line' and modified to put the mode-line
;; at the start of the file and remove any existing mode-line.

;; a '<=' in `noweb-find-chunk-index' changed to '<', so we get the
;; right answer if point is on the first character in a chunk

;; The name of `noweb-post-command-hook' changed to
;; `noweb-post-command-function', since it is a function.

;; All the highlighting code moved to a separate file:
;; (noweb-font-lock-mode.el)

;; Menu driven tangling is in the process of being added. It can
;; currently tangle a single chunk or a series of  chunks with the
;; same name (which I refer to as a thread) into a separate
;; buffer. This buffer can then be saved to a file, sent to an
;; interpreter, whatever. I haven't tested using line-numbers as yet.

;;; noweb-mode.el ends here
