;; ess-latex.el - Interface ESS and LaTeX.

;; Copyright (C) 1999 by A.J. Rossini <rossini@biostat.washington.edu>
;; based on noweb-mode by: Thorsten.Ohl @ Physik.TH-Darmstadt.de
;;     with a little help from Norman Ramsey <norman@bellcore.com>
;;                         and Mark Lunt <mark.lunt@mrc-bsu.cam.ac.uk>

;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

;; This file is part of ESS.

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



;;; Variables

(defconst ess-latex-minor-mode-RCS-Id
  "Imported to ESS Subversion repository and RCS ids not maintained.")

(defconst ess-latex-minor-mode-RCS-Name
  " ")

(defvar ess-latex-minor-mode-prefix "\M-n"
  "*Prefix key to use for noweb mode commands.
The value of this variable is checked as part of loading noweb mode.
After that, changing the prefix key requires manipulating keymaps.")

(defvar ess-latex-minor-mode-load-hook nil
  "Hook that is run after noweb mode is loaded.")

(defvar ess-latex-mode-hook nil
  "Hook that is run after entering ess-latex mode.")

(defvar ess-latex-select-code-mode-hook nil
  "Hook that is run after the code mode is selected.
This is the place to overwrite keybindings of the ESS-LATEX-CODE-MODE.")

(defvar ess-latex-select-doc-mode-hook nil
  "Hook that is run after the documentation mode is selected.
This is the place to overwrite keybindings of the ESS-LATEX-DOC-MODE.")

(defvar ess-latex-select-mode-hook nil
  "Hook that is run after the documentation or the code mode is selected.
This is the place to overwrite keybindings of the other modes.")

(defvar ess-latex-changed-chunk-hook nil
  "Hook that is run every time point moves from one chunk to another.
It will be run whether or not the major-mode changes.")

(defvar ess-latex-default-code-mode 'fundamental-mode
  "Default major mode for editing code chunks.
This is set to FUNDAMENTAL-MODE by default, but you might want to
change this in the Local Variables section of your file to something
more appropriate, like C-MODE, FORTRAN-MODE, or even
INDENTED-TEXT-MODE.")

(defvar ess-latex-code-mode 'c-mode
  "Major mode for editing this particular code chunk.
It defaults to ess-latex-default-code-mode, but can be reset by a comment
on the first line of the chunk containing the string
\"-*- NEWMODE -*-\" or
\"-*- NEWMODE-mode -*-\" or
\"-*- mode: NEWMODE -*- \"  or
\"-*- mode: NEWMODE-mode -*- \"
Option three is recommended, as it is the closest to standard emacs usage.")

(defvar ess-latex-default-doc-mode 'latex-mode
  "Major mode for editing documentation chunks.")

(defvar ess-latex-doc-mode-syntax-table nil
  "A syntax-table syntax table that makes quoted code in doc chunks to behave.")

(defvar ess-latex-last-chunk-index 0
  "This keeps track of the chunk we have just been in. If this is not the same as the current chunk, we have to check if we need to change major mode.")

(defvar ess-latex-chunk-vector nil
  "Vector of the chunks in this buffer.")

(defvar ess-latex-narrowing nil
  "If not NIL, the display will always be narrowed to the
current chunk pair.")

(defvar ess-latex-electric-@-and-< t
  "If not nil, the keys `@' and `<' will be bound to ESS-LATEX-ELECTRIC-@
and ESS-LATEX-ELECTRIC-<, respectively.")

(defvar ess-latex-use-mouse-navigation t
  "If not nil, enables moving between chunks using mouse-1.
Clicking on the '<<' at the beginning of a chunk name takes you to the
previous occurence of that chunk name, clicking on the '>>' takes you
to the next.
Assumes mouse-1 is bound to mouse-set-point, so if you have rebound
mouse-1, this will override your binding.")

; 
; (defvar ess-latex-weave-options "-delay")
; (defvar ess-latex-latex-viewer "xdvi")
; (defvar ess-latex-html-viewer "netscape")

; (defun ess-latex-weave (&optional name)
;   (interactive)
;   (let ((buffer (get-buffer-create "Weave Buffer")))
;     (if (not name)
;       (progn
;       ;; Assume latex documentation, but set to html if appropriate
;       (if (eq ess-latex-doc-mode html-mode)
;           (setq name (concat (substring (buffer-file-name) 0
;                                         (string-match ".nw" name))
;                              ".html"))
;         (setq name (concat (substring (buffer-file-name) 0
;                                           (string-match ".nw" name))
;                                ".tex")))))
;     (setq name (concat "> " name))
;     (setq ess-latex-weave-options (concat ess-latex-weave-options name))
;     (start-process weave-process buffer "noweave" ess-latex-weave-options)))

; (defun ess-latex-view  ())

;;; Setup
(defvar ess-latex-mode nil
  "Buffer local variable, T iff this buffer is edited in ess-latex mode.")

;; For some reason that I do not understand, `newline' does not do the
;; right thing in quoted code. If point is not preceded by whitespace,
;; it moves to the beginning of the current line, not the beginning of
;; the new line. `newline 1' works fine, hence the kludge. I'd love to
;; understand what's going on, though. Try running M-x newline in the
;; middle of a code quote in a doc chunk to see
;; what I mean: its odd.

(defun ess-latex-newline (&optional arg)
  "A kludge to get round very odd behaviour of newline in quoted code."
  (interactive "p")
  (if arg (newline arg) (newline 1)))

(defvar ess-latex-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-n" 'ess-latex-next-chunk)
    (define-key map "\C-p" 'ess-latex-previous-chunk)
    (define-key map "\M-n" 'ess-latex-goto-next)
    (define-key map "\M-m" 'ess-latex-insert-default-mode-line)
    (define-key map "\M-p" 'ess-latex-goto-previous)
    (define-key map "c" 'ess-latex-next-code-chunk)
    (define-key map "C" 'ess-latex-previous-code-chunk)
    (define-key map "d" 'ess-latex-next-doc-chunk)
    (define-key map "D" 'ess-latex-previous-doc-chunk)
    (define-key map "g" 'ess-latex-goto-chunk)
    (define-key map "\C-l" 'ess-latex-update-chunk-vector)
    (define-key map "\M-l" 'ess-latex-update-chunk-vector)
    (define-key map "w" 'ess-latex-copy-chunk-as-kill)
    (define-key map "W" 'ess-latex-copy-chunk-pair-as-kill)
    (define-key map "k" 'ess-latex-kill-chunk)
    (define-key map "K" 'ess-latex-kill-chunk-pair)
    (define-key map "m" 'ess-latex-mark-chunk)
    (define-key map "M" 'ess-latex-mark-chunk-pair)
    (define-key map "n" 'ess-latex-narrow-to-chunk)
    (define-key map "N" 'ess-latex-narrow-to-chunk-pair)
    (define-key map "t" 'ess-latex-toggle-narrowing)
    (define-key map "\t" 'ess-latex-complete-chunk)
    (define-key map "q" 'ess-latex-fill-chunk)
    (define-key map "i" 'ess-latex-new-chunk)
    (define-key map "o" 'ess-latex-occur)
    (define-key map "v" 'ess-latex-mode-version)
    (define-key map "h" 'ess-latex-describe-mode)
    (define-key map "\C-h" 'ess-latex-describe-mode)
    map)
  "ess-latex minor-mode prefix keymap")

(defvar ess-latex-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (if ess-latex-electric-@-and-<
        (progn
          (define-key map "@" 'ess-latex-electric-@)
          (define-key map "<" 'ess-latex-electric-<)))
    (define-key map "\M-q" 'ess-latex-fill-paragraph-chunk)
    (define-key map [tab] 'ess-latex-indent-line)
    (define-key map [return] 'ess-latex-newline)
    (define-key map [mouse-1] 'ess-latex-mouse-first-button)
    (define-key map ess-latex-mode-prefix ess-latex-mode-prefix-map)
    map)
  "Ess-Latex minor mode keymap")

(easy-menu-define
 ess-latex-minor-mode-menu ess-latex-minor-mode-map "Menu keymap for ess-latex."
 '("Ess-Latex"
   ("Movement"
    ["Previous chunk" ess-latex-previous-chunk t]
    ["Next chunk" ess-latex-next-chunk t]
    ["Previous chunk of same name" ess-latex-goto-previous t]
    ["Next chunk of same name" ess-latex-goto-next t]
    ["Goto chunk" ess-latex-goto-chunk t]
    ["Previous code chunk" ess-latex-previous-code-chunk t]
    ["Next code chunk" ess-latex-next-code-chunk t]
    ["Previous documentation chunk" ess-latex-previous-doc-chunk t]
    ["Next documentation chunk" ess-latex-next-doc-chunk t])
   ("Editing"
    ["Copy chunk" ess-latex-copy-chunk-as-kill t]
    ["Copy chunk pair" ess-latex-copy-chunk-pair-as-kill t]
    ["Kill chunk" ess-latex-kill-chunk t]
    ["Kill chunk pair" ess-latex-kill-chunk-pair t]
    ["Mark chunk" ess-latex-mark-chunk t]
    ["Mark chunk pair" ess-latex-mark-chunk-pair t])
   ("Narrowing"
    ["Narrow to chunk" ess-latex-narrow-to-chunk t]
    ["Narrow to chunk pair" ess-latex-narrow-to-chunk-pair t]
    ["Toggle auto narrowing" ess-latex-toggle-narrowing t]
    ["Widen" widen t])
   ("Modes"
    ["Set documentation mode" ess-latex-set-doc-mode t]
    ["Set default code mode" ess-latex-set-code-mode t]
    ["Set code mode for this chunk" ess-latex-set-this-code-mode t]
    ["Insert default mode line" ess-latex-insert-default-mode-line t])
   ("Tangling"
    ["Tangle current chunk" ess-latex-tangle-chunk t]
    ["Tangle current thread" ess-latex-tangle-current-thread t]
    ["Tangle named thread" ess-latex-tangle-thread t])
   ("Miscellaneous"
    ["Complete chunk name" ess-latex-complete-chunk t]
    ["Fill current chunk" ess-latex-fill-chunk t]
    ["Insert new chunk" ess-latex-new-chunk t]
    ["Update the chunk vector" ess-latex-update-chunk-vector t]
    ["Chunk occurrences" ess-latex-occur t])
   "--"
   ["Help" ess-latex-describe-mode t]
   ["Version" ess-latex-mode-version t]))

;; Add ess-latex-mode to the list of minor modes
(if (not (assq 'ess-latex-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(ess-latex-mode " Ess-Latex")))))
;; Add ess-latex-minor-mode-map to the list of minor-mode keymaps
;; available. Then, whenever ess-latex-mode is activated, the keymap is
;; automatically activated
(if (not (assq 'ess-latex-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'ess-latex-mode ess-latex-minor-mode-map)
                minor-mode-map-alist)))

(defun ess-latex-minor-mode (&optional arg)
  "Minor meta mode for editing ess-latex files. See ESS-LATEX-MODE."
  (interactive)
  (ess-latex-minor-mode arg))

(defun ess-latex-mode ( &optional arg )
  "Minor meta mode for editing ess-latex files.
`Meta' refers to the fact that this minor mode is switching major
modes depending on the location of point.

The following special keystrokes are available in ess-latex mode:

Movement:
\\[ess-latex-next-chunk] \tgoto the next chunk
\\[ess-latex-previous-chunk] \tgoto the previous chunk
\\[ess-latex-goto-previous] \tgoto the previous chunk of the same name
\\[ess-latex-goto-next] \tgoto the next chunk of the same name
\\[ess-latex-goto-chunk] \t\tgoto a chunk
\\[ess-latex-next-code-chunk] \t\tgoto the next code chunk
\\[ess-latex-previous-code-chunk] \t\tgoto the previous code chunk
\\[ess-latex-next-doc-chunk] \t\tgoto the next documentation chunk
\\[ess-latex-previous-doc-chunk] \t\tgoto the previous documentation chunk

Copying/Killing/Marking/Narrowing:
\\[ess-latex-copy-chunk-as-kill] \t\tcopy the chunk the point is in into the kill ring
\\[ess-latex-copy-chunk-pair-as-kill] \t\tcopy the pair of doc/code chunks the point is in
\\[ess-latex-kill-chunk] \t\tkill the chunk the point is in
\\[ess-latex-kill-chunk-pair] \t\tkill the pair of doc/code chunks the point is in
\\[ess-latex-mark-chunk] \t\tmark the chunk the point is in
\\[ess-latex-mark-chunk-pair] \t\tmark the pair of doc/code chunks the point is in
\\[ess-latex-narrow-to-chunk] \t\tnarrow to the chunk the point is in
\\[ess-latex-narrow-to-chunk-pair] \t\tnarrow to the pair of doc/code chunks the point is in
\\[widen] \twiden
\\[ess-latex-toggle-narrowing] \t\ttoggle auto narrowing

Filling and Indenting:
\\[ess-latex-fill-chunk] \tfill (or indent) the chunk at point according to mode
\\[ess-latex-fill-paragraph-chunk] \tfill the paragraph at point, restricted to chunk
\\[ess-latex-indent-line] \tindent the line at point according to mode

Insertion:
\\[ess-latex-insert-default-mode-line] \tinsert a line to set this file's code mode
\\[ess-latex-new-chunk] \t\tinsert a new chunk at point
\\[ess-latex-complete-chunk] \tcomplete the chunk name before point
\\[ess-latex-electric-@] \t\tinsert a `@' or start a new doc chunk
\\[ess-latex-electric-<] \t\tinsert a `<' or start a new code chunk

Modes:
\\[ess-latex-set-doc-mode] \t\tset the major mode for editing doc chunks
\\[ess-latex-set-code-mode] \tset the major mode for editing code chunks
\\[ess-latex-set-this-code-mode] \tset the major mode for editing this code chunk

Misc:
\\[ess-latex-occur] \t\tfind all occurrences of the current chunk
\\[ess-latex-update-chunk-vector] \tupdate the markers for chunks
\\[ess-latex-describe-mode] \tdescribe ess-latex-mode
\\[ess-latex-mode-version] \t\tshow ess-latex-mode's version in the minibuffer
"  (interactive "P")
; This bit is tricky: copied almost verbatim from bib-cite-mode.el
; It seems to ensure that the variable ess-latex-mode is made
; local to this buffer. It then sets ess-latex-mode to `t' if
;     1) It was called with an argument greater than 0
; or  2) It was called with no argument, and ess-latex-mode is
;        currently nil
; ess-latex-mode is nil if the argument was <= 0 or there
; was no argument and ess-latex-mode is currently `t'
  (set (make-local-variable 'ess-latex-mode)
       (if arg
           (> (prefix-numeric-value arg) 0)
         (not ess-latex-mode)))
; Now, if ess-latex-mode is true, we want to turn
; ess-latex-mode on
  (cond
   (ess-latex-mode                 ;Setup the minor-mode
    (mapcar 'ess-latex-make-variable-permanent-local
            '(ess-latex-mode
              after-change-functions
              ess-latex-narrowing
              ess-latex-chunk-vector
              post-command-hook
              isearch-mode-hook
              isearch-mode-end-hook
              ess-latex-doc-mode
              ess-latex-code-mode
              ess-latex-default-code-mode
              ess-latex-last-chunk-index))
    (ess-latex-update-chunk-vector)
    (if (equal 0 (ess-latex-find-chunk-index-buffer))
        (setq ess-latex-last-chunk-index 1)
      (setq ess-latex-last-chunk-index 0))
    (if font-lock-mode
        (progn
          (font-lock-mode -1)
          (ess-latex-font-lock-mode 1)))
    (add-hook 'post-command-hook 'ess-latex-post-command-function)
    (add-hook 'after-change-functions 'ess-latex-after-change-function)
    (add-hook 'ess-latex-select-doc-mode-hook 'ess-latex-auto-fill-doc-mode)
    (add-hook 'ess-latex-select-code-mode-hook 'ess-latex-auto-fill-code-mode)
    (add-hook 'isearch-mode-hook 'ess-latex-note-isearch-mode)
    (add-hook 'isearch-mode-end-hook 'ess-latex-note-isearch-mode-end)
    (setq ess-latex-doc-mode-syntax-table nil)
    (run-hooks 'ess-latex-mode-hook)
    (message
     "ess-latex mode: use `M-x ess-latex-describe-mode' for further information"))
   ;; If we didn't do the above, then we want to turn ess-latex-mode
   ;; off, no matter what (hence the condition `t')
   (t
    (remove-hook 'post-command-hook 'ess-latex-post-command-function)
    (remove-hook 'after-change-functions 'ess-latex-after-change-function)
    (remove-hook 'ess-latex-select-doc-mode-hook 'ess-latex-auto-fill-doc-mode)
    (remove-hook 'ess-latex-select-code-mode-hook 'ess-latex-auto-fill-code-mode)
    (remove-hook 'isearch-mode-hook 'ess-latex-note-isearch-mode)
    (remove-hook 'isearch-mode-end-hook 'ess-latex-note-isearch-mode-end)
    (if ess-latex-font-lock-mode
        (progn
          (ess-latex-font-lock-mode -1)
          (message "Ess-Latex and Ess-Latex-Font-Lock Modes Removed"))
      (message "Ess-Latex mode removed")))))

(defun ess-latex-make-variable-permanent-local (var)
  "Declare VAR buffer local, but protect it from beeing killed
by major mode changes."
  (make-variable-buffer-local var)
  (put var 'permanent-local 't))

(defun ess-latex-note-isearch-mode ()
  "Take note of an incremental search in progress"
  (remove-hook 'post-command-hook 'ess-latex-post-command-function))

(defun ess-latex-note-isearch-mode-end ()
  "Take note of an incremental search having ended"
  (add-hook 'post-command-hook 'ess-latex-post-command-function))

(defun ess-latex-post-command-function ()
  "The hook being run after each command in ess-latex mode."
  (ess-latex-select-mode))

(defun ess-latex-after-change-function (begin end length)
  "Function to run after every change in a ess-latex buffer.
If the changed region contains a chunk start (^@ or ^<<), it will
update the chunk vector"
  (save-excursion
    (goto-char begin)
    (if (re-search-forward "^\\(@[^@]\\)\\|\\(<<\\)" end t)
      (ess-latex-update-chunk-vector))))


;;; Chunks

(defun ess-latex-update-chunk-vector ()
  "Scan the whole buffer and place a marker at each \"^@\" and \"^<<\".
Record them in ESS-LATEX-CHUNK-VECTOR."
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
            (next-line 1)
            (while (looking-at "@ %def")
              (next-line 1))
            (setq chunk-list
                  ;; Now we can tell code vs docs
                  (cons (cons (if (looking-at "<<\\(.*\\)>>=")
                                  (buffer-substring-no-properties
                                   (match-beginning 1) (match-end 1))
                                'doc)
                              (point-marker))
                        chunk-list))))
        (next-line 1))
      (setq chunk-list (cons (cons 'doc (point-max-marker)) chunk-list))
      (setq ess-latex-chunk-vector (vconcat (reverse chunk-list))))))

(defun ess-latex-find-chunk ()
  "Return a pair consisting of the name (or 'DOC) and the
marker of the current chunk."
  (if (not ess-latex-chunk-vector)
      (ess-latex-update-chunk-vector))
  (aref ess-latex-chunk-vector (ess-latex-find-chunk-index-buffer)))

(defun ess-latex-chunk-is-code (index)
  "Return t if the chunk 'index' is a code chunk, nil otherwise"
  (interactive)
  (stringp (car (ess-latex-chunk-vector-aref index))))

(defun ess-latex-in-code-chunk ()
  "Return t if we are in a code chunk, nil otherwise."
  (interactive)
  (ess-latex-chunk-is-code (ess-latex-find-chunk-index-buffer)))

(defun ess-latex-in-mode-line ()
  "Return the name of the mode to use if we are in a mode line, nil
otherwise."
  (interactive)
  (let (beg end mode)
    (save-excursion
      (beginning-of-line 1)
      (and (search-forward "-*-"
                           (save-excursion (end-of-line) (point))
                           t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (setq mode (concat
                         (downcase (buffer-substring beg end))
                         "-mode"))
             (if ;(and 
		 (>= (length mode) 11)
                      (progn
                        (if
                            (equal (substring mode -10 -5) "-mode")
                            (setq mode (substring mode 0 -5)))
                        (if
                            (equal (substring mode 0 5) "mode:")
                            (setq mode (substring mode 6)))))
             (intern mode))))))

(defun ess-latex-find-chunk-index-buffer ()
  "Return the index of the current chunk in ESS-LATEX-CHUNK-VECTOR."
  (ess-latex-find-chunk-index 0 (1- (length ess-latex-chunk-vector))))

(defun ess-latex-find-chunk-index (low hi)
  (if (= hi (1+ low))
      low
    (let ((med (/ (+ low hi) 2)))
      (if (< (point) (cdr (aref ess-latex-chunk-vector med)))
          (ess-latex-find-chunk-index low med)
        (ess-latex-find-chunk-index med hi)))))

(defun ess-latex-chunk-region ()
  "Return a pair consisting of the beginning and end of the current chunk."
  (interactive)
  (let ((start (ess-latex-find-chunk-index-buffer)))
    (cons (marker-position (cdr (aref ess-latex-chunk-vector start)))
          (marker-position (cdr (aref ess-latex-chunk-vector (1+ start)))))))

(defun ess-latex-copy-code-chunk ()
  "Copy the current code chunk to the kill ring, excluding the chunk name.
This will be particularly useful when interfacing with ESS."
  (interactive)
  (let ((r (ess-latex-chunk-region)))
    (save-excursion
      (goto-char (car r))
      (if (ess-latex-in-code-chunk)
          (progn
            (beginning-of-line 2)
            (copy-region-as-kill (point) (cdr r)))))))

(defun ess-latex-extract-code-chunk ()
  "Create a new buffer with the same name as the current code chunk,
and copy all code  from chunks of the same name to it."
  (interactive)
  (save-excursion
    (if (ess-latex-in-code-chunk)
        (progn
          (let ((chunk-name (car (ess-latex-find-chunk)))
                (chunk-counter 0)
                (copy-counter 0)
                (this-chunk) (oldbuf (current-buffer)))
            (if (get-buffer chunk-name)
                (progn
                  (set-buffer-modified-p nil)
                  (kill-buffer chunk-name)))
            (get-buffer-create chunk-name)
            (message "Created buffer %s" chunk-name)
            (while (< chunk-counter (- (length ess-latex-chunk-vector) 2))
              (setq this-chunk (ess-latex-chunk-vector-aref
                                chunk-counter))
              (message "Current buffer is %s" (car this-chunk))
              (if (equal chunk-name (car this-chunk))
                  (progn
                    (setq copy-counter (+ copy-counter 1))
                    (goto-char (cdr this-chunk))
                    (ess-latex-copy-code-chunk)
                    (set-buffer chunk-name)
                    (goto-char (point-max))
                    (yank)
                    (set-buffer oldbuf)))
              (setq chunk-counter (+ chunk-counter 1)))
            (message "Copied %d bits" copy-counter)
            (set-buffer chunk-name)
            (copy-region-as-kill (point-min)(point-max)))))))

(defun ess-latex-chunk-pair-region ()
  "Return a pair consisting of the beginning and end of the current pair of
documentation and code chunks."
  (interactive)
  (let* ((start (ess-latex-find-chunk-index-buffer))
         (end (1+ start)))
    (if (ess-latex-chunk-is-code start)
        (cons (marker-position (cdr (aref ess-latex-chunk-vector (1- start))))
              (marker-position (cdr (aref ess-latex-chunk-vector end))))
      (while (not (ess-latex-chunk-is-code  end))
        (setq end (1+ end)))
      (cons (marker-position (cdr (aref ess-latex-chunk-vector start)))
            (marker-position (cdr (aref ess-latex-chunk-vector (1+ end))))))))

(defun ess-latex-chunk-vector-aref (i)
  (if (< i 0)
      (error "Before first chunk."))
  (if (>= i (length ess-latex-chunk-vector))
      (error "Beyond last chunk."))
  (aref ess-latex-chunk-vector i))

(defun ess-latex-complete-chunk ()
  "Complete the chunk name before point, if any."
  (interactive)
  (if (ess-latex-in-code-chunk)
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
                   (alist (ess-latex-build-chunk-alist))
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

(defun ess-latex-hide-code-quotes ()
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
                  ;; useless.  See ESS-LATEX-RESTORE-CODE-QUOTES.
                  (save-excursion
                    (setq quote-list (cons (cons (copy-marker e)
                                                 (buffer-substring b e))
                                           quote-list))
                    (goto-char b)
                    (insert-char ?* (- e b) t)
                    (delete-char (- e b))))))))
      (reverse quote-list))))

(defun ess-latex-restore-code-quotes (quote-list)
  "Reinsert the strings modified by `ess-latex-hide-code-quotes'."
  (save-excursion
    (mapcar '(lambda (q)
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

(defun ess-latex-fill-chunk ()
  "Fill the current chunk according to mode.
Run `fill-region' on documentation chunks and `indent-region' on code
chunks."
  (interactive)
  (save-excursion
    (save-restriction
      (ess-latex-narrow-to-chunk)
      (if (ess-latex-in-code-chunk)
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
        (let ((quote-list (ess-latex-hide-code-quotes)))
          (fill-region (point-min) (point-max))
          (ess-latex-restore-code-quotes quote-list))))))

(defun ess-latex-indent-line ()
  "Indent the current line according to mode, after narrowing to this chunk."
  (interactive)
  (save-restriction
    (ess-latex-narrow-to-chunk)
    (if (stringp (car (ess-latex-find-chunk)))
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

(defun ess-latex-fill-paragraph-chunk (&optional justify)
  "Fill a paragraph in the current chunk."
  (interactive "P")
  (ess-latex-update-chunk-vector)
  (save-restriction
    (save-excursion
      (ess-latex-narrow-to-chunk)
      (if (stringp (car (ess-latex-find-chunk)))
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
        (let ((quote-list (ess-latex-hide-code-quotes)))
          (fill-paragraph justify)
          (ess-latex-restore-code-quotes quote-list))))))

(defun ess-latex-auto-fill-doc-chunk ()
  "Replacement for `do-auto-fill'."
  (save-restriction
    (narrow-to-region (car (ess-latex-chunk-region))
                      (save-excursion
                        (end-of-line)
                        (point)))
    (let ((quote-list (ess-latex-hide-code-quotes)))
      (do-auto-fill)
      (ess-latex-restore-code-quotes quote-list))))

(defun ess-latex-auto-fill-doc-mode ()
  "Install the improved auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'ess-latex-auto-fill-doc-chunk)))

(defun ess-latex-auto-fill-code-mode ()
  "Install the default auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'do-auto-fill)))

;;; Marking

(defun ess-latex-mark-chunk ()
  "Mark the current chunk."
  (interactive)
  (let ((r (ess-latex-chunk-region)))
    (goto-char (car r))
    (push-mark (cdr r) nil t)))

(defun ess-latex-mark-chunk-pair ()
  "Mark the current pair of documentation and code chunks."
  (interactive)
  (let ((r (ess-latex-chunk-pair-region)))
    (goto-char (car r))
    (push-mark (cdr r) nil t)))


;;; Narrowing

(defun ess-latex-toggle-narrowing (&optional arg)
  "Toggle if we should narrow the display to the current pair of
documentation and code chunks after each movement.  With argument:
switch narrowing on."
  (interactive "P")
  (if (or arg (not ess-latex-narrowing))
      (progn
        (setq ess-latex-narrowing t)
        (ess-latex-narrow-to-chunk-pair))
    (setq ess-latex-narrowing nil)
    (widen)))

(defun ess-latex-narrow-to-chunk ()
  "Narrow the display to the current chunk."
  (interactive)
  (let ((r (ess-latex-chunk-region)))
    (narrow-to-region (car r) (cdr r))))

(defun ess-latex-narrow-to-chunk-pair ()
  "Narrow the display to the current pair of documentation and code chunks."
  (interactive)
  (let ((r (ess-latex-chunk-pair-region)))
    (narrow-to-region (car r) (cdr r))))


;;; Killing

(defun ess-latex-kill-chunk ()
  "Kill the current chunk."
  (interactive)
  (let ((r (ess-latex-chunk-region)))
    (kill-region (car r) (cdr r))))

(defun ess-latex-kill-chunk-pair ()
  "Kill the current pair of chunks."
  (interactive)
  (let ((r (ess-latex-chunk-pair-region)))
    (kill-region (car r) (cdr r))))

(defun ess-latex-copy-chunk-as-kill ()
  "Place the current chunk on the kill ring."
  (interactive)
  (let ((r (ess-latex-chunk-region)))
    (copy-region-as-kill (car r) (cdr r))))

(defun ess-latex-copy-chunk-pair-as-kill ()
  "Place the current pair of chunks on the kill ring."
  (interactive)
  (let ((r (ess-latex-chunk-pair-region)))
    (copy-region-as-kill (car r) (cdr r))))


;;; Movement

(defun ess-latex-sign (n)
  "Return the sign of N."
  (if (< n 0) -1 1))

(defun ess-latex-next-doc-chunk (&optional cnt)
  "Goto to the Nth documentation chunk from point."
  (interactive "p")
  (widen)
  (let ((start (ess-latex-find-chunk-index-buffer))
        (i 1))
    (while (<= i (abs cnt))
      (setq start (+ (ess-latex-sign cnt) start))
      (while (ess-latex-chunk-is-code start)
        (setq start (+ (ess-latex-sign cnt) start)))
      (setq i (1+ i)))
    (goto-char (marker-position (cdr (ess-latex-chunk-vector-aref start))))
    (forward-char 1))
  (if ess-latex-narrowing
      (ess-latex-narrow-to-chunk-pair)))

(defun ess-latex-previous-doc-chunk (&optional n)
  "Goto to the -Nth documentation chunk from point."
  (interactive "p")
  (ess-latex-next-doc-chunk (- n)))

(defun ess-latex-next-code-chunk (&optional cnt)
  "Goto to the Nth code chunk from point."
  (interactive "p")
  (widen)
  (let ((start (ess-latex-find-chunk-index-buffer))
        (i 1))
    (while (<= i (abs cnt))
      (setq start (+ (ess-latex-sign cnt) start))
      (while (not (ess-latex-chunk-is-code start))
        (setq start (+ (ess-latex-sign cnt) start)))
      (setq i (1+ i)))
    (goto-char (marker-position (cdr (ess-latex-chunk-vector-aref start))))
    (next-line 1))
  (if ess-latex-narrowing
      (ess-latex-narrow-to-chunk-pair)))

(defun ess-latex-previous-code-chunk (&optional n)
  "Goto to the -Nth code chunk from point."
  (interactive "p")
  (ess-latex-next-code-chunk (- n)))

(defun ess-latex-next-chunk (&optional n)
  "If in a documentation chunk, goto to the Nth documentation
chunk from point, else goto to the Nth code chunk from point."
  (interactive "p")
  (if (ess-latex-in-code-chunk)
      (ess-latex-next-code-chunk n)
    (ess-latex-next-doc-chunk n)))

(defun ess-latex-previous-chunk (&optional n)
  "If in a documentation chunk, goto to the -Nth documentation
chunk from point, else goto to the -Nth code chunk from point."
  (interactive "p")
  (ess-latex-next-chunk (- n)))

(defvar ess-latex-chunk-history nil
  "")

(defun ess-latex-goto-chunk ()
  "Goto the named chunk."
  (interactive)
  (widen)
  (let* ((completion-ignore-case t)
         (alist (ess-latex-build-chunk-alist))
         (chunk (completing-read
                 "Chunk: " alist nil t
                 (ess-latex-goto-chunk-default)
                 ess-latex-chunk-history)))
    (goto-char (cdr (assoc chunk alist))))
  (if ess-latex-narrowing
      (ess-latex-narrow-to-chunk-pair)))

(defun ess-latex-goto-chunk-default ()
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

(defun ess-latex-build-chunk-alist ()
  (if (not ess-latex-chunk-vector)
      (ess-latex-update-chunk-vector))
  ;; The naive recursive solution will exceed MAX-LISP-EVAL-DEPTH in
  ;; buffers w/ many chunks.  Maybe there is a tail recursivce solution,
  ;; but iterative solutions should be acceptable for dealing with vectors.
  (let ((alist nil)
        (i (1- (length ess-latex-chunk-vector))))
    (while (>= i 0)
      (let* ((chunk (aref ess-latex-chunk-vector i))
             (name (car chunk))
             (marker (cdr chunk)))
        (if (and (stringp name)
                 (not (assoc name alist)))
            (setq alist (cons (cons name marker) alist))))
      (setq i (1- i)))
    alist))

(defun ess-latex-goto-next (&optional cnt)
  "Goto the continuation of the current chunk."
  (interactive "p")
  (widen)
  (if (not ess-latex-chunk-vector)
      (ess-latex-update-chunk-vector))
  (let ((start (ess-latex-find-chunk-index-buffer)))
    (if (not (ess-latex-chunk-is-code  start))
        (setq start (1+ start)))
    (if (ess-latex-chunk-is-code start)
        (let ((name (car (ess-latex-chunk-vector-aref start)))
              (i 1))
          (while (<= i (abs cnt))
            (setq start (+ (ess-latex-sign cnt) start))
            (while (not (equal (car (ess-latex-chunk-vector-aref start))
                               name))
              (setq start (+ (ess-latex-sign cnt) start)))
            (setq i (1+ i)))
          (goto-char (marker-position
                      (cdr (ess-latex-chunk-vector-aref start))))
          (next-line 1))))
  (if ess-latex-narrowing
      (ess-latex-narrow-to-chunk-pair)))

(defun ess-latex-goto-previous (&optional cnt)
  "Goto the previous chunk."
  (interactive "p")
  (ess-latex-goto-next (- cnt)))

(defun ess-latex-occur (arg)
  "Find all occurences of the current chunk.
This function simply runs OCCUR on \"<<NAME>>\"."
  (interactive "P")
  (let ((n (if (and arg
                    (numberp arg))
               arg
             0))
        (idx (ess-latex-find-chunk-index-buffer)))
    (if (ess-latex-chunk-is-code idx)
        (occur (regexp-quote (concat "<<"
                                     (car (aref ess-latex-chunk-vector idx))
                                     ">>"))
               n)
      (setq idx (1+ idx))
      (while (not (ess-latex-chunk-is-code idx))
        (setq idx (1+ idx)))
      (occur (regexp-quote (concat "<<"
                                   (car (aref ess-latex-chunk-vector idx))
                                   ">>"))
                           n))))


;;; Insertion

(defun ess-latex-new-chunk (name)
  "Insert a new chunk."
  (interactive "sChunk name: ")
  (insert "@ \n<<" name ">>=\n")
  (save-excursion
    (insert "@ %def \n"))
  (ess-latex-update-chunk-vector))

(defun ess-latex-at-beginning-of-line ()
  (equal (save-excursion
           (beginning-of-line)
           (point))
         (point)))

(defun ess-latex-electric-@ (arg)
  "Smart incarnation of `@', starting a new documentation chunk, maybe.
If given an numerical argument, it will act just like the dumb `@'.
Otherwise and if at the beginning of a line in a code chunk:
insert \"@ \" and update the chunk vector."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (ess-latex-at-beginning-of-line)
             (ess-latex-in-code-chunk))
        (progn
          (insert "@ ")
          (ess-latex-update-chunk-vector))
      (self-insert-command 1))))

(defun ess-latex-electric-< (arg)
  "Smart incarnation of `<', starting a new code chunk, maybe.
If given an numerical argument, it will act just like the dumb `<'.
Otherwise and if at the beginning of a line in a documentation chunk:
insert \"<<>>=\" and a newline if necessary.  Leave point in the middle
and and update the chunk vector."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (ess-latex-at-beginning-of-line)
             (not (stringp (car (ess-latex-find-chunk)))))
        (progn
          (insert "<<")
          (save-excursion
            (insert ">>=")
            (if (not (looking-at "\\s *$"))
                (newline)))
          (ess-latex-update-chunk-vector))
      (self-insert-command 1))))


;;; Modes

(defun ess-latex-set-chunk-code-mode ()
  "Set the ess-latex-code-mode for the current chunk"
  (interactive)
  (if (ess-latex-in-code-chunk)
      (progn
        ;; Reset code-mode to default and then check for a mode comment.
        (setq ess-latex-code-mode ess-latex-default-code-mode)
        (let (mode chunk-name)
          (save-restriction
            (save-excursion
              (end-of-line)
              (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
              (setq chunk-name (match-string 1))
              (widen)
              (goto-char (point-min))
              (re-search-forward (concat "^<<" chunk-name ">>=") nil t)
              (beginning-of-line 2)
              (setq mode (ess-latex-in-mode-line))
              (if (functionp mode)
                  (setq ess-latex-code-mode mode))))))
    (error "This only makes sense in a code chunk")))

(defun ess-latex-set-doc-syntax-table ()
  "Sets the doc-mode syntax-table to treat code quotes as comments."
  (interactive)
  (let ((square-bracket-string (char-to-string (char-syntax ?\[))))
    (if (string= square-bracket-string "(")
        (progn
          (modify-syntax-entry ?\[ "(]12b" ess-latex-doc-mode-syntax-table)
          (modify-syntax-entry ?\] ")[34b" ess-latex-doc-mode-syntax-table))
      (progn
        (modify-syntax-entry  ?\[
                              (concat square-bracket-string " 12b")
                              ess-latex-doc-mode-syntax-table)
        (modify-syntax-entry  ?\]
                              (concat square-bracket-string " 34b")
                              ess-latex-doc-mode-syntax-table)))))

(defun ess-latex-select-mode ()
  "Select ESS-LATEX-DOC-MODE or ESS-LATEX-CODE-MODE, as appropriate."
  (interactive)
  (let ((this-chunk-index (ess-latex-find-chunk-index-buffer)))
    ;; Has the last change to the buffer taken us into a different
    ;; chunk ?
    (if (not (equal this-chunk-index ess-latex-last-chunk-index))
        (progn
          (setq ess-latex-last-chunk-index this-chunk-index)
          (if (ess-latex-in-code-chunk)
              ;; Inside a code chunk
              (progn
                ;; Find out which code mode to use
                (ess-latex-set-chunk-code-mode)
                ;; If we aren't already using it, use it.
                (if (not (equal major-mode ess-latex-code-mode))
                    (progn
                      (funcall ess-latex-code-mode)
                      (run-hooks 'ess-latex-select-mode-hook)
                      (run-hooks 'ess-latex-select-code-mode-hook))))
            ;; Inside a documentation chunk
            (progn
              (if (not (equal major-mode ess-latex-doc-mode))
                  (progn
                    (funcall ess-latex-doc-mode)))
              (if (not ess-latex-doc-mode-syntax-table)
                  (progn
                    (message "Setting up syntax table")
                    (setq ess-latex-doc-mode-syntax-table
                          (make-syntax-table (syntax-table)))
                    (ess-latex-set-doc-syntax-table)))
              (set-syntax-table ess-latex-doc-mode-syntax-table)
              (run-hooks 'ess-latex-select-mode-hook)
              (run-hooks 'ess-latex-select-doc-mode-hook)))
          (run-hooks 'ess-latex-changed-chunk-hook)))))

(defvar ess-latex-doc-mode ess-latex-default-doc-mode
  "Default major mode for editing ess-latex documentation chunks.
It is not possible to have more than one doc-mode in a file.
However, this variable is used to determine whether the doc-mode needs
to by added to the mode-line")

(defun ess-latex-set-doc-mode (mode)
  "Change the major mode for editing documentation chunks."
  (interactive "CNew major mode for documentation chunks: ")
  (setq ess-latex-doc-mode mode)
  (setq ess-latex-doc-mode-syntax-table nil)
  ;;Pretend we've changed chunk, so the mode will be reset if necessary
  (setq ess-latex-last-chunk-index (1- ess-latex-last-chunk-index))
  (ess-latex-select-mode))

(defun ess-latex-set-code-mode (mode)
  "Change the major mode for editing all code chunks."
  (interactive "CNew major mode for all code chunks: ")
  (setq ess-latex-default-code-mode mode)
  ;;Pretend we've changed chunk, so the mode will be reset if necessary
  (setq ess-latex-last-chunk-index (1- ess-latex-last-chunk-index))
  (ess-latex-select-mode))

(defun ess-latex-set-this-code-mode (mode)
  "Change the major mode for editing this code chunk.
The only sensible way to do this is to add a mode line to the chunk"
  (interactive "CNew major mode for this code chunk: ")
  (if (ess-latex-in-code-chunk)
      (progn
        (setq ess-latex-code-mode mode)
        (save-restriction
          (save-excursion
            (let (chunk-name)
              (widen)
              (end-of-line)
              (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
              (setq chunk-name (match-string 1))
              (goto-char (point-min))
              (re-search-forward (concat "^<<" chunk-name ">>=") nil t)
              (beginning-of-line 2))
            ;; remove mode-line, if there is one
            (if (ess-latex-in-mode-line)
                (progn
                  (kill-line)
                  (kill-line)))
            (if (not (equal ess-latex-code-mode ess-latex-default-code-mode))
                (progn
                  (setq mode (substring (symbol-name mode) 0 -5))
                  ;; Need to set major mode so that we can comment out
                  ;; the mode line
                  (funcall ess-latex-code-mode)
                  (insert comment-start
                          " -*- " mode
                          " -*- " comment-end "\n")))
            (setq ess-latex-last-chunk-index (1- ess-latex-last-chunk-index)))))
    (message "This only makes sense in a code chunk.")))

;;; Misc

(defun ess-latex-mode-version ()
  "Echo the RCS identification of ess-latex mode."
  (interactive)
  (message "Thorsten's ess-latex-mode (PRERELEASE). RCS: %s"
           ess-latex-mode-RCS-Id))

(defun ess-latex-describe-mode ()
  "Describe ess-latex mode."
  (interactive)
  (describe-function 'ess-latex-mode))

(defun ess-latex-insert-default-mode-line ()
  "Insert line that will set the ess-latex mode of this file in emacs.
The file is set to use the current doc and default-code modes, so
ensure they are set correctly (with ess-latex-set-code-mode and
ess-latex-set-doc-mode) before calling this function"
  (interactive)
  (save-excursion
    (goto-char 1)
    (if (ess-latex-in-mode-line)
        (progn
          (kill-line)
          (kill-line)))
    (if (not (eq major-mode ess-latex-doc-mode))
        (ess-latex-select-mode))
    (insert comment-start " -*- mode: ess-latex; ess-latex-default-code-mode: "
            (symbol-name ess-latex-default-code-mode)
            (if (not (eq ess-latex-doc-mode ess-latex-default-doc-mode))
                (concat "; ess-latex-doc-mode: " (symbol-name
                                              ess-latex-doc-mode) ";")
              ";")
            " -*-" comment-end "\n"))
  (ess-latex-select-mode))

(defun ess-latex-mouse-first-button (event)
  (interactive "e")
  (mouse-set-point event)
  (if (and ess-latex-use-mouse-navigation
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
              (re-search-backward (concat "<<" chunk-name ">>") nil t))
          (if (and (<= (match-end 2) (point))
                   (>  (+ 2 (match-end 2)) (point)))
              (let ((chunk-name (buffer-substring-no-properties
                                 (match-beginning 2)
                                 (match-end 2))))
                (re-search-forward (concat "<<" chunk-name ">>") nil t)))))))


;;; Debugging

(defun ess-latex-log (s)
  (let ((b (current-buffer)))
    (switch-to-buffer (get-buffer-create "*ess-latex-log*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert s)
    (setq buffer-read-only t)
    (switch-to-buffer b)))





(defvar ess-latex-thread-alist nil
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

(defun ess-latex-update-thread-alist ()
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

      (setq ess-latex-thread-alist new-thread-alist))))


; Option setting functions to go here

(defun ess-latex-set-thread-line-format ())

(defun ess-latex-set-thread-tabs ())


(defvar ess-latex-default-line-number-format nil
  "The format string to use to  define line numbers in this thread.
If nil, do  not use line numbers.")

(defvar ess-latex-default-line-number-skip-lines 0
  "The number of initial lines to output before the line number.
This may be useful in shell scripts, where the first line (or two) must have a
  specific form.")

(defvar ess-latex-default-tab-width 8
  "If a number, convert tabs to  that number of spaces in the output. If nil, let tabs through to the output unaltered.")

(defvar ess-latex-line-number-format  ess-latex-default-line-number-format
  "The format string to use to  define line numbers in this thread.
If nil, do  not use line numbers.")

(defvar ess-latex-line-number-skip-lines ess-latex-default-line-number-skip-lines
  "The number of initial lines to output before the line number.
This may be useful in shell scripts, where the first line (or two) must have a
  specific form.")

(defvar ess-latex-tab-width  ess-latex-default-tab-width
  "If a number, convert tabs to  that number of spaces in the output. If nil, let tabs through to the output unaltered.")

(defun ess-latex-get-thread-local-variables ()
  "Get the values of the variables that are local to a thread."
  (interactive)
  (save-restriction
    (save-excursion
      (end-of-line)
      (re-search-backward "^[ \t]*<<\\(.*\\)>>=" nil t)
      (let ((chunk-name (match-string 1)))
        (widen)
        (goto-char (point-min))
        (re-search-forward (concat "^<<" chunk-name ">>=") nil t)
        (beginning-of-line 2)
        (while (looking-at ".*-\*-.*-\*-")
          (let ((this-line (buffer-substring-no-properties
                            (point)
                            (progn (end-of-line) (point)))))
            (if (string-match
                 "mode:[ \t]*\\([^\t ]*\\)" this-line)
                (setq ess-latex-code-mode (match-string-no-properties 1 this-line)))
            (if (string-match
                 "ess-latex-line-number-format:[ \t]*\"\\([^\"]*\\)\"" this-line)
                (setq ess-latex-line-number-format
                      (match-string-no-properties 1 this-line)))
            (if (string-match
                 "ess-latex-line-number-skip-lines:[ \t]*\\([^\t ]*\\)" this-line)
                (setq ess-latex-line-number-skip-lines
                      (string-to-number
                       (match-string-no-properties 1 this-line))))
            (if (string-match
                 "ess-latex-tab-width:[ \t]*\\([^\t ]*\\)" this-line)
                (setq ess-latex-tab-width
                      (string-to-number
                       (match-string-no-properties 1 this-line))))
            (beginning-of-line 2)))))))

(defun ess-latex-reset-thread-local-variables ()
  "Resets the thread-local variables to their default values"
  (setq ess-latex-tab-width ess-latex-default-tab-width)
  (setq ess-latex-line-number-format ess-latex-default-line-number-format)
  (setq ess-latex-line-number-skip-lines ess-latex-default-line-number-skip-lines))

(defun ess-latex-write-line-number (line-number-format buffer)
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


(defun ess-latex-tangle-chunk ( &optional buffer prefix-string)
  "Generate the code produced by this chunk, & any threads used in this chunk."
  (interactive)
  (save-excursion
    (ess-latex-reset-thread-local-variables)
    (ess-latex-get-thread-local-variables)
    (ess-latex-update-chunk-vector)
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
         (thread-info (assoc chunk-name ess-latex-thread-alist))
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
              (while (> ess-latex-line-number-skip-lines 0)
                (append-to-buffer tangle-buffer
                                  (point)
                                  (save-excursion (progn
                                                    (end-of-line) (point))))
                (beginning-of-line 2)
                (1- ess-latex-line-number-skip-lines))
            (ess-latex-write-line-number line-number-format buffer))
          (message "Now at %d" (point))

          (while (< (point) chunk-end)
            (untabify (point) (save-excursion (beginning-of-line 2)(point)))
            ;; This RE gave me trouble. Without the `\"', it recognised itself
            ;; and so could not copy itself correctly.
            (if (looking-at "\\([^\n\"@]*\\)<<\\(.*\\)\\(>>\\)\\([^\n\"]*\\)$")
                (progn
                  (save-restriction
                    (save-excursion
                      (setq thread-name-re (concat "<<"
                                                   (match-string 2)
                                                   ">>="))
                      (setq pre-chunk (match-string 1))
                      (if prefix-string (setq pre-chunk (concat prefix-string pre-chunk)))
                      (setq post-chunk (match-string 4))
                      (widen)
                      (goto-char (point-min))
                      (while (re-search-forward thread-name-re nil t)
                        (ess-latex-tangle-chunk tangle-buffer pre-chunk)
                        (next-line 1)))
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
                    (setq pre-chunk (concat prefix-string pre-chunk)))
                ;; And copy it to the buffer
                (save-excursion
                  (set-buffer tangle-buffer)
                  (insert pre-chunk)))
            ;; If this is the first line of the chunk, we need to change
            ;; prefix-string to consist solely of spaces
            (if (and first-line
                     prefix-string)
                (progn
                  (setq prefix-string (make-string (length prefix-string) ?\  ))
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

(defun ess-latex-tangle-thread ( name &optional buffer)
  "Given the name of a thread, tangles the thread to buffer.
If no buffer is given, create a new one with the same name as the thread."
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
              (ess-latex-tangle-chunk buffer)))))))

(defun ess-latex-tangle-current-thread ( &optional buffer)
  (interactive)
  (save-excursion
    (let* ((chunk-start (progn
                          (re-search-backward "^<<\\([^>]*\\)>>=[\t ]*$"
                                              nil t)
                          (beginning-of-line 2)
                          (point)))
           (chunk-name (buffer-substring-no-properties
                        (match-end 1)
                        (match-beginning 1))))
      (ess-latex-tangle-thread chunk-name buffer))))
;menu functions


;;; Finale

(run-hooks 'ess-latex-mode-load-hook)
(provide 'ess-latex-mode)

;; Changes made by Mark Lunt (mark.lunt@mrc-bsu.cam.ac.uk) 22/03/1999

;; The possibility of having code chunks using more than one language
;; was added. This was first developed by Adnan Yaqub
;; (AYaqub@orga.com) for syntax highlighting, but even people who hate
;; highlighting may like to maintain their Makefile with their code,
;; or test-scripts with their programs, or even user documentation as
;; latex-mode code chunks.
;; This required quite a few changes to ess-latex-mode:
;; 1) A new variable `ess-latex-default-code-mode' was create to do the job
;;    `ess-latex-code-mode' used to.
;; 2) ess-latex-code-mode now contains the code-mode of the current chunk
;; 3) Each chunk can now have its own mode-line to tell emacs what
;;    mode to use to edit it. The function `ess-latex-in-mode-line'
;;    recognises such mode-lines, and the function
;;    `ess-latex-set-this-code-mode' sets the code mode for the current
;;    chunk and adds a mode-line if necessary. If several chunks have
;;    the same name, the mode-line must appear in the first chunk with
;;    that name.
;; 4) The mechanism for deciding whether to change mode was altered,
;;    since the old method assumed a single code mode. Now,
;;    `ess-latex-last-chunk-index' keeps track of which chunk we were in
;;    last. If we have moved to a different chunk, we have to check
;;    which mode we should be in, and change if necessary.

;; The keymap and menu-map handling was changed. Easymenu was used to
;; define the menu, and it the keymap was attached to the 'official'
;; minor-modes-keymaps list. This means that
;; 1) It was automatically loaded when ess-latex-mode was active and
;;    unloaded when it was inactive.
;; 2) There was no need to worry about the major mode map clobbering
;;    it , since it takes precedence over the major mode
;;    map. `ess-latex-setup-keymap' is therefore now superfluous
;; The menu was also reorganised to make it less cluttered, so there
;; would be room for adding tangling and weaving commands (one day).

;; Mouse navigation is supported, in so far as clicking mouse-1 on the
;; '<<' of a chunk name moves to the previous instance of that chunk
;; name, and clicking in the '>>' moves to the next instance. They are
;; not mouse-hightlighted, though: too much hassle for zero added
;; functionality.

;; ess-latex-doc-mode has been given its own syntax-table. It is the same
;; as the current doc-mode syntax-table, except that [[ is a comment
;; start and ]] a comment end. Fixes some ugliness in LaTeX-mode if
;; `$' or `%' appear in quoted code (or even `<<', which happens often
;; in C++).
;; (This should make ess-latex-hide-code-quotes and
;; ess-latex-restore-code-quotes unnecessary, but I have not yet removed
;; them, nor the calls to them).

;; A new function `ess-latex-indent-line' was defined and bound by default
;; to the tab key. This should indent the current line correctly in
;; whichever mode we are currently in. Previously, c-mode in
;; particular did not behave well with indentation (although
;; `ess-latex-fill-chunk' worked fine). Indentation is only accurate
;; within the chunk: it does not know the syntax at the end of the
;; previous chunk, so it does not know where to start indenting in
;; this chunk. However, provided the indentation within each chunk is correct,
;; notangle will correctly indented code.

;; (I think it would be good to separate filling and indenting,
;; though, since `indent-region' and `fill-region' have completely
;; different meanings in LaTeX-mode (and both are useful))

;; ess-latex-mode and ess-latex-minor-mode were given an optional argument, so
;; that (ess-latex-mode -1) turns it off, (ess-latex-mode 1) turns it on, and
;; (ess-latex-mode) toggles it. This is considered normal for minor modes.

;; buffer-substring changed to buffer-substring-no-properties:
;; comparisons with buffer-substring can be unreliable if highlighting
;; is used.

;; New functions `ess-latex-in-code-chunk' & `ess-latex-chunk-is-code' created
;; to replace (if (stringp (car (ess-latex-find-chunk)))) and
;; (if (stringp (car (ess-latex-chunk-vector-aref index)))).

;; `ess-latex-insert-mode-line' was renamed
;; `ess-latex-insert-default-mode-line' and modified to put the mode-line
;; at the start of the file and remove any existing mode-line.

;; a '<=' in `ess-latex-find-chunk-index' changed to '<', so we get the
;; right answer if point is on the first character in a chunk

;; The name of `ess-latex-post-command-hook' changed to
;; `ess-latex-post-command-function', since it is a function.

;; All the highlighting code moved to a separate file (ess-latex-font-lock-mode.el)

;; Menu driven tangling is in the process of being added. It can
;; currently tangle a single chunk or a series of  chunks with the
;; same name (which I refer to as a thread) into a separate
;; buffer. This buffer can then be saved to a file, sent to an
;; interpreter, whatever. I haven't tested using line-numbers as yet.
