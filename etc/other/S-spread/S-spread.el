;; spreadsheet in S, S-mode or stand-alone
;; Richard M. Heiberger
;; 1996

;; S-mode
;; Load this file from a running *S* window after starting S/Splus with M-x S
;; or
;; stand-alone
;; Load this file from the dired window in which .Data exists.


                                        ;(set-variable 'buffers-menu-max-size nil)


(defvar spread-directory (concat "/tmp/" (make-temp-name "spr"))
  "Directory in which to store ascii spreadsheet displays.")

(defvar spread-command-file (concat spread-directory "/*command*")
  "File through which S will communicate with emacs.")

(defvar spread-directory-p nil
  "predicate value non-nil when directory has been defined.")

(defun print-find-emacs nil "display spread.frame from minibuffer" (interactive)
  (spread-print-find (read-string "spread.frame: ") t))


(defun emacs-rc nil "" (interactive)
  (emacs-cell "1")
  )

(defun emacs-macro nil "" (interactive)
  (emacs-cell "2")
  )

(defun emacs-macro-control-text nil "" (interactive)
  (emacs-cell "4")
  )

(defun emacs-macro-print-text nil "" (interactive)
  (emacs-cell "5")
  )

(defun emacs-cell (result-type) "" (interactive)
  (setq spread-name (buffer-name))
  (setq r (count-lines 1 (point)))
  (setq c (current-column))
  (set-buffer S-buffer)
  (spread-insert
   (format "emacs.cell('%s', %s, %s, %s)"
           spread-name r c result-type
           )
   )
  (save-excursion
    (set-buffer "*command*")
    (revert-t-t)
    (goto-char (point-min))
    (setq beg (point)) (end-of-line)
    (if (equal result-type "4")
        (progn
          (setq command (buffer-substring beg (point)))
          (set-buffer S-buffer)
          (spread-insert command)
          )
      (setq command (read-string "> " (buffer-substring beg (point))))
      (set-buffer S-buffer)
      (spread-insert command)
      (spread-insert "invisible(assign(.Active, x))")
      (spread-print-find spread-name nil)
      (goto-line r)(forward-char c)
      ))
  )

(defun spread-insert (spread-command) "" (interactive)
  (goto-char (point-max))
  (insert spread-command)
  (comint-send-input)
  (accept-process-output spread-process)
  )

(defun revert-t-t nil "revert-buffer with no questions asked"
  (interactive)
  (revert-buffer t t)
  )

(defun revert-t-t-read-only nil "revert-buffer, no questions, read-only"
  (interactive)
  (revert-buffer t t)
  (setq buffer-read-only t)
  )


(defvar spread-mode-map nil "Keymap for Spread mode.")
(if spread-mode-map
    nil
  (setq spread-mode-map (make-sparse-keymap))
  (define-key spread-mode-map "\C-cv" 'revert-t-t-read-only)
  (define-key spread-mode-map "\C-m" 'emacs-rc)
  (define-key spread-mode-map "\C-cc" 'emacs-macro)
  (define-key spread-mode-map "\C-cs" 'emacs-macro-control-text)
  (define-key spread-mode-map "\C-cp" 'emacs-macro-print-text)
  (define-key spread-mode-map "f" 'emacs-print-find-emacs)

  (define-key spread-mode-map [mouse-2] 'spread-mouse-print-find-emacs)
  (define-key spread-mode-map [mouse-3] 'spread-mouse-rc)
  )

(defun spread-mouse-rc (event) "move point then enter"
  (interactive "e")
  (mouse-set-point event)
  (emacs-rc)
  )

(defun spread-mouse-print-find-emacs (event) "move point then find file"
  (interactive "e")
  (mouse-set-point event)
  (emacs-print-find-emacs)
  )

(defun spread-mode () "Major mode for spreadsheets.\\{spread-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'beg)
  (make-local-variable 'command)
  (use-local-map spread-mode-map)
  (setq mode-name "Spread")
  (setq major-mode 'spread-mode)
  (if (equal (buffer-name) ".Registry") (spread-highlight-macro))
  (setq buffer-read-only t)
  )


;; from dired.el L547
                                        ;         (put-text-property (point)
                                        ;                            (save-excursion
                                        ;                              (dired-move-to-end-of-filename)
                                        ;                              (point))
                                        ;                            'mouse-face 'highlight)
                                        ;
;; (put-text-property (point) (mark) 'mouse-face 'highlight)


(defun spread-highlight-macro nil
  "highlight spread.frame names for mouse access"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "**macro**")(forward-char)

    (toggle-read-only -1)
    (while (progn
             (setq beg (point))(end-of-line)
             (not (= beg (point)))
             )
      (put-text-property beg (1-(point)) 'mouse-face 'highlight)
      (forward-char)
      )
    (toggle-read-only 1)
    )
  (save-buffer)
  )

(defun emacs-print-find-emacs nil "" (interactive)
  (beginning-of-line)
  (setq beg (point)) (end-of-line) (backward-char)
  (setq spread-name (buffer-substring beg (point)))
  (spread-print-find spread-name nil)
  )




(defun find-spread-frame-directory nil
  "Locate directory in which spread.frame functions are stored."
  (list-command-history)
  (set-buffer "*Command History*")
  (goto-char (point-min))
  (search-forward "(load-file ")
  (goto-char (1+ (match-end 0)))(setq beg (point))
  (end-of-line)(search-backward "/")
  (goto-char (match-end 0))
  (setq spread-frame-directory
        (expand-file-name (buffer-substring beg (point))))
  (kill-buffer "*Command History*")
  )

(defvar inferior-spread-mode nil
  "Non-nil if using inferior-spread-mode as a minor mode of some other mode.")
(make-variable-buffer-local 'inferior-spread-mode)
(put 'inferior-spread-mode 'permanent-local t)
(or (assq 'inferior-spread-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(inferior-spread-mode " spread")))))

(defvar inferior-spread-mode-map nil)
(if inferior-spread-mode-map
    nil
  (setq inferior-spread-mode-map (make-sparse-keymap))
  (define-key inferior-spread-mode-map "\C-cv" 'revert-t-t)
  (define-key inferior-spread-mode-map "\C-cr" 'print-find-emacs))


(or (assq 'inferior-spread-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'inferior-spread-mode inferior-spread-mode-map)
                minor-mode-map-alist)))

(defun inferior-spread-mode (&optional arg)
  "Toggle Inferior Spread mode.
With arg, turn Inferior Spread mode on if arg is positive, off otherwise."
  (interactive "P")
  (setq inferior-spread-mode
        (if (null arg) (not inferior-spread-mode)
          (> (prefix-numeric-value arg) 0)))
  (if inferior-spread-mode
      (progn
        (set-process-filter spread-process 'comint-output-filter)
        (set-variable 'comint-output-filter-functions
                      '(spread-output-filter
                        comint-postoutput-scroll-to-bottom))
        (set-variable 'comint-scroll-to-bottom-on-output "this")
        (set-variable 'comint-scroll-show-maximum-output t)
        (force-mode-line-update))
    (message "Don't know how to turn off Inferior Spread mode")))


(defun spread-process ()
  "Start stand-alone S process to run spread."
  (comint-run S-program)
  (setq spread-process (get-buffer-process (current-buffer)))
  (setq comint-prompt-regexp shell-prompt-pattern)
  (if (not(file-writable-p ".Data/.Audit"))
      (accept-process-output spread-process))
  (accept-process-output spread-process)
  spread-process
  )

(defun spread-output-filter (str)
  "detect errors in S output"
  (if (or
       (string-match "Dumped" str)
       (string-match "Error" str)
       )
      (progn
        (switch-to-buffer-other-window S-buffer)
        (comint-show-maximum-output)
        (set-variable 'quit-flag t); beeps and writes "quit" in the message area
        )
    )
  )





(defun spread-print-find (spread-name update-Registry)
  "Place SPREAD-NAME in foreground of S-buffer (*S* or *Splus*),
update .Registry and revert buffer when UPDATE-REGISTRY is t,
print all views of spread.frame associated with SPREAD-NAME in .Registry
to /tmp/spr***** directory, and find or revert all views into emacs buffers."
  (interactive)
  (set-buffer S-buffer)
  (spread-insert
   (format "print.find.emacs('%s', update.Registry=%s)"
           spread-name (if update-Registry "T" "F")))
  (if update-Registry
      (save-excursion (spread-find-file ".Registry")))
  (spread-print-sprds)
  (switch-to-buffer spread-name)
  )

(defun spread-print-sprds () "Display in buffers all views of spread.frame"
  (interactive)
  (save-excursion
    (set-buffer "*command*")
    (revert-t-t)
    (goto-char (point-min))

    (while (< (point) (point-max))
      (set-buffer "*command*")
      (setq beg (point)) (end-of-line)
      (setq spread-name-i (buffer-substring beg (point)))
      (save-excursion (spread-find-file spread-name-i))
      (forward-line)))
  )

(defun spread-find-file (spread-name) "Display one view of spread.frame"
  (interactive)
  (switch-to-buffer spread-name)
  (if (buffer-file-name)
      (revert-t-t-read-only)
    (kill-buffer spread-name)
    (find-file (concat spread-directory "/" spread-name))
    )
  (spread-mode)
  )


(defun spread-start () "load emacs spread.frame handler"

  (if (equal major-mode 'inferior-S-mode)
      (progn
        (setq spread-mode "S-mode")
        (setq S-buffer (current-buffer)))
    (if (equal major-mode 'dired-mode)
        (progn
          (setq spread-mode "stand-alone")
          (setq S-program (read-string "Splus or S? " "Splus"))
          (setq S-buffer (concat "*" (file-name-nondirectory S-program) "*"))
          (if (not (get-buffer S-buffer))
              (get-buffer-create S-buffer))
          (if (get-buffer-process S-buffer) (set-variable 'quit-flag t)))
      (set-variable 'quit-flag t)))

  (setq spread-home-directory default-directory)
  (find-spread-frame-directory)
  (if (not spread-directory-p)
      (progn (make-directory spread-directory)
             (setq spread-directory-p t)))
  (set-buffer S-buffer)
  (cd spread-home-directory)
  (setq spread-process
        (if (equal spread-mode "stand-alone")
            (spread-process)
          (get-buffer-process (current-buffer))))
  (inferior-spread-mode 1)
  (spread-insert
   (format "assign('.spread.Data',attach('%s.Data'),frame=0)"
           spread-frame-directory))
  (spread-insert
   (format "emacs.start('%s')" spread-directory))
  (find-file spread-command-file)
  (spread-find-file ".Registry")
  )

;; start it up
(spread-start)
