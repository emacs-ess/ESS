;;(defun ess-eval-string (string) "" body)
;;(defun ess-eval-string-popup (string) "" body)
;;(defun ess-eval-string-buffer (string) "" body)

(defun ess-region-or-word-at-point ()
  "If the region is active, return the contents of the region.
If the region is not active, return the word-at-point."
  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (word-at-point)))


(defun ess-eval-expanded (&optional head tail commands-buffer)
  "Send the expanded region or word-at-point to the inferior-ess
process after first concating the head and tail.  If the region is
active, the function uses the current region.  If the region is not
active, the function uses the word-at-point"
  (interactive)
  (if (not head) (setq head "summary("))
  (if (not tail) (setq tail ")"))
  (if (not commands-buffer) (setq commands-buffer
				  (get-buffer-create "tmp-buffer")))
  (ess-command (concat head
		       (ess-region-or-word-at-point)
		       tail)
	       commands-buffer))
(define-key ess-mode-map "\C-c\C-w" 'ess-eval-expanded)

;; (defun ess-expand-string (string &optional head tail)
;;   "Expand the STRING by concating the HEAD and TAIL.
;; Default result is 'summary(string)'."
;;   (interactive)
;;   (if (not head) (setq head "summary("))
;;   (if (not tail) (setq tail ")"))
;;   (concat head string tail))


;; Write a popup Edit-data-menubar that will allow attributes and components
;; to be displayed.
;; 
;; popup menu items:
;; summary
;; print
;; show
;; help
;; dump to buffer
;; Edit.data (with optional location: window/buffer/frame)
;; drilldown
;; place in quotes  (needed to get help on "[[" for example)
;; browser on
;; browser off
;; browser popup menu
;; add your own item to this menu

