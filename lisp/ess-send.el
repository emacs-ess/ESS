;; First pass at context sensitive help.

(defun ess-eval-expanded (&optional head tail commands-buffer)
  "Send the expanded current region or word-at-point to the
inferior-ess process after first concating the head and tail.
If the region is active, the function uses the current region.
If the region is not active, the function uses the word-at-point"
  (interactive)
  (if (not head) (setq head "summary("))
  (if (not tail) (setq tail ")"))
  (if (not commands-buffer) (setq commands-buffer (get-buffer-create "tmp-buffer")))
  (let (kill-ring
	current-word)
    (if mark-active
	(progn
	  (copy-region-as-kill (region-beginning) (region-end))
	  (setq current-word (current-kill 1)))
      (setq current-word (word-at-point)))
    (ess-command (concat head current-word tail) commands-buffer)))

;; this is probably not the best key or key-map
(define-key ess-mode-map "\C-c\C-w" 'ess-eval-expanded)




;; previous version, sends expanded text to Commands window

;;;(defun ess-eval-expanded (&optional head tail) ""
;;;  (interactive)
;;;  (if (not head) (setq head "summary("))
;;;  (if (not tail) (setq tail ")"))
;;;  (let (kill-ring
;;;	   current-word)
;;;    (if mark-active
;;;	   (progn
;;;	     (copy-region-as-kill (region-beginning) (region-end))
;;;	     (setq current-word (current-kill 1)))
;;;	 (setq current-word (word-at-point)))
;;;    (ess-eval-linewise (concat head current-word tail))))
;;;(define-key ess-mode-map "\C-c\C-w" 'ess-eval-expanded)




;; First working version: set of three functions.
;; The region and word-at-point are in independent functions and
;; and are called by the main function.

;;(defun ess-eval-expanded (&optional head tail) ""
;;  (interactive)
;;  (if mark-active (ess-eval-expanded-region
;;		     (region-beginning) (region-end) head tail)
;;    (ess-eval-expanded-word-at-point head tail)))

;;(defun ess-eval-expanded-region (start end &optional head tail)
;;  "Send the expanded current region to the inferior ESS process after
;;first concating the head and tail."
;;    (let (kill-ring
;;	    expanded-region)
;;	(copy-region-as-kill start end)
;;	(setq expanded-region (concat head (current-kill 1) tail))
;;	(ess-eval-linewise expanded-region))
;;)

;; (setq debug-on-error t)

;;(defun ess-eval-expanded-word-at-point (&optional head tail)
;;  "Send the expanded word-at-point to the inferior ESS process after
;;first concating the head and tail."
;;    (let (expanded-region)
;;	(setq expanded-region (concat head (word-at-point) tail))
;;	(ess-eval-linewise expanded-region))
;;)
