
##### Assignment

### 1a Smart assign key ----------------------------------------------

foo¶

##! (define-key ess-mode-map "_" #'ess-insert-assign)
##! "_"

foo <- ¶

##> "_"

foo_¶

##> "_"

foo_ <- ¶


### 3a Binding `ess-insert-assign` to simple key ---------------------

foo¶

##! (define-key ess-mode-map ";" #'ess-insert-assign)
##! ";"

foo <- ¶

##> ";"

foo;¶

##> ";"

foo; <- ¶


### 3b Binding `ess-insert-assign` to simple key with nil smart key --

foo¶

##! (setq ess-smart-S-assign-key nil)
##! ";"

foo <- ¶

##> ";"

foo;¶

##> (define-key ess-mode-map ";" nil) ; Reset
##> (setq ess-smart-S-assign-key "_") ; Reset
##> ";"

foo;;¶


### 3c Binding `ess-insert-assign` to complex key --------------------

foo¶

##! (let ((map (make-sparse-keymap)))
##!   (define-key map (kbd "M--") #'ess-insert-assign)
##!   (use-local-map map))
##! "M--"

foo <- ¶

##> "M--"

foo-¶


### 4 `ess-insert-assign` uses `ess-assign-list` ---------------------

foo¶

##! (local-set-key "_" #'ess-insert-assign)
##! (setq-local ess-assign-list '(" <~ "))
##! "_"

foo <~ ¶

##> "_"

foo_¶

##> "_"

foo_ <~ ¶

