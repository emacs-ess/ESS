
##### Assignment

### 1a Smart assign key ----------------------------------------------

foo¶

##! "_"

foo <- ¶

##> "_"

foo_¶

##> "_"

foo_ <- ¶


### 1b Can set `ess-smart-S-assign` to nil at any time ---------------

foo¶

##! (setq ess-smart-S-assign-key nil)
##! "_"

foo_¶

##> "_"

foo__¶

##> (setq ess-smart-S-assign-key "_") ; Reset
##> "_"

foo__ <- ¶


### 2 Unbinding smart key --------------------------------------------

foo¶

##! (define-key ess-mode-map "_" nil)
##! "_"

foo_¶

##> (define-key ess-mode-map "_" #'ess-smart-S-assign) ; Reset
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

##! (setq-local ess-assign-list '(" <~ "))
##! "_"

foo <~ ¶

##> "_"

foo_¶

##> "_"

foo_ <~ ¶

