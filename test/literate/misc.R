
##### Indentation

### 1 ----------------------------------------------------------------

  {
          fun¶_call(
argument
) +
stuff1
 } +
stuff2

##! "C-M-q"

  {
          fun¶_call(
              argument
          ) +
              stuff1
 } +
stuff2

##! "C-u"
##! "C-M-q"

{
    fun¶_call(
        argument
    ) +
        stuff1
} +
    stuff2



##### Keybindings

### 1 Smart assign ---------------------------------------------------

foo¶

##! "_"

foo <- ¶

##> "_"

foo_¶

##> "_"

foo_ <- ¶


### 2 Smart assign redefinition --------------------------------------

foo¶

##! (setq ess-smart-S-assign-key ";")
##! "_bar;"

foo_bar;¶

##! (setq ess-smart-S-assign-key ";")
##! (R-mode)
##! ";"

foo <- ¶

##> ";"

foo;¶

##! (setq ess-smart-S-assign-key nil)
##! "_"

foo_¶

##! ;; Reset
##! (setq ess-smart-S-assign-key "_")
##! (R-mode)
##! "_"

foo <- ¶


### 3 Can bind `ess-insert-assign` -----------------------------------

foo¶

##! (let ((map (make-sparse-keymap)))
##!   (define-key map (kbd "M--") #'ess-insert-assign)
##!   (use-local-map map))
##! "M--"

foo <- ¶

##> "M--"

foo¶

##> "M--"

foo <- ¶


### 4 Smart assign uses `ess-assign-list` ----------------------------

foo¶

##! (setq-local ess-assign-list '(" <~ "))
##! "_"

foo <~ ¶

##> "_"

foo_¶

##> "_"

foo_ <~ ¶


### 5 Can unbind "_" key from ESS maps -------------------------------

foo¶

##! "_"

foo <- ¶

##! (define-key ess-mode-map "_" nil)
##! "_"

foo_¶


### 6 Custom "_" key in ESS maps has precedence ----------------------

foo¶

##! (define-key ess-mode-map "_" (lambda () (interactive) (insert "FOO")))
##! (setq ess-smart-S-assign-key "_")
##! (R-mode)
##! "_"

fooFOO¶

