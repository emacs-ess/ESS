
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

foo¶

