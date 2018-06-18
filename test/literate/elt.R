
##### ELT

### 1 Can skip tests -------------------------------------------------

¶NULL

##! (should t)
##> (when (>= emacs-major-version 25)
##>   (ert-skip "Reason")
##>   (should nil))

¶NULL


### 2 Multiple cursors -----------------------------------------------

¶NULL
¶NA
foo ¶bar

##! (forward-word)
##> (should (eolp))

NULL¶
NA¶
foo bar¶

##> (backward-word)

¶NULL
¶NA
foo ¶bar


### 3 Multiple cursors on single lines -------------------------------

¶NULL ¶NULL
¶NA ¶NA
¶foo ¶bar

##! (forward-word)

NULL¶ NULL¶
NA¶ NA¶
foo¶ bar¶


### 4 Multiple cursors with line changes -----------------------------

¶foo¶foo
¶bar¶bar
¶baz¶baz

##! (forward-char 3)
##> (when (and (eolp) (not (eobp)))
##>   (delete-char 1))
##> (insert "\nQUUX\n")
##> (backward-char)

foo
QUUX¶
foo
QUUX¶
bar
QUUX¶
bar
QUUX¶
baz
QUUX¶
baz
QUUX¶



### 5 Multiple cursors with line changes behind earlier marker -------

¶foo¶foo

##! (ignore-errors (backward-char 3))
##> (insert "bar")

barbar¶¶foofoo

