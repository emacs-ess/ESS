
##### ELT

### 1 Can skip tests -------------------------------------------------

¶NULL

##! (should t)
##> (ert-skip "Reason")
##> (should nil)

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

