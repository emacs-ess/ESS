(defvar semantic-toplevel-r-bovine-table
  nil
  "Table for use with semantic for parsing R.")

(defvar semantic-r-keyword-table
  (semantic-flex-make-keyword-table 
   `( ("..." . VARARGS)
      ("<-" . ASSIGNMENT)
      ("_" . ASSIGNMENT2)
      ("$" . DOLLAR)
      ("<<-" . ENVASSIGNMENT)
      ("try" . TRY)
      ("while" . WHILE)
      ("library" . LIBRARY)
      ("source" . SOURCE)
      ("require" . REQUIRE)
      )
   '(
     ("<-" summary "assignment of closure")
     ("_" summary "assignment of closure")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("while" summary "while (<expr>) <closure>;")
     ("library" summary "library(packagename)")
     ("source" summary "source()")
     ("require" summary "require(packagename)")
     ))
  "Keyword table for use with semantic for parsing R.")

(defvar semantic-default-r-setup 
  nil
  " test .")




