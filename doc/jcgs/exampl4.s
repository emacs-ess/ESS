## S-mode Detects unbalanced parentheses

## Correct Complicated statement

if ((abs(end(x) + tspar(x)["deltat"] - start(y)) < eps))
  (frequency(x) == frequency(y)) ||
   ((length(units(x))==0) ||
    (length(units(y))==0) ||
    (units(x) == units(y)))


## Incorrect Complicated statement

if ((abs(end(x) + tspar(x)["deltat"] - start(y)) < eps))
  (frequency(x) == frequency(y)) ||
   ((length(units(x))==0) ||
    (length(units(y))==0) ||
    (units(x) == units(y))]

##On a color display screen the unbalanced parentheses are
##bright purple.


