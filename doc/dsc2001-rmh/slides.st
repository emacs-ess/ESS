Automatic syntactic indentation and highlighting of source

##as typed -- assuming screen is wide enough
case0701 <- read.table(file='case0701.asc', header=T)

##as typed -- too long.  Line is folded arbitrarily
case0701 <- read.table(file='case0701.asc', heade
r=T)

##insert line break, syntactic indentation lost
case0701 <- read.table(file='case0701.asc',
header=T)

## automatic indentation at correct parenthesis level
case0701 <- read.table(file='case0701.asc',
                       header=T)


Fill-paragraph understands comments.

as typed -- assuming screen is wide enough
## Sometimes statements are complicated with many levels of
## parentheses.  ESS gets the indentation right.

Too long and folded.
## Sometimes statements are complicated with many
levels of
## parentheses.  ESS gets the indentation right.

Fill-paragraph broke lines evenly and generated the comment symbols.
## Sometimes statements are complicated with
## many levels of parentheses.  ESS gets the
## indentation right.



Detects unbalanced parentheses

## Complicated statement

if ((abs(end(x) + tspar(x)["deltat"] - start(y))
     < eps)  &&
    (frequency(x) == frequency(y)) &&
    ((length(units(x))==0) ||
     (length(units(y))==0) ||
     (units(x) == units(y))])

On a color screen the unbalanced parentheses are bright purple.






Syntactic highlighting simplifies detection of unbalanced
quotation marks.

tmp <- f(x, "this is a string", y, z)

tmp <- f(x, "this is a string, y, z)











")
Command Line Interfaces
The user enters a line, the program responds

> case0902.lm <- lm(brain ~ gest + body + litter,
+                   data=log(case0902))
> anova(case0902.lm)
Analysis of Variance Table

Response: brain

Terms added sequentially (first to last)
          Df Sum of Sq  Mean Sq  F Value    Pr(F)
     gest  1  355.7437 355.7437 1578.333 0.000000
     body  1   69.7193  69.7193  309.324 0.000000
   litter  1    1.6125   1.6125    7.154 0.008852
Residuals 92   20.7361   0.2254                     


Graphical User Interface

> trellis.device(color=F)
> splom(~log(case0902), main="ex0902")  ##place title on plot
> anova(case0902.lm)

See GIF file











## Syntax for S  (with indentation by S)

> date
function()
{
	.C("dos_date",
		ret = character(1))$ret
}
> 

## Syntax for S (with indentation by ESS DEFAULT style)
date <- function()
{
  .C("dos_date",
     ret = character(1))$ret
}


## Syntax for S
objip <- function(pattern, where = search(), frame = NULL)
{
	result <- list()
	if(version$arch == "i386")
		pattern <- paste("*", pattern, "*", sep = "")
	for(i in where) {
		obj <- objects(i, p = pattern)
		if(length(obj) > 0)
			result[[i]] <- obj
	}
	for(i in frame) {
		obj <- objects(frame = i, p = pattern)
		if(length(obj) > 0)
			result[[paste("Frame", i, sep = ".")]] <- obj
	}
	result
}
/* syntax for SAS (see slides.sas file for correct font-lock) */

proc anova;
   class year type;
   model return = year|type;
   means type / tukey alpha=.05;
run;











## interaction with inferior ESS buffer

sum( 12, 34 + 34)




--(Unix)** tmp.s      (ESS[S] [S+4])---------
> sum( 12, 34 + 34)
[1] 80
> 34 + 34
[1] 68
> 



--(Unix)** *S+4*      (iESS [S+4]: run)-------

## transcript Editing

## transcript

> sum( 12, 34 + 34)
[1] 80
> 34 + 34
[1] 68
> 

## transcript after M-x ess-transcript-clean-region

> sum( 12, 34 + 34)
> 34 + 34




## switching processes

sum( 12, 34 + 34)

--(Unix)** tmp.s      (ESS[S] [S+4])-----------------
> 
> sum( 12, 34 + 34)
[1] 80
>

--(Unix)** *S+4*      (iESS [S+4]: run)--------------
>
> sum( 12, 34 + 34)
[1] 80
>

--(Unix)** *R*        (iESS [R] : run)---------------
Process to use: R
/* executable SAS example*/  /* see slides.sas for highligting */
proc anova;
     class store space;
     model sales = store space;
run;
--(Unix)-- myfile.sas        (ESS[SAS] [none])---------------------
    Source       DF        Anova SS   Mean Square  F Value   Pr > F

    STORE         5      6192.13889    1238.42778    23.68   0.0001
    SPACE         5       161.80556      32.36111     0.62   0.6868
--(Unix)%% myfile.lst*       (Fundamental [] ESSlst)---------------
28         proc anova;
29             class store space;
30             model sales = store space;
31         run;

NOTE: The PROCEDURE ANOVA printed pages 1-2.
--(Unix)%% myfile.log*       (Fundamental [] ESSlog)---------------
