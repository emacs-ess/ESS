/* syntax for SAS */

proc anova;
   class year type;
   model return = year|type;
run;












/* executable SAS example*/
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
