options linesize=74;
libname regr 'd:/rmh/ESS/philasug/regr';

filename grafout "regr.ps";

goptions device=ps gsfname=grafout gsfmode=replace gaccess=sasgastd
            csymbol=black;  /* csymbol makes 'symbol v=' work correctly */

data regr.fat;
    infile 'd:/rmh/hh/datasets/fat.data' firstobs=2;
    input bodyfat abdomin biceps forearm wrist;
run;

%include 'd:/stat/Data/friendly/macros/scatmat.sas' ;
%scatmat(data=regr.fat,
    var= bodyfat abdomin biceps forearm wrist,
    gout=regr.gseg);
run;

proc glm data=regr.fat;
    model bodyfat = abdomin;
    output out=regr.fat2 p=bodyfathat ;
run;


proc gplot data=regr.fat2;
    symbol1 v=dot ;
    symbol2 v=point i=rl;
    plot bodyfat   * abdomin = 1
        bodyfathat * abdomin = 2 / overlay;
run;
