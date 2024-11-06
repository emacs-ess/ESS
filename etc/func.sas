

proc freq order=freq data=sashelp.vfunc;
    tables source fnctype fncprod;
run;

/*
title 'FNCPROD=B';
proc print data=sashelp.vfunc;
    where fncname>' ' & FNCPROD='B';
run;

title 'FNCPROD=X';
proc print data=sashelp.vfunc;
    where fncname>' ' & FNCPROD='X';
run;

title 'FNCPROD=T';
proc print data=sashelp.vfunc;
    where fncname>' ' & FNCPROD='T';
run;

title 'FNCPROD=I';
proc print data=sashelp.vfunc;
    where fncname>' ' & FNCPROD='I';
run;

    endsas;
*/

data vfunc;
    set sashelp.vfunc end=last;
    by fncname;
    where fncname>' ' & (fncprod='X' | source=fncprod='B');
    file "func.txt";

    if _n_=1 then do;
        put ";; SAS functions found in sashelp.vfunc where fncprod='X' | source=fncprod='B'";
    end;
    
    if last.fncname;
    
    fncname=lowcase(fncname);

    retain alpha 'abcdefghijklmnopqrstuvwxyz' prev 1;
    
    format fncname $quote32.;

    i=indexc(alpha, substr(fncname, 1, 1));

    if i>prev then do; 
        prev=i;
        put;
    end;
    
    put fncname @;

    if last then put;
run;

data vfunc;
    set sashelp.vfunc end=last;
    by fncname;
    if last.fncname;

    where fncname>' ' & (fncprod='X' | source=fncprod='B');
    file "check-func.sas";

    fncname=trim(lowcase(fncname))||'();';

    put fncname;
run;

proc contents varnum;
run;

proc print;
    var fncname;
    format fncname $quote32.;
run;
