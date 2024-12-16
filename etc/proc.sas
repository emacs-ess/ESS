
/*
all.txt scraped from
<https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/allprodsproc/procedures.htm>;
*/

data proc;
    length name $ 20;
    infile 'all.txt';
   
    input name;

    if length(name)>1 & name^='SAS';

    if verify(name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ')=0;

    name=lowcase(name);
run;

proc sort nodupkey data=proc;
    by name;
run;

data proc;
    set proc;
    by name;
*these PROCs are not present on my system: YMMV;
*if it starts with HP, then it requires the high-performance products;
    *where name not in:(
        'aggregation', 'appsrv', 
        'compile', 
        'db2ext', 'db2util', 'dmsrvadm', 'dmsrvdatasvc', 'dmsrvprocesssvc',
        'dqloclst', 'dqmatch', 'dqscheme', 
        'gis', 
        'hp',
        'imstat', 'imxfer', 'items',
        'lasr', 
        'mddb', 
        'optgraph',
        'pds', 'pdscopy',
        'quest', 
        'rdc', 'rdpool', 'rdsec', 'recommend', 'release', 'risk',
        'source',
        'tapecopy', 'tapelabel',
        'vasmp'
        );
run;

proc print; 
run;

data proc;
    set proc end=last;

    file "proc.txt";

    retain alpha 'abcdefghijklmnopqrstuvwxyz' prev 1;
    
    format name $quote32.;

    i=indexc(alpha, substr(name, 1, 1));

    if i>prev then do; 
        prev=i;
        put;
    end;
    
    put name @;

    if last then put;
run;

data proc;
length name $ 35;
    set proc end=last;
format name;
    file "check-proc.sas";
    name='proc '||trim(name)||';run;quit;';
    put name;
run;
