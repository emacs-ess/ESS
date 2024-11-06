
data proc;
    length name $ 20;
    input name;
datalines;
freq
sgdesign
sgmap
sgplot
sgrender
sgscatter
;
run;

%macro main;

data files;
    length file $ 200;
    infile "ls1";
    input file;
run;

%let k=%_nobs(data=files);

%do i=1 %to &k;

data _null_;
    set files(obs=&i firstobs=&i);
    call symput("file", trim(file));
run;

data _0;
    length name $ 20;
    infile "&file";
    input @'PROC ' name;
    name=compress(name, '.,: ');

    if length(name)>1;

    if verify(name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ')=0;

    name=lowcase(name);
run;

%_sort(data=_0, out=_0, by=name, sort=nodupkey);

data proc;
    set proc _0;
    by name;
    if first.name;
run;

%end;

data proc;
    set proc;
    by name;
    where name not in('casoperate', 'casual', 
        'datapath', 'dates', 'dath', 'dbms', 'descending', 'distr',
        'dmdb', 'dmine', 'dmneurl', 'dmreg', 'dmvq', 'docparse',
        'emclus', 'eng', 'entrytype', 'etb',
        'geostan', 'global', 'gremov',
        'imlh', 'imll', 'imstat', 'indexpath',
        'keepvars', 
        'lasr', 'layout', 'listnode', 'lognumberformat',
        'maxsteps', 'mddb', 'mdx', 'missing', 'mlsid', 
        'noexec', 'noseqcover', 
        'ods', 'oliphant', 'outedge', 'outstat', 'outvert',
        'parms', 'path', 'pmbr', 'printinternalnames', 'printtarget',
        'record', 'records', 'reph',
        'sas', 'save', 'segsize', 'selectmgr', 'seth', 'spd', 'sq', 'sqlh', 'sqll', 
        'svdonly', 'svm', 'svmscore', 'targetstd', 'taxonomy', 'tgparse', 'tpars',
        'xml', 'xsrun',
        'yzb');
run;

proc print; 
run;

%mend main;

%main;

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
