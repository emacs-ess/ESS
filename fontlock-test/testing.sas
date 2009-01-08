/* */

/*
* ;
*/

* bad(news) ;

/*
abort;
array;
*/

data onelevel;
data two.level;
data onelevel(keep=pt rename=(pt=patient));
data two.level(keep=pt rename=(pt=patient));

attrib;
by;
cards;
cards4;
delete;
dm;
else do;
else if something then something;
do;
do over;
drop;
drop=;
end;
endsas;
error;
file;
filename;
footnote;
footnote1;
footnote10;
format;
go to;
goto;
else if;
if;
if something then something;
if something then something do;
else;
else do;
informat;
input;
keep;
keep=;
label;
length;
libname;
link;
lostcard;
options;
goptions;
output;
put;
rename;
rename=(;
retain;
return;
skip;
stop;
title;
title1;
title10;
where shhh;
where=(;
id;
tables;
window;
x;
class;
model;
var;
%include;
%Include;
select;
Select;
SELECT;
select(;
data;
Data;
data=;
set;
merge;
modify;
update;
run;
out=;

proc any data=onelevel out=two.level;
proc any data=onelevel(where=(something)) out=two.level(where=(something));

when(;
else do;
then do;
do until(;
do while(;
%_episode(;
