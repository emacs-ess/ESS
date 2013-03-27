
/********************/
/* comment box*/
/********************/

    ods listing; 


/* From: Star Ying <starying@outlook.com> */
/*     Subject: [ESS] Another indentation error */
/*     To: "ess-help@r-project.org" <ess-help@r-project.org> */
/*     Date: Tue, 26 Mar 2013 14:33:22 -0400 (5 minutes, 16 seconds ago) */
/*     X-Boundary: ________________________________________________________________________________________________________________________________________________________________________________________________________ */

/*     So I have another indentation error with ess and emacs that I have run into. For the example below: */

data example;
    merge
        lib.data lib.data2

        %if val=ue %then %do;
        lib.data3
            %end;
        ;
by var;

/* The last line produces an error if I hit tab or if I try to indent that */
/* region. Emacs produces the error */
/* "invalid search bound (wrong side of point)". The code itself is valid and runs */
/* in sas proper. Is this just me or is this a reproducible error? */
    
