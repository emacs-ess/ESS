a <- function(ch) {
    if(ch == Inf) {
	E.cond <- numeric(nb)
    }
    else {
	indic  <- ifelse(jinf+1 <= 1 & jsup >= 1,1,0)
	E.cond <- ch*(-pbinom(jinf,ni,prb) + 1-pbinom(js.n,ni,prb)) +
	    ifelse(ni == 1, prb*indic,
		   mu*(pbinom(js.n-1,pmax(ni-1,1),prb)-
		       pbinom(jinf-1,pmax(ni-1,1),prb))) / sV -
			   ## why is the following line wrongly indented by Emacs/ESS ?
			   mu/sV*(pbinom(js.n,ni,prb) - pbinom(jinf,ni,prb))

	indic2 <- ifelse(jinf+1 <= 1 & jsup >= 1 & ni == 2,1,0)

    }
}
