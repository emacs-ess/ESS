who <- c(
"Aaaaa",
"Bbbbb",
"Cccc",
"Zzzzzzz",
"maximum")

what <- c("1a","1b","1c","1d","1e","1f","1g",
"1h","2a","2b","2c")

x <- matrix(0, length(who), length(what),
	dimnames=list(who,what))

x["maximum",] <- c(5,5,6,4,20,6,16,8,10,10,10)

section.7 <- as.grade(x)


