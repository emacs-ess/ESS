x <- 1:10
y <- rnorm(10)
xy.lm <- lm(y ~ x)
anova(xy.lm)
