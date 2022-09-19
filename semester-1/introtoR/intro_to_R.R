string <- "A string in single quotes"
library(stringr)
cat(string)

x <- c(1, 4, 2, 4, 1, 3, 1, 2, 4)
X <- factor(x, levels=1:5, labels=c("one", "two", "three", "four", "five"))
X
unclass(X)
X

load(url("http://www.stats.gla.ac.uk/~levers/rp/chol.RData"))
chol
chol.lowdl <- subset(chol,hdl<40)
chol.lowdl

(1/sqrt(2))*qtukey(0.05,4,24)
