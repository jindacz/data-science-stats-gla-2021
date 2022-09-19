
#####lab 1 other examples

SO2 <- read.csv("SO2.csv")
attach(SO2)

year <- Years + Weeks / 53
so2  <- ln.SO2.


plot(so2 ~ Weeks)
lo <- loess(so2~Weeks)
lines(1:53, predict(lo, newdat=data.frame(Weeks=1:53)), col = "blue", lwd = 3)
lo2 <- loess(so2~Weeks,span=0.5)
lines(1:53, predict(lo2, newdat=data.frame(Weeks=1:53)), col = "red", lwd = 3)
lo3 <- loess(so2~Weeks, span=0.3)
lines(1:53, predict(lo3, newdat=data.frame(Weeks=1:53)), col = "green", lwd = 3)


plot(so2 ~ year, type="l")
model <- lm(so2 ~ sin(2 * pi * (Weeks - 53) / 53) +
                  cos(2 * pi * (Weeks - 53) / 53) + year)


boxplot(so2~ Weeks)
print(summary(model))
plot(so2 ~ year)
d <- expand.grid(Weeks = 1:53, year = 1989:2000)
d[,2] <- d[,2] + d[,1] / 53
pred <- predict(model, d)
lines(d[,2], pred, col = 2, lwd=4)
detach(SO2)










