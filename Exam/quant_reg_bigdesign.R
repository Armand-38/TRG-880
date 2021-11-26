#Quantile regression in R.
library(readxl)
require(quantreg)
require(ggplot2)
require(gridExtra)

#Polynomial regression

#OLS
setwd("/home/rian/Dropbox/2. TRG880/1. Assignments/Assignment 7/Code")
d = read_xlsx("quantreg.xlsx")
x = d$age
y = d$bmia

X = cbind(x, x**2, x**3, 1/x, sqrt(x), sqrt(x)*x)
d2 = data.frame(cbind(y, X))
names = c("y", "x", "x2", "x3", "inv_x", "sqrt_x", "dat_one")
names(d2) = names 


par(mfrow=c(1,1))
plot(d$age, d$bmia)
prim_reg = lm(y ~ . , d2 )
res = cbind(x, prim_reg$fitted.values)  # fitted.values give the yhat values of the fitted regression
res = res[order(res[,1]),] # order all according to the x so that it can be plotted
lines(res[,1], res[,2], col = "red", pch = 19, lwd = 3)

#Quantile
taus = seq(0.1, 0.9, 0.1)
taus = matrix(taus)
prim_qr = rq(y ~ ., data = d2, tau = taus)

summary.rq(prim_qr)

# plotting different quantiles
colors <- c("#ffe6e6", "#ffcccc", "#ff9999", "#ff6666", "#ff3333",
            "#ff0000", "#cc0000", "#b30000", "#800000", "#4d0000", "#000000")
plot(y ~ x, data = d2, pch = 16, main = "bmia ~ age")

res2 = cbind(x, prim_qr$fitted.values) # for quantile regression fitted.values will return the yhat value for each quantile you specified
res2 = res2[order(res2[,1]),]
for (j in 2:ncol(prim_qr$fitted.values)) {
  lines(res2[, 1], res2[,j], col = colors[j], lwd = 3)
}

# par(mfrow=c(2,3))
# for (j in 1:ncol(X)) {
#   slope = prim_qr$coefficients[j,]
#   plot(taus, slope)
#   lines(taus, slope)
#   lines(taus, rep(prim_reg$coefficients[j], length(taus)))
# }


# for theis example the interpretation of the betas does not make sense since we added
# terms to make it polynomial, but use this code to get these plots fot he betas and interpret them
par(mfrow=c(2,3))
# for (j in 2:ncol(X)){
#   plot(summary(prim_qr, se = "iid"))
# }
plot(summary(prim_qr, se = "iid"))




