#B-Splines
library(openxlsx)
library(dplyr)
library(tidyverse)
library(splines)
library(ggplot2)
library('matrixStats')


d = read_excel("Heart.xls")

n = nrow(d)

d = d[,2:ncol(d)] %>%
  dplyr::select(-adiposity,-typea)

d$famhist=factor(d$famhist)


# Replace this construct bases part with the B-splines basis functions

# d_k = function(x, e, k, K){
#   hockey1 = (x - e[k])^3 
#   hockey2 = (x - e[K])^3
#   hockey1[hockey1<0] <- 0
#   hockey2[hockey2<0] <- 0
#   denominator = e[K] - e[k] 
#   return((hockey1 - hockey2)/denominator)
# }

basis <- function(x, degree, i, knots) {
  if(degree == 0){
    B <- ifelse((x >= knots[i]) & (x < knots[i+1]), 1, 0)
  } else {
    if((knots[degree+i] - knots[i]) == 0) {
      alpha1 <- 0
    } else {
      alpha1 <- (x - knots[i])/(knots[degree+i] - knots[i]) # formula on page 5 of spline_primer_pdf
    }
    if((knots[i+degree+1] - knots[i+1]) == 0) {
      alpha2 <- 0
    } else {
      alpha2 <- (knots[i+degree+1] - x)/(knots[i+degree+1] - knots[i+1]) # formula on page 5 of spline_primer_pdf
    }
    B <- alpha1*basis(x, (degree-1), i, knots) + alpha2*basis(x, (degree-1), (i+1), knots) # Recursively construct the basis
  }
  return(B)
}

bs <- function(x, degree=3, interior.knots=NULL, intercept=FALSE, Boundary.knots = c(0,1)) {
  if(missing(x)) stop("You must provide x")
  if(degree < 1) stop("The spline degree must be at least 1")
  Boundary.knots <- sort(Boundary.knots)
  interior.knots.sorted <- NULL
  if(!is.null(interior.knots)) interior.knots.sorted <- sort(interior.knots)  # Sort the interior knots
  
  knots <- c(rep(Boundary.knots[1], (degree+1)), interior.knots.sorted, rep(Boundary.knots[2], (degree+1))) # craete sorted list of knots
  # knots = c((degree+1 0 values), NULL, (degree+1 1 values))
  K <- length(interior.knots) + degree + 1
  
  B.mat <- matrix(0,length(x),K)
  
  for(j in 1:K) B.mat[,j] <- basis(x, degree, j, knots) # call the basis function to construct the bases
  if(any(x == Boundary.knots[2])) B.mat[x == Boundary.knots[2], K] <- 1
  if(intercept == FALSE) {
    return(B.mat[,-1])
  } else {
    return(B.mat)
  }
}
std.scaler<-function(x){  # can also use the scale function
  means<-colMeans(x)
  sds<-matrixStats::colSds(x)
  return((x-means)/sds)
}

# Basis for sbp (h(x1))
x1 = d$sbp
#x1 = std.scaler(x1)
x1 <- scale(x1)
e1 = unname(quantile(x1)) #knot values for sbp
e1 <- e1[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e1[1],e1[2],e1[3])

par(mfrow = c(1,1))
n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B1 <- bs(x1, degree=4 , intercept = FALSE, Boundary.knots=c(min(x1), max(x1)), interior.knots = c(e1[1],e1[2],e1[3]))
B1
values <- cbind(x1,B1)
sort_val <- values[order(values[,1]),]
matplot(sort_val[,1], sort_val[,2:7], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x1", main= "Plot of basis functions for variable sbp")
#B2


h_x1 = cbind(x1,B1[,1:7])


########### Basis for tobaco (h(x2)) ##########
par(mfrow = c(2,2))

# Basis for tobacco (h(x2))
x2 = d$tobacco
#x2 = std.scaler(x2)
x2 <- scale(x2)
e2 = unname(quantile(x2)) #knot values for tobacco
e2 <- e2[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e2[1],e2[2],e2[3])


n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B2 <- bs(x2, degree=4 , intercept = FALSE, Boundary.knots=c(min(x2), max(x2)), interior.knots = c(e2[1],e2[2],e2[3]))
B2

values2 <- cbind(x2,B2)
sort_val2 <- values2[order(values2[,1]),]
matplot(sort_val2[,1], sort_val2[,2:8], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x2", main= "Plot of basis functions for variable tobacco")
#B2


h_x2 = cbind(x2,B2[,1:7])
h_x2 <- as.matrix(h_x2)


########### Basis for ldl (h(x3)) ##########

par(mfrow = c(2,2))

# Basis for ldl (h(x3))
x3 = d$ldl
#x2 = std.scaler(x2)
x3 <- scale(x3)
e3 = unname(quantile(x3)) #knot values for ldl
e3 <- e3[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e3[1],e3[2],e3[3])


n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B3 <- bs(x3, degree=4 , intercept = FALSE, Boundary.knots=c(min(x3), max(x3)), interior.knots = c(e3[1],e3[2],e3[3]))
B3

values3 <- cbind(x3,B3)
sort_val3 <- values3[order(values3[,1]),]
matplot(sort_val3[,1], sort_val3[,2:8], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x3", main= "Plot of basis functions for variable ldl")
#B2


h_x3 = cbind(x3,B3[,1:7])
h_x3 <- as.matrix(h_x3)

########### Basis for famhist (h(x4)) ##########

h_x4 = as.numeric(d$famhist)
h_x4 <- as.matrix(h_x4)

########### Basis for obesity (h(x5)) ##########

par(mfrow = c(2,2))

# Basis for obesity(h(x5))
x5 = d$obesity
#x2 = std.scaler(x2)
x5 <- scale(x5)
e5 = unname(quantile(x5)) #knot values for ldl
e5 <- e5[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e5[1],e5[2],e5[3])


n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B5 <- bs(x5, degree=4 , intercept = FALSE, Boundary.knots=c(min(x5), max(x5)), interior.knots = c(e5[1],e5[2],e5[3]))
B5

values5 <- cbind(x5,B5)
sort_val5 <- values5[order(values5[,1]),]
matplot(sort_val5[,1], sort_val5[,2:8], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x5", main= "Plot of basis functions for variable obesity")
#B2


h_x5 = cbind(x5,B5[,1:7])
h_x5 <- as.matrix(h_x5)

########### Basis for alcohol (h(x6)) ##########

# Basis for alcohol(h(x6))
x6 = d$alcohol
#x2 = std.scaler(x2)
x6 <- scale(x6)
e6 = unname(quantile(x6)) #knot values for ldl
e6 <- e6[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e6[1],e6[2],e6[3])


n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B6 <- bs(x6, degree=4 , intercept = FALSE, Boundary.knots=c(min(x6), max(x6)), interior.knots = c(e6[1],e6[2],e6[3]))
B6

values6 <- cbind(x6,B6)
sort_val6 <- values6[order(values6[,1]),]
matplot(sort_val6[,1], sort_val6[,2:8], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x6", main= "Plot of basis functions for variable alcohol")
#B2


h_x6 = cbind(x6,B6[,1:7])
h_x6 <- as.matrix(h_x6)
########### Basis for age (h(x7)) ##########

par(mfrow = c(1,1))

# Basis for age(h(x7))
x7 = d$age
#x2 = std.scaler(x2)
x7 <- scale(x7)
e7 = unname(quantile(x7)) #knot values for ldl
e7 <- e7[2:4] # only want 0.25, 0.5 and 0.75 quantiles
interior.knots = c(e7[1],e7[2],e7[3])


n <- 1000

# set intercept = FALSE and add a global intercept at the end, with all of the basis functions
B7 <- bs(x7, degree=4 , intercept = FALSE, Boundary.knots=c(min(x7), max(x7)), interior.knots = c(e7[1],e7[2],e7[3]))
B7

values7 <- cbind(x7,B7)
sort_val7 <- values7[order(values7[,1]),]
matplot(sort_val7[,1], sort_val7[,2:8], type="l", lwd=2,ylab = "Basis function value", xlab = "Basis functions for x7", main= "Plot of basis functions for variable age")
#B2


h_x7 = cbind(x7,B7[,1:7])
h_x7 <- as.matrix(h_x7)


# we are now done creating basis functions for each of the variables 


chd = as.numeric(d$chd)
y = chd

## Construct the global basis function to be used in the logistic regression

x = data.matrix(data.frame(h_x1[,1:7],h_x2[,1:7],h_x3[,1:7],h_x4,h_x5[,1:7],h_x6[,1:7],h_x7[,1:7]))
x = scale(x)
n <- nrow(x)
#intercept = matrix(1, n, 1)
#x = cbind(intercept, x) #standardised X

allval <- cbind(y,x)
allval <- as.data.frame(allval)
glm.fit <- glm(y ~ ., data = allval, family = binomial)
summary(glm.fit)

betas <- glm.fit$coefficients
betas

beta <- betas
beta <- as.matrix(beta, ncol = 11,nrow=4)
#?scale()

#x = data.matrix(data.frame(intercept = matrix(1, n, 1), h_x1, h_x2, h_x3, h_x4, h_x5, h_x7)) #Excluding alcohol
beta = matrix(0, nrow = ncol(x), ncol = 1)
prob = function(x, beta){
  return(1/(1+exp(-x%*%beta)))
}

# Fit the Logistic regression
# Calculate the betas iteratively

x

det(x)

tol = 0.0001
crit = 1
counter = 0
#while (crit > tol){
  counter = counter+1
  probs = prob(x, beta)
  W = diag(diag(probs%*%t((1-probs))), n, n)
  #score = t(x)%*%(y - probs)
  #hessian = -t(x)%*%W%*%x
  #beta_new = beta + solve(hessian)%*%score
  z = x%*%beta + solve(W)%*%(y-probs)
  beta_new = solve(t(x)%*%W%*%x)%*%t(x)%*%W%*%z
  crit = max(beta-beta_new)
  beta = beta_new
#}

beta


####Covariance matrix of betas (SIGMA) ####

#inv(H`*W*H)


solve(t(x)%*%W%*%x)
sigma = unname(solve(t(x)%*%W%*%x))
sigma

# sigma <- round(round(sigma,digits=5))
# sigma

det(sigma)

glm.fit$residuals



# beta <- glm.fit$coefficients
# beta <- unname(beta)
# beta

y_pred = predict(glm.fit, d, type="response")




















#####Plots####

#sbp plot:
idx_sbp = c(2, 3, 4, 5, 6, 7, 8)

x_sbp = x[,idx_sbp]
x_sbp = x_sbp[order(x_sbp[,1]),]

betas_sbp = as.matrix(unname(beta[idx_sbp,]))
yhat_sbp = x_sbp%*%betas_sbp


#plot(d$sbp, scaled.yhat_sbp)

#sbp variance plot:

idx_sbp = c(2, 3, 4, 5, 6, 7, 8)
betas_sbp = as.matrix(unname(beta[idx_sbp,]))  #Get the beta coefficients 
yhat_sbp = x[,idx_sbp]%*%betas_sbp

sigma_sbp = sigma[2:8, 2:8]

var_sbp = x[,idx_sbp]%*%sigma_sbp%*%t(x[,idx_sbp])
pointwise_var_sbp = diag(var_sbp)
ci_upper_sbp = yhat_sbp + 2*sqrt(pointwise_var_sbp)
ci_lower_sbp = yhat_sbp - 2*sqrt(pointwise_var_sbp)

sbp_plot = data.frame(d$sbp, yhat_sbp, ci_lower_sbp, ci_upper_sbp)
sbp_plot = sbp_plot[order(sbp_plot$d.sbp),]


plot(sbp_plot[,1], sbp_plot[,2], type = "l")
lines(sbp_plot[,1], sbp_plot[,4], col = "red")
lines(sbp_plot[,1], sbp_plot[,3], col = "blue")

ggplot(sbp_plot, aes(sbp_plot[,1])) + ggtitle("Plot of Sbp") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(Sbp)", x = "Sbp") +
  geom_line(aes(y=sbp_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=sbp_plot[,3], ymax=sbp_plot[,4]), alpha=0.2)


#tobacco plot

idx_tob = c(9,10,11,12,13,14,15)
betas_tob = as.matrix(unname(beta[idx_tob,]))
yhat_tob = x[,idx_tob]%*%betas_tob

sigma_tob = sigma[9:15, 9:15]

var_tob = x[,idx_tob]%*%sigma_tob%*%t(x[,idx_tob])
pointwise_var_tob = diag(var_tob)
ci_upper_tob = yhat_tob + 2*sqrt(pointwise_var_tob)
ci_lower_tob = yhat_tob - 2*sqrt(pointwise_var_tob)

tob_plot = data.frame(d$tobacco, yhat_tob, ci_lower_tob, ci_upper_tob)
tob_plot = tob_plot[order(tob_plot$d.tobacco),]


plot(tob_plot[,1], tob_plot[,2], type = "l")
lines(tob_plot[,1], tob_plot[,4], col = "red")
lines(tob_plot[,1], tob_plot[,3], col = "blue")

ggplot(tob_plot, aes(tob_plot[,1])) +  ggtitle("Plot of Tobacco") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(Tobacco)", x = "Tobacco") +
  geom_line(aes(y=tob_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=tob_plot[,3], ymax=tob_plot[,4]), alpha=0.3)



#ldl plot


idx_ldl = c(16, 17, 18, 19, 20, 21 ,22)
betas_ldl = as.matrix(unname(beta[idx_ldl,]))
yhat_ldl = x[,idx_ldl]%*%betas_ldl

sigma_ldl = sigma[16:22, 16:22]

var_ldl = x[,idx_ldl]%*%sigma_ldl%*%t(x[,idx_ldl])
var_ldl
pointwise_var_ldl = diag(var_ldl)
ci_upper_ldl = yhat_ldl + 2*sqrt(pointwise_var_ldl)
ci_lower_ldl = yhat_ldl - 2*sqrt(pointwise_var_ldl)

ldl_plot = data.frame(d$ldl, yhat_ldl, ci_lower_ldl, ci_upper_ldl)
ldl_plot = ldl_plot[order(ldl_plot$d.ldl),]


plot(ldl_plot[,1], ldl_plot[,2], type = "l")
lines(ldl_plot[,1], ldl_plot[,4], col = "red")
lines(ldl_plot[,1], ldl_plot[,3], col = "blue")

ggplot(ldl_plot, aes(ldl_plot[,1])) + ggtitle("Plot of ldl") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(ldl)", x = "ldl") +
  geom_line(aes(y=ldl_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=ldl_plot[,3], ymax=ldl_plot[,4]), alpha=0.2)

# ff <- cbind(ci_lower_ldl,ci_upper_ldl)
# ff
# plot obesity

idx_ob = c(23, 24, 25, 26, 27, 28, 29)
betas_ob = as.matrix(unname(beta[idx_ob,]))
yhat_ob = x[,idx_ob]%*%betas_ob

sigma_ob = sigma[23:29, 23:29]

var_ob = x[,idx_ob]%*%sigma_ob%*%t(x[,idx_ob])
pointwise_var_ob = diag(var_ob)
ci_upper_ob = yhat_ob + 2*sqrt(pointwise_var_ob)
ci_lower_ob = yhat_ob - 2*sqrt(pointwise_var_ob)

ob_plot = data.frame(d$obesity, yhat_ob, ci_lower_ob, ci_upper_ob)
ob_plot = ob_plot[order(ob_plot$d.obesity),]


plot(ob_plot[,1], ob_plot[,2], type = "l", main = "Plot of Obesity")
lines(ob_plot[,1], ob_plot[,4], col = "red")
lines(ob_plot[,1], ob_plot[,3], col = "blue")

ggplot(ob_plot, aes(ob_plot[,1]))  + ggtitle("Plot of Obesity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(Obesity)", x = "Obesity") +
  geom_line(aes(y=ob_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=ob_plot[,3], ymax=ob_plot[,4]), alpha=0.3)











# plot age

idx_age = c(37, 38, 39, 40, 41, 42, 43)
betas_age = as.matrix(unname(beta[idx_age,]))
yhat_age = x[,idx_age]%*%betas_age

sigma_age = sigma[37:43, 37:43]

var_age = x[,idx_age]%*%sigma_age%*%t(x[,idx_age])
pointwise_var_age = diag(var_age)
ci_upper_age = yhat_age + 2*sqrt(pointwise_var_age)
ci_lower_age = yhat_age - 2*sqrt(pointwise_var_age)

age_plot = data.frame(d$age, yhat_age, ci_lower_age, ci_upper_age)
age_plot = age_plot[order(age_plot$d.age),]


plot(age_plot[,1], age_plot[,2], type = "l")
lines(age_plot[,1], age_plot[,4], col = "red")
lines(age_plot[,1], age_plot[,3], col = "blue")

ggplot(age_plot, aes(age_plot[,1])) + ggtitle("Plot of Age") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(Age)", x = "Age") +
  geom_line(aes(y=age_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=age_plot[,3], ymax=age_plot[,4]), alpha=0.3)



#Famhist

idx_fh = c(14)
betas_fh = as.matrix(unname(beta[idx_fh,]))
yhat_fh = x[,idx_fh]%*%betas_fh

sigma_fh = sigma[14, 14]

var_fh = x[,idx_fh]%*%sigma_fh%*%t(x[,idx_fh])
pointwise_var_fh = diag(var_fh)
ci_upper_fh = yhat_fh + 2*sqrt(pointwise_var_fh)
ci_lower_fh = yhat_fh - 2*sqrt(pointwise_var_fh)

age_plot = data.frame(d$age, yhat_fh, ci_lower_fh, ci_upper_fh)
age_plot = age_plot[order(age_plot$d.age),]


plot(age_plot[,1], age_plot[,2], type = "l")
lines(age_plot[,1], age_plot[,4], col = "red")
lines(age_plot[,1], age_plot[,3], col = "blue")

ggplot(age_plot, aes(age_plot[,1])) + ggtitle("Plot of Age") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="f_hat(Age)", x = "Age") +
  geom_line(aes(y=age_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=age_plot[,3], ymax=age_plot[,4]), alpha=0.3)
















# # Make the Pairs Plot
# pairs(d[1:7],col=as.factor(d$chd))

# #Fitting a logistic regression model to the data
# heartModel=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family='binomial',data=d)
# summary(heartModel)

# #Fitting a natural spline to the data

# format = "chd ~ ns(sbp,df=4) + ns(tobacco,df=4) + ns(ldl,df=4) + famhist + ns(obesity,df=4) + ns(alcohol,df=4) + ns(age,df=4)"
# format = formula(format)

# ?glm
# splineModel = glm( format, data=d, family=binomial )
# print( summary(splineModel), digits=3 )

# drop1(splineModel, scope=format, test="Chisq" )

# backstep2 = step(splineModel) # Backwards selection is the default
# summary(backstep2)
# drop1( backstep2, test="Chisq" )

# splineModel$coefficients
# length(splineModel$coefficients)

# #Plot for sbp
# sbp = d$sbp

# e1 = 0.4
# e2 = 0.6
# e4 = max(sbp)





# theta1 = -1.47936736 #beta1
# theta2 = -1.351182 #beta2
# theta3 = -3.75372259#theta3
# theta4 = 1.39731908#theta4

# thetaprime1 = theta1*(e4 - e1)
# thetaprime2 = theta2*(e4 - e2)
# thetaprime3 = theta3*(e4 - e3)
# thetaprime4 = theta4*(e4 - e4)

# dk3 = ((sbp - e3)^3 - (sbp - e4)^3)/(e4-e3)
# dk4 = 0

# n1 = 1
# n2 = sbp
# n3 = dk3 - dk3
# n4 = dk4 - dk3


# f.sbp =  thetaprime1*n1 + thetaprime2*n2 + thetaprime3*n3 + thetaprime4*n4


# plot(sbp, f.sbp)
