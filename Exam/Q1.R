## Question 1

### Code for Question 1


library('pracma')
library('readxl')
library('matrixStats')

Dataset4 <- function(num_samples = 2000000){ # Will have VIF greater than 100
  gen1 <- rnorm(n = num_samples, mean = 80, sd= 10)
  gen2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  gen3 <- rnorm(n = num_samples, mean = 25, sd= 4) # solid linear combination of the other 2 varaibles
  x1 <-  gen1*2 + 100 + gen2 - 702
  x2 <- gen1*(1.95) +100 + gen2 - 750
  x3 <- gen1 + gen3*3.3 -200
  x4 <- gen3*3.95  + gen1*0.65 + 100
  y  <- x1 + x2 + x3  + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}

Dataset_lowvif <- function(num_samples = 2000000){ # Will have VIF less than 5
  x1 <- rnorm(n = num_samples, mean = 80, sd= 5)
  x2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  x3 <- rnorm(n = num_samples, mean = 25, sd= 5) # solid linear combination of the other 2 varaibles
  x4 <- rnorm(n = num_samples, mean = 20, sd = 5)
  y  <- x1 + x2*2 + x3*0.5 + x4**2 + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}

Dataset_moderatevif <- function(num_samples = 2000000){ # Will have VIF between 5 and 10
  gen1 <- rnorm(n = num_samples, mean = 80, sd= 10)
  gen2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  gen3 <- rnorm(n = num_samples, mean = 25, sd= 4)
  x1 <-  gen1*2 + 100 + gen2 -800
  x2 <- gen1*(1) -300 + gen2*0.9
  x3 <- gen1 + gen3*0.5 -250
  x4 <- gen3*4.5 -220 + gen2*0.29
  y  <- x1 + x2 + x3 + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, y)
  return(data)
}


#df<-read_excel('dat2017.xlsx')

Dataset_q1 <- function(num_samples = 2000000){ # Will have VIF between 5 and 10
  gen1 <- rnorm(n = num_samples, mean = 80, sd= 10)
  gen2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  gen3 <- rnorm(n = num_samples, mean = 25, sd= 4)
  x1 <-  gen2*2 + 100 + gen3 
  x2 <- gen2*0.9 + gen1*(1) - 300 +x1
  x3 <- gen1 + gen3*0.5 -250
  x4 <- gen3*1 -220 + gen2*1 -500
  y  <- x1 + x2 + x3 + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}

# dataset lowhigh will have low VIF values and high VIF values

Dataset_lowhigh <- function(num_samples = 2000000){ # Will have 2 variables with VIF less than 5 and 2 variables with VIF more than 100 
  gen1 <- rnorm(n = num_samples, mean = 80, sd= 10)
  gen2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  gen3 <- rnorm(n = num_samples, mean = 25, sd= 4) # solid linear combination of the other 2 varaibles
  gen4 <- rnorm(n = num_samples, mean = 25, sd= 4)
  x1 <-  gen1*2 + 100 + gen2 - 702
  x2 <- gen1*(1.95) +100 + gen2 - 750
  x3 <- gen4
  x4 <- gen3*3.95  + gen1*0.65 + 100
  y  <- x1 + x2 + x3  + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}

Dataset_q1 <- Dataset_q1()
#Dataset_1 <- Dataset1()

VIFQ1 <- Caculate_VIF(Dataset_q1)
VIFQ1

VIF1 <- Caculate_VIF(Dataset_1)
VIF1


Caculate_VIF(Dataset2())


x<-as.matrix(Dataset_q1[,-5])
x<-scale(x)
x<-cbind(1,x)

y <- as.matrix(Dataset_q1[,5])
xy_data <- cbind(x,y)

x1<-as.matrix(Dataset_1[,-5])
x1<-scale(x1)
x1<-cbind(1,x1)

y1 <- as.matrix(Dataset_1[,5])
xy1 <- cbind(x1,y1)

# Data_4 <- Dataset4()
# x<-as.matrix(Data_4[,-5])
# x<-scale(x)
# x<-cbind(1,x)
# 
# y <- as.matrix(Data_4[,5])
# xy_data <- cbind(x,y)

Caculate_VIF(Data_4)

Caculate_VIF(Dataset_1)
Caculate_VIF <- function(Dataset){
  #Dataset<- Dataset_2
  num_var <- ncol(Dataset)
  x <- Dataset[,-num_var]
  x <- as.matrix(x)
  
  vif_fp<-NULL
  for(i in  seq_len(num_var-1)){
    reg<-Lin_reg_fp(x[,-i],x[,i]) # take all except the ith x and regress it against the ith x
    vif_fp<-cbind(vif_fp,reg$vif)
  }
  return(vif_fp)
}


VIFQ1 <- Caculate_VIF(Dataset_1)
VIFQ1

Caculate_VIF(Data_4)
RidgeRegression(x,y, lam=10000)



RidgeRegression(x1,y1, lam=100000)

RidgeRegression <- function(x_in,y_in,lam){
  n<-nrow(x_in)
  d<-ncol(x_in)
  I<-diag(nrow = d)
  betas<-solve(t(x_in)%*%x_in + lam*t(I))%*%t(x_in)%*%y_in  # basically the only change for ridge regression
  y.hat <- x_in %*% betas
  y.mean <- mean(y)
  # sigma2 <- sum((y - y.hat)^2)
  sse <- t(y_in - y.hat) %*% (y_in - y.hat)  # sse = (y-yhat)^2
  mse <- sse / (n - 2)
  root.sq.err <- sqrt(mse) # root.sq error is the standard error
  variance <- mean((y.hat-mean(y.hat))^2)
  #variance2 <- mse*(solve(t(x_in)%*%x_in+lam*I))%*%(t(x_in)%*%x_in)%*%solve(t(x_in)%*%x_in+lam*I)
  
  bias <- (mean(y.hat-y_in))^2
  bias2 <- -lam*(solve(t(x_in)%*%x_in+lam*I))%*%(unname(betas)) 
 
  ssr <- t(y.hat-y.mean) %*% (y.hat-y.mean)
  rsq <- ssr/(ssr+sse)
  sst1 <- t(y_in-y.mean)%*%(y_in-y.mean) 
  sst2 <- sse + ssr

  #variance <- mse-bias
  return(list(betas = betas,sse=sse, mse=mse, root.sq.err=root.sq.err, bias = bias, variance = variance,ssr = ssr, sst1 = sst1, sst2 = sst2, rsq = rsq, bias2 = bias2))#, variance2 = variance2))
}



data2 <- Dataset2()
Caculate_VIF(data2)
x2<-as.matrix(Dataset_1[,-5])
x2<-scale(x1)
x2<-cbind(1,x1)

y2 <- as.matrix(Dataset_1[,5])
xy2 <- cbind(x1,y1)



lambdas2<-seq(0, 1000,1)
# DONT RUN THIS

# Do multiple cross validations for different lambda values, the one with the lowest value will be the best model.
scores<-NULL
count3 <- 0
bias <- NULL
for(i in lambdas2){
  count3 <- count3 +1
  scores<-rbind(scores,cross_validation(xy,10,i, RidgeRegression)) # the cross validation function returns the cross val scores
  print(paste0((count3/length(lambdas2))*100,"%"))
  # bias <- rbind(bias,RidgeRegression$bias)
}




RidgeRegression(x2,y2, lam=2000000)

# Question 1.2

# Do a bootstrap ridge regression to estimate the sampling distributions od the regression coefficients

num_samples = 10

bootstrapRidge<-function (xy){
  bet<-NULL
  count <- 0
  for(i in seq_len(num_samples)){
    idxs<-sample(100000, nrow(xy), replace = T) # take nrow(xy)  samples each time, with replacement
    boot<-xy[idxs,]
    betas<-RidgeRegression(boot[, 1:5], boot[,6], lam=100)$betas # get the betas from the ridge regression for this example 
    bet<-cbind(bet, betas) #place the next betas in the next column and bind with all the previous ones 
    count <- count +1
    print(paste0(count))
  }
  #bet <- as.data.frame(bet)
  return(list(bet = bet, count = count))
}


boot_ridge_betas <- bootstrapRidge(xy_data)
beta_vals <- boot_ridge_betas$bet
beta_vals <- as.data.frame(beta_vals)  
#averages <- rowmeans()
  
#boot_ridge_betas[9]

# Calculate the 95% confidence intervals for the coefficients

boot_beta0 <- sort(beta_vals[1,])
boot_beta1 <- sort(beta_vals[2,])
boot_beta2 <- sort(beta_vals[3,])
boot_beta3 <- sort(beta_vals[4,])
boot_beta4 <- sort(beta_vals[5,])

b0_lower <- boot_beta0[0.025*num_samples]
b1_lower <- boot_beta1[0.025*num_samples]
b2_lower <- boot_beta2[0.025*num_samples]
b3_lower <- boot_beta3[0.025*num_samples]
b4_lower <- boot_beta4[0.025*num_samples]

b0_upper <- boot_beta0[0.975*num_samples]
b1_upper <- boot_beta1[0.975*num_samples]
b2_upper <- boot_beta2[0.975*num_samples]
b3_upper <- boot_beta3[0.975*num_samples]
b4_upper <- boot_beta4[0.975*num_samples]

b0_CI <- cbind(b0_lower,b0_upper)
b1_CI <- cbind(b1_lower,b1_upper)
b2_CI <- cbind(b2_lower,b2_upper)
b3_CI <- cbind(b3_lower,b3_upper)
b4_CI <- cbind(b4_lower,b4_upper)



Final_CIs <- unname(rbind(b0_CI,b1_CI,b2_CI,b3_CI,b4_CI))
Final_CIs

par(mfrow=c(2,2))
beta_vals <- as.matrix(beta_vals)
col2 <- beta_vals
#return(bet)
for(i in seq_len(ncol(xy1-1))) {
  plot_val <- unname(col2[i,])
  hist(plot_val, prob = TRUE, col = 'skyblue3',breaks = 12, main = paste0("Beta ", i-1, " of OLS for sample size=",100000," (Dataset2)"), xlab="Beta value")
  x5 <- seq(min(plot_val), max(plot_val), length = 40)
  f5 <- dnorm(x5, mean = mean(plot_val), sd = sd(plot_val))
  lines(x5, f5, col = "red", lwd = 2)
  #lines(density(plot_val), lwd = 2, col = 'red')
  # curve(density(col2[i,]))
}




RidgeRegression(x,y, lam=10)

##############################################################################################
################## Do Bootstrap for different values of Lambda ###############################
#unname(regs$var)


lambdas2<-seq(0, 300000,10000)



#scores<-NULL
count3 <- 0
bias2 <- NULL
Var <- NULL
MSE <- NULL  
for(i in seq_len(length(lambdas2))){
  #i <- 1
  count3 <- count3 +1
  reg <- RidgeRegression(x,y,lam = lambdas2[i])
  bias2 <- rbind(bias2,unname(reg$bias))
  Var <- rbind(Var,unname(reg$variance))
  MSE <- rbind(MSE,unname(reg$mse))
  #scores<-rbind(scores,RidgeRegression(x,y,lambdas2[i])) # the cross validation function returns the cross val scores
  print(paste0((count3/length(lambdas2))*100,"%"))
  # bias <- rbind(bias,RidgeRegression$bias)
}



results <- cbind(bias2,Var,MSE)

plot(lambdas2,bias2 , type="l", col = "black", lwd =2, ylab = "Value", main = "Plot of Bias Vs Variance, as Lambda increases")
lines(lambdas2,Var, col='red', lwd =2)

legend(8, 8, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), cex=0.8)

par(mfrow=c(1,1))

vect<-cbind(MSE,bias2,Var,lambdas2)

plot(MSE)
lines(bias2, col='blue')
lines(Var, col = "red", lwd = 3)

plot(Var)
