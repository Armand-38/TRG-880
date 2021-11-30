# Question 2

options(scipen = 999) # remove scientific notation

# Define a fucntion to do linear regression from first principles
Lin_reg_fp<-function(x,y) { 
  beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
  n <- length(x)
  p <- ncol(x)
  #obtain predicted values
  y.hat <- x %*% beta.hat
  sse <- t(y - y.hat) %*% (y - y.hat)
  mse <- sse / (n - 2)
  root.sq.err <- sqrt(mse)
  ybar<-mean(y)
  ssr<-t(y.hat-ybar) %*% (y.hat - ybar)  # deviation from the mean
  rsq<-ssr/(ssr+sse)
  vif<-1/(1-rsq)
  return(list(beta.hat = beta.hat, root.sq.err=root.sq.err,rsq=rsq, vif=vif))
}

# Calculate the datasets, each of the datasets will have different levels of VIF

Dataset1 <- function(num_samples = 2000000){ # Will have VIF less than 5
  x1 <- rnorm(n = num_samples, mean = 80, sd= 5)
  x2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  x3 <- rnorm(n = num_samples, mean = 25, sd= 5) # solid linear combination of the other 2 varaibles
  x4 <- rnorm(n = num_samples, mean = 20, sd = 5)
  y  <- x1 + x2*2 + x3*0.5 + x4**2 + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}


Dataset2 <- function(num_samples = 2000000){ # Will have VIF between 5 and 10
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



Dataset3 <- function(num_samples = 2000000){ # Will have VIF between 10 and 15
  gen1 <- rnorm(n = num_samples, mean = 80, sd= 10)
  gen2 <- rnorm(n = num_samples, mean = 70, sd= 5)
  gen3 <- rnorm(n = num_samples, mean = 25, sd= 4) # solid linear combination of the other 2 varaibles
  x1 <-  gen1*2 + 80 + gen2 - 1000
  x2 <-  gen3*0.85 
  x3 <- gen1 + gen3*0.75 -1900 
  x4 <- gen3*5 -20 + gen1*2 +1080
  y  <- x1 + x2 + x3  + 3 + rnorm(n = num_samples, mean = 0, sd = 1) # add random error to the model
  data <- data.frame(x1, x2, x3, x4, y)
  return(data)
}



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



Dataset5 <- function(num_samples = 2000000){ # Will have 2 variables with VIF less than 5 and 2 variables with VIF more than 100 
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

# Calculate VIF for each of the variables in the dataset
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

# Calculate the correlations between each pair of variables
Calculate_corrs <- function(Dataset){
  num_var <- ncol(Dataset)
  data <- Dataset
  data <- data[,-num_var]
  corrvect <- matrix(NA,nrow = num_var-1,ncol = num_var-1)
  for (i in 1:num_var-1){
    for (j in 1:num_var-1){
      corr <- cor(data[,i],data[,j])
      corrvect[i,j] <- corr
      
    }
  }
#corrvect <- as.data.frame(corrvect)  
#correlations <- matrix(nrow = 4,ncol = 4,data = corrvect)
  return(corrvect)
}


# Initialise the datasets (do not add an intercept before calculating the VIF values)

Dataset_1 <- Dataset1()
Dataset_2 <- Dataset2()
Dataset_3 <- Dataset3()
Dataset_4 <- Dataset4()
Dataset_5 <- Dataset5()




corr_dataset1 <- Calculate_corrs(Dataset1())
corr_dataset1
# 
corr_dataset2 <- Calculate_corrs(Dataset2())
corr_dataset2
# 
corr_dataset3 <- Calculate_corrs(Dataset3())
corr_dataset3
# 
# corr_dataset4 <- Calculate_corrs(Dataset4())
# corr_dataset4
# 
# corr_dataset5 <- Calculate_corrs(Dataset4())
# corr_dataset5
# 

VIF1 <- Caculate_VIF(Dataset_1)
VIF1


VIF2 <- Caculate_VIF(Dataset_2)
VIF2


VIF3 <- Caculate_VIF(Dataset3())
VIF3

# 
VIF4 <- Caculate_VIF(Dataset4())
VIF4
# 
# VIF5 <- Caculate_VIF(Dataset5())
# VIF5


# Change the datasets to matrices so they can be analysed by the linear regression, and add an intercept
# for the OLS regression from first principles

# Dataset_1 <- as.matrix(Dataset_1)
# #intercept <- matrix(1, nrow = nrow(Dataset_1),ncol = 1)
# #Dataset_1 <- cbind(intercept,Dataset_1)
# 
Dataset_2 <- Dataset2()
Dataset_2 <- as.matrix(Dataset_2)
x2 <- Dataset_2[,-4]
x2 <- scale(x2)
Dataset_2 <- cbind(x2,Dataset_2[,4])
intercept <- matrix(1, nrow = nrow(Dataset_1),ncol = 1)
Dataset_2 <- cbind(intercept,Dataset_2)


Dataset_1 <- Dataset1()
Dataset_1 <- as.matrix(Dataset_1)
x1 <- Dataset_2[,-5]
x1 <- scale(x1)
Dataset_2 <- cbind(x1,Dataset_1[,5])
intercept <- matrix(1, nrow = nrow(Dataset_1),ncol = 1)
Dataset_1 <- cbind(intercept,Dataset_1)
# 
# Dataset_3 <- as.matrix(Dataset_3)
# Dataset_3 <- cbind(intercept,Dataset_3)
# 
# Dataset_4 <- as.matrix(Dataset_4)
# Dataset_4 <- cbind(intercept,Dataset_4)
# 
# Dataset_5 <- as.matrix(Dataset_5)
# Dataset_5 <- cbind(intercept,Dataset_5)
## Now that we have all of our datasets, we can move on to the simulation

## Simulation 1, using Dataset1 ##

# Create vector to store the sample sizes

sample_sizes <- c(20,100,1000,10000,100000,1000000)

bootstrapOLS_fp<-function (xy){ #xy will be the dataset read into the function
  count <- 0
  for (j in 1:6){
  count <- count + 1  
  bet<-NULL
  for(i in seq_len(1000)){  # do 1000 bootstraps for each sample size
    idxs<-sample(seq_len(nrow(xy)), sample_sizes[j], replace = T)
    boot<-xy[idxs,]
    betas<-Lin_reg_fp(boot[, 1:ncol(xy)-1], boot[,ncol(xy)])$beta.hat
    bet<-cbind(bet, betas)
  }
  
  col2 <- bet
  #return(bet)
  for(i in seq_len(ncol(xy)-1)) {
    plot_val <- unname(col2[i,])
    hist(plot_val, prob = TRUE, col = 'skyblue3',breaks = 12, main = paste0("Beta ", i-1, " of OLS for sample size=",sample_sizes[j]), xlab="beta value" )
    x5 <- seq(min(plot_val), max(plot_val), length = 40)
    f5 <- dnorm(x5, mean = mean(plot_val), sd = sd(plot_val))
    lines(x5, f5, col = "red", lwd = 2)
    #lines(density(plot_val), lwd = 2, col = 'red')
    # curve(density(col2[i,]))
  }
  print(paste0("Done with iteration ",j))
  #return(bet)
  }
  
}


bootstrapOLS_package<-function (xy){ #xy will be the dataset read into the function
  count <- 0
  for (j in 1:5){
    count <- count + 1  
    bet<-NULL
    xy <- Dataset_5
    xy <- as.matrix(xy)
    count2 <- 0
    #j=1
    for(i in seq_len(1000)){  # do 1000 bootstraps for each sample size
      idxs<-sample(seq_len(nrow(xy)),  sample_sizes[j], replace = T)
      boot<-xy[idxs,]
      boot <- as.data.frame(boot)
      betas1<- lm(y ~., data = boot)
      betas <- unname(betas1$coefficients)
      bet<-cbind(bet, betas)
      count2 <- count2 + 1
     # print(paste0(count2))
      summary(betas1)
    }
    
    col2 <- bet
    #return(bet)
    for(i in seq_len(ncol(xy))) {
      plot_val <- unname(col2[i,])
      hist(plot_val, prob = TRUE, col = 'skyblue3',breaks = 12, main = paste0("Beta ", i-1, " of OLS for sample size=",sample_sizes[j]," (Sim 5)"), xlab="Beta value")
      x5 <- seq(min(plot_val), max(plot_val), length = 40)
      f5 <- dnorm(x5, mean = mean(plot_val), sd = sd(plot_val))
      lines(x5, f5, col = "red", lwd = 2)
      #lines(density(plot_val), lwd = 2, col = 'red')
      # curve(density(col2[i,]))
    }
    print(paste0("Done with iteration ",j))
    #return(bet)
  }
  
}

#prim_rq<-rq(y ~ x, data = boot, tau = tau_val) # get the betas from the quantile regression for this example
#betas<- unname(prim_rq$coefficients)


# Fit our OLS and lm and compare the coefficients
par(mfcol = c(2, 2))

fp_coef <- Lin_reg_fp(Dataset_1[,1:5],Dataset_1[,6])$beta.hat
fp_coef

Dataset_2 <- Dataset2()
#Dataset_1 <- Dataset_1[,-1] # remove intercept
Dataset_2 <- as.data.frame(Dataset_2)
model_coef <- lm(y ~., data = Dataset_2)
coeff2 <- unname(model_coef$coefficients)  # do it like this in the simulation function
summary(coeff2)


Caculate_VIF(Dataset_4)


Dataset_1 <- Dataset1()
y <- Dataset_1[,5]
Dataset_1 <- Dataset_1[,-5]
Datset_1 <- scale(Dataset_1)
intercept <- matrix(1, nrow = nrow(Dataset_1),ncol = 1)
Dataset_1 <- cbind(intercept,Dataset_1)
Dataset_1 <- cbind(Dataset_1,y)

Dataset_5 <- Dataset5()
y <- Dataset_5[,-5]
Datset_5 <- scale(Dataset_5)
intercept <- matrix(1, nrow = nrow(Dataset_1),ncol = 1)
Dataset_5 <- cbind(intercept,Dataset_5)



#Dataset_1 <- Dataset_1[,-1] # remove intercept
Dataset_5 <- as.data.frame(Dataset_5)
model_coef5 <- lm(y ~., data = Dataset_5)
coeff5 <- unname(model_coef$coefficients)  # do it like this in the simulation function
summary(model_coef5)





#bootstrapOLS(Dataset_1)
col1<-bootstrapOLS_fp(Dataset_1)
col1


col2<-bootstrapOLS_package(Dataset_2)
col2


col3<-bootstrapOLS_package(Dataset_3)
col3


col4<-bootstrapOLS_package(Dataset_4)
col4


col5<-bootstrapOLS_package(Dataset_5)
col5




col3 <- bootstrapOLS_fp(Dataset_2)
#col2 <- as.data.frame(col2)
#rowSds(col2)

# for(i in seq_len(5)) {
#   hist(unname(col2[i,]), main = paste0("Beta ", i-1, " of OLS", xlab="betas"))
#   # curve(density(col2[i,]))
# }

Dataset_2<- Dataset2()
Dataset_2 <- scale(Dataset_2[,1:3])
# dummy <- Dataset_4[,1:4]
# dummy <- scale(dummy)
# Dataset_4 <- cbind(dummy,Dataset_4[5])
Dataset_2 <- as.data.frame(Dataset_2)
model_coef2 <- lm(y ~., data = Dataset_2)
#coeff4 <- unname(model_coef4$coefficients)  # do it like this in the simulation function
summary(model_coef2)



Lin_reg_fp(Dataset_2[, 1:5], Dataset_2[,1])

col3<-bootstrapOLS_fp(Dataset_2)
