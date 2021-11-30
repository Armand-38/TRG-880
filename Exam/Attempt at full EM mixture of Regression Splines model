# first import the data


# Attempt at full mixture of regression splines 

library(readxl)

# function to perform Simple Linear Regression (SLR)
SLR<-function(x,y) {
  beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
  n <- length(x)
  p <- ncol(x)
  #obtain predicted values
  y.hat <- x %*% beta.hat
  sigma2 <- sum((y - y.hat)^2)
  sqrt(sigma2)
  J <- pracma::ones(length(x))
  sse <- t(y - y.hat) %*% (y - y.hat)
  mse <- sse / (n - 2)
  root.sq.err <- sqrt(mse)
  SLR.obj<-list(beta.hat = beta.hat)
  return(SLR.obj)
}

#import the data
mix <- read.csv(file = 'mix.csv')
plot(mix, main = "Graphical representation of the data") # graphical representation

x<-mix$x  
y<-mix$y 
xy<-cbind(x,y)
intercept <-  matrix(1, nrow = nrow(xy))
x<-cbind(intercept, x)

xy<-cbind(x,y)

colnames(xy) <- c("intercept", "x", "y")

# you need to construct a new grid for each component, thus define a function to construct the grid

construct_grid <- function(n, c){ # c is the minimum distance allowed between points for structural breaks
  grid <- NULL
  for (i in (c+1):(n-2*c)){
    for (j in (i+c+1):(n-c)){
      add <- cbind(i,j)
      grid <- rbind(grid,add)
    }
  }
  return(grid)
}

do_gridsearch <- function(x, y, grid){
  # now do the grid search
  
  xinit <- x
  yinit <- y
  intercept <- matrix(1, nrow = length(x))
  print(length(xinit))
  print(length(yinit))
  print(length(intercept))
  
  placeholder_sse = 9999999999999999
  
  # the following loop will test all possibilities in the grid for structural breakpoints
  
  count = 0
  for (i in seq_len(nrow(grid))) {
    #i= 2
    
    count = count +1
    x1s_p <- grid[i,1]
    #x1s_p
    x2s_p <- grid[i,2]
    x1s = xinit[x1s_p]
    x2s = xinit[x2s_p]
    print(x1s)
    print(x2s)
    
    xmodel2 <- cbind(intercept,xinit, (((xinit-x1s)>0)*(xinit-x1s)), (((xinit-x2s)>0)*(xinit-x2s)))

    x <- xmodel2
    new_betas <- solve(t(x) %*% x) %*% t(x) %*% y

   # mult <- t(xmodel2)%*%xmodel2
    #deter <- det(t(xmodel2)%*%xmodel2)
    #print(deter)
    fit <- lm(yinit~xmodel2)
    sse =  sum(fit$residuals^2)
    print(sse)
    
    if (sse<placeholder_sse) {
      yhat1 <- fit$fitted.values
      placeholder_sse = sse
      x1s_pm <- x1s
      x2s_pm <- x2s
      knots <- cbind(x1s_pm,x2s_pm)  #Update the knot values
    }
    print(count)
  }
  out = list(min_sse = placeholder_sse, knots = knots, yhat = yhat1, xmodel2 = xmodel2, new_betas = new_betas)
  return(out)
}

######### start out with kmeans ######################


# initialize clustering solution with kmeans
inital.values <- kmeans(mix,2)   # change data
plot(mix,col = inital.values$cluster,  main = "Model of mixture of regression splines")  # change data

clusters<-inital.values$cluster

# use the within cluster ss from kmeans to pool a variance parameter
sigma<-sqrt(sum(inital.values$withinss/nrow(xy)))

# split the data according to kmeans initialization
splitted.xy1<-xy[clusters==1,] 
splitted.xy2<-xy[clusters==2,]

# calculate inital betas using SLR
# initial.values1<-SLR(splitted.xy1[,1:2],splitted.xy1[,3])
# initial.values2<-SLR(splitted.xy2[,1:2], splitted.xy2[,3])

# inital.values1_lm <- lm(y~x, data.frame(splitted.xy1))
# inital.values2_lm <- lm(y~x, data.frame(splitted.xy2))
# y.hat <- rbind(inital.values1$y.hat,initial.values2$y.hat)


n<-nrow(xy)
# concatenate the beta parmeters and use the cluster assignment ratios as priors for the mixing probs
betas<-cbind(initial.values1$beta.hat,initial.values2$beta.hat)


pis<-rbind(nrow(splitted.xy1)/nrow(xy),nrow(splitted.xy2)/nrow(xy)) # 1000 since there are 100 data points, change this!!!




# construct the grid the will be searched over during the gridsearch

grid <- construct_grid(n,c=5)

# nrow(grid) shows how many combinations the grid has

grid1 <- construct_grid(nrow(splitted.xy1),5)
grid2 <- construct_grid(nrow(splitted.xy2),5)

search1 <- do_gridsearch(splitted.xy1[,2],splitted.xy1[,3],grid1)
search1$knots
initial.values1<-search1$new_betas
search2 <- do_gridsearch(splitted.xy2[,2],splitted.xy2[,3],grid2)
search2$knots
initial.values2<-search2$new_betas


n<-nrow(xy)
# concatenate the beta parmeters and use the cluster assignment ratios as priors for the mixing probs
betas<-cbind(initial.values1,initial.values2)


lines(splitted.xy1[,2],search1$yhat)
lines(splitted.xy2[,2],search2$yhat , col = "red")
#abline(v=c(145.75,254.918,20,156.269,229.569), col="blue", lty=2, lwd=1)
#search1$yhat




## Now start the EM algorithm

splitted.xy_all <- rbind(splitted.xy1,splitted.xy2)

error<-10000
i<-1
count = 0
while (error>0.1) {
  count = count + 1 
  den <- NULL
  # caluclate responsibilties
  #for (i in seq_len(2)) {  # run the for loop twice since we had 2 clusters
    grid1 <- construct_grid(nrow(splitted.xy1),5) # construct grid based the new splitted.xy values
    search1 <- do_gridsearch(splitted.xy1[,2],splitted.xy1[,3],grid1)
    newmod <- search1$xmodel2
    y.hat1 <- newmod %*% betas[, i]
    #sse <- t(y - y.hat) %*% (y - y.hat)
    sse <- search1$min_sse
    mse <- sse / (n - 2)
    den <- cbind(den, pis[i] * dnorm(y, mean = y.hat1, sd = sigma)) # calculate the denominator of the gammas
  #}
    grid2 <- construct_grid(nrow(splitted.xy2),5) # construct grid based the new splitted.xy values
    search2 <- do_gridsearch(splitted.xy2[,2],splitted.xy2[,3],grid1)
    newmod2 <- search2$xmodel2
    y.hat2 <- newmod %*% betas[, i]
    #sse <- t(y - y.hat) %*% (y - y.hat)
    sse2 <- search2$min_sse
    mse2 <- sse2 / (n - 2)
    den <- cbind(den, pis[i] * dnorm(y, mean = y.hat2, sd = sigma)) # calculate the denominator of the gammas
    
  # Now calculate the gammas  
  denom <- rowSums(den)
  gamma_ik <- den / denom
  # using (iteratively) weighted least squares determine new parameters of beta
  beta.new <- NULL
  for (i in seq_len(2)) {
    W_k <- pracma::Diag(gamma_ik[, i])
    beta.new <- cbind(beta.new, solve(t(x) %*% W_k %*% x) %*% t(x) %*% W_k %*% y)
  }
  
  # calculate new mixing probs
  pi_k <- colSums(gamma_ik) / nrow(xy)
  print("Mixing Probabilities")
  print(pi_k)
  print("New Betas")
  print(beta.new)
  # caluclate new pooled variance
  sigma2 <- sum(gamma_ik[, 1] * (y - x %*% beta.new[, 1])^2 + gamma_ik[, 2] * (y - x %*% beta.new[, 2])^2) / nrow(xy)
  sigma <- sqrt(sigma2)
  print(sigma)
  error <- max(abs(betas-beta.new))  # see how much the betas changed between iterations
  print("Error")
  print(error)
  betas <- beta.new
  pi <- pi_k
  print(i+1)
  
  idxs <- which.max(gamma_ik[,1])
  abline(betas[1,1],betas[1,2])
  
  # get cluster assignments (get the column index with the max gamma, that will be the cluster assignment)
  clusters_plot <- apply(gamma_ik, 1, function(x) which.max(x))
  
  # split the data according to posterior probabilities (use gammas to get the new clusters)
  splitted.xy1<-xy[clusters_plot==1,]
  splitted.xy2<-xy[clusters_plot==2,]
  
  predicted1<-splitted.xy1[,1:2]%*%betas[,1]  # Do OLS (y = X*Betas)
  predicted2<-splitted.xy2[,1:2]%*%betas[,2]  # Do OLS (y = X*Betas)
  
  plot(x[,2],y,col = clusters_plot, main="Mixture Regression Cluster Assignments", xlab = "x")
  lines(splitted.xy1[,2], predicted1)
  lines(splitted.xy2[,2], predicted2, col = "red")
  
  num_rows1 <- nrow(splitted.xy1)
  num_rows2 <- nrow(splitted.xy2)
  
  #splitted.xyall <- rbind(splitted.xy1,splitted.xy2)
}
