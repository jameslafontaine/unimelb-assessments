library(MASS)



## FOR CHECKING ANSWERS




## Question 1 ##

X = scan(file="assignment3_prob1_2023.txt", what=double())

length(X)

hist(X)

# 1d (Implement the EM algorithm and obtain the MLE of the parameters)

# w.init : initial value for pi
# lambda.init : initial value for lambda
# epsilon : If the incomplete log-likelihood has changed by less than epsilon, 
# EM will stop.
# max.iter : maximum number of EM-iterations
mixture.EM <- function(X, w.init, lambda.init, epsilon=1e-5, max.iter=100) {
  
  w.curr = w.init
  lambda.curr = lambda.init
  
  # store incomplete log-likehoods for each iteration
  log_liks = c()
  
  # compute incomplete log-likehoods using initial values of parameters. 
  log_liks = c(log_liks, compute.log.lik(X, w.curr, lambda.curr)$ill)
  
  # set the change in incomplete log-likelihood with 1
  delta.ll = 1
  
  # number of iteration
  n.iter = 1
  
  # If the log-likelihood has changed by less than epsilon, EM will stop.   
  while((delta.ll > epsilon) & (n.iter <= max.iter)){
    
    # run EM step
    EM.out = EM.iter(X, w.curr, lambda.curr)
    
    # replace the current value with the new parameter estimate
    w.curr = EM.out$w.new
    lambda.curr = EM.out$mu.new
    
    # incomplete log-likehoods with new parameter estimate 
    log_liks = c(log_liks, compute.log.lik(X, w.curr, lambda.curr)$ill)
    
    # compute the change in incomplete log-likelihood 
    delta.ll = log_liks[length(log_liks)]  - log_liks[length(log_liks)-1]
    
    # increase the number of iteration 
    n.iter = n.iter + 1
  }
  return(list(w.curr=w.curr, lambda.curr=lambda.curr, log_liks=log_liks))
}


EM.iter <- function(X, w.curr, lambda.curr) {
  
  # E-step: compute E_{Z|X,\theta_0}[I(Z_i = k)]
  
  # for each sample $X_i$, compute $P(X_i, Z_i=k)$ 
  prob.x.z = compute.prob.x.z(X, w.curr, lambda.curr)$prob.x.z
  
  # compute P(Z_i=k | X_i)
  P_ik = prob.x.z / rowSums(prob.x.z)
  
  # M-step
  w.new = colSums(P_ik)/sum(P_ik)  # sum(P_ik) is equivalent to sample size 
  mu.new = colSums(P_ik*X)/colSums(P_ik)
  
  return(list(w.new=w.new, lambda.new=lambda.new))
}


#Now we write a function to compute the incomplete log-likelihood, assuming the parameters are known.
#$$\ell(\theta) = \sum_{i=1}^n \log \left( \sum_{k=1}^3 \pi_k f(x_i;\mu_k, \sigma_k^2=2) \right )$$
  
# Compute incomplete log-likehoods
compute.log.lik <- function(X, w.curr, lambda.curr) {
  
  # for each sample $X_i$, compute $P(X_i, Z_i=k)$
  prob.x.z = compute.prob.x.z(X, w.curr, lambdau.curr)$prob.x.z
  
  # incomplete log-likehoods
  ill = sum(log(rowSums(prob.x.z)))
  
  return(list(ill=ill))
}

# for each sample $X_i$, compute $P(X_i, Z_i=k)$
compute.prob.x.z <- function(X, w.curr, lambda.curr) {
  
  # for each sample $X_i$, compute $P(X_i, Z_i=k)$. Store these values in the columns of L:
  L = matrix(NA, nrow=length(X), ncol= length(w.curr))
  for(k in seq_len(ncol(L))) {
    L[, k] = dpois(X, lambda=lambda.curr[k])*w.curr[k]
  }
  
  return(list(prob.x.z=L))
}


# Apply the EM algorithm to the first simulated data set
#Run EM algorithm with different initial values and check that the incomplete log-likelihoods increases at each step by plotting them. 

EM1 <- mixture.EM(X.1, w.init=c(0.3,0.3, 0.4), lambda.init=c(3, 20, 35), epsilon=1e-5, max.iter=100)
ee = EM1
print(paste("Estimate pi = (", round(ee$w.curr[1],2), ",",
            round(ee$w.curr[2],2), ",",
            round(ee$w.curr[3],2), ")", sep=""))
print(paste("True pi = (", pi.true.1[1], ",", pi.true.1[2], ",", pi.true.1[3],")", sep=""))
print(paste("Estimate lambda = (", round(ee$lambda.curr[1],2), ",",
            round(ee$lambda.curr[2],2), ",",
            round(ee$lambda.curr[3],2), ")", sep=""))
print(paste("True lambda = (", lambda.true.1[1], ",", lambda.true.1[2], ",", lambda.true.1[3],")", sep=""))
plot(ee$log_liks, ylab='incomplete log-likelihood', xlab='iteration')



EM2 <- mixture.EM(X.1, w.init=c(0.1,0.2, 0.7), lambda.init=c(5, 25, 40), epsilon=1e-5, max.iter=100)
ee = EM2
print(paste("Estimate pi = (", round(ee$w.curr[1],2), ",",
            round(ee$w.curr[2],2), ",",
            round(ee$w.curr[3],2), ")", sep=""))
print(paste("True pi = (", pi.true.1[1], ",", pi.true.1[2], ",", pi.true.1[3],")", sep=""))
print(paste("Estimate lambda = (", round(ee$lambda.curr[1],2), ",",
            round(ee$lambda.curr[2],2), ",",
            round(ee$lambda.curr[3],2), ")", sep=""))
print(paste("True lambda = (", lambda.true.1[1], ",", lambda.true.1[2], ",", lambda.true.1[3],")", sep=""))
plot(ee$log_liks, ylab='incomplete log-likelihood', xlab='iteration')


#Check which estimators have the highest incomplete log-likelihood.

EM1$log_liks[length(EM1$log_liks)]
EM2$log_liks[length(EM2$log_liks)]


#Estimators from the two EM runs have (equally) highest incomplete log-likelihoods. You can see that the estimators from the EM runs are the same if labels for clusters are switched. So it doesn’t matter which estimators we choose. I will choose the estimators from the first EM run - $\hat\pi_1 = 0.2211658, \hat\pi_2 = 0.2854424, \hat\mu_1 = -9.99961961, \hat\mu_2 = -0.03233427, \hat\mu_3 = 6.05589298$ which are similar to true values of the parameters.