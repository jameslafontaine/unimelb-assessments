library(MASS)

plot_model = function(model_name, betahat0, betahat1, inverse_link) {
  plot(damage/6 ~ temp, orings, main=model_name, xlim=c(25,85), ylim=c(0,1),
       xlab="Temperature", ylab="Prob of damage")
  x = seq(25,85,1)
  lines(x, inverse_link(betahat0 + betahat1*x), col="red")
}

ilogit = function(x) exp(x)/(1+exp(x))

iprobit = function(x) pnorm(x)

## FOR CHECKING ANSWERS, CANT USE GLM IN QUESTION 1 WORKING OUT ##
probit_model = glm(cbind(damage, 6-damage) ~ temp, family=binomial(link="probit"),
                  orings)
summary(probit_model)

logit_model = glm(cbind(damage, 6-damage) ~ temp, family=binomial(link="logit"),
                   orings)
summary(logit_model)

plot(logit_model)

plot_model("Probit model", probit_model$coefficients[1], probit_model$coefficients[2], iprobit)

plot_model("Logit model", logit_model$coefficients[1], logit_model$coefficients[2], ilogit)




## Question 1 ##

# 1a (find MLE of Betas using probit link function)
library(faraway)
data(orings)

logL = function(beta, orings) {
  eta = cbind(1, orings$temp) %*% beta
  return (sum(orings$damage * log(pnorm(eta)) + (6 - orings$damage) * log(1 - pnorm(eta)) ))
}

(betahat = optim(c(10, -.1), logL, orings=orings, control=list(fnscale=-1))$par)

# check optim probit model against glm probit model
#plot_model("Optim Probit Model", betahat[1], betahat[2], iprobit)




#1b (Compute 95% CIs for beta estimates)
iprobit = function(x) pnorm(x)
phat = iprobit(betahat[1] + orings$temp*betahat[2])
I11 = 3/pi * sum(dexp(qnorm(phat)^2) / (phat*(1-phat)))
I12 = 3/pi * sum(orings$temp * dexp(qnorm(phat)^2) / (phat*(1-phat)))
I22 = 3/pi * sum(orings$temp^2 * dexp(qnorm(phat)^2) / (phat*(1-phat)))

Iinv = solve(matrix(c(I11, I12, I12, I22), 2, 2))

betahat[1] + c(-1,1)*qnorm(0.975)*sqrt(Iinv[1,1])

betahat[2] + c(-1,1)*qnorm(0.975)*sqrt(Iinv[2,2])

confint(probit_model)

# 1c (Likelihood ratio test for significance of beta1)


logL.F = function(beta, orings) {
  eta = cbind(1, orings$temp) %*% beta
  return (sum(orings$damage * log(pnorm(eta)) + (6 - orings$damage) * log(1 - pnorm(eta)) ))
}

logL.R = function(beta, orings) {
  eta = beta
  return (sum(orings$damage * log(pnorm(eta)) + (6 - orings$damage) * log(1 - pnorm(eta)) ))
}


(betahat.F = optim(c(10, -.1), logL.F, orings=orings, control=list(fnscale=-1))$par)

(betahat.R = optim(c(5), logL.R, orings=orings, control=list(fnscale=-1))$par)

(LR = -2*(logL.R(betahat.R, orings) - logL.F(betahat.F, orings)))

pchisq(LR, df=1, lower=FALSE)

# p-value < 0.05, reject H0: beta1 = 0

# 1d (Estimate probability of damage when temperature equals 31 (include 95% CI))

si2 = matrix(c(1, 31), 1, 2) %*% Iinv %*% matrix(c(1, 31), 2, 1)

etahat = betahat[1] + betahat[2]*31

eta_l = etahat - qnorm(0.975)*sqrt(si2)

eta_r = etahat + qnorm(0.975)*sqrt(si2)

c(eta_l, eta_r)

iprobit(etahat)

c(iprobit(eta_l), iprobit(eta_r))

# 1e (plots comparing fitted probit model to glm logit model)
ilogit = function(x) exp(x)/(1+exp(x))

iprobit = function(x) pnorm(x)

log_mod = glm(cbind(damage, 6-damage) ~ temp, family=binomial(link="logit"),
                  orings)

  plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim=c(0,1),
       xlab="Temperature", ylab="Prob of damage")
  x = seq(25,85,1)
  lines(x, ilogit(log_mod$coefficients[1] + log_mod$coefficients[2]*x), col="red", lwd=2)
  lines(x, iprobit(betahat[1] + betahat[2]*x), col="blue", lwd=2)

  legend(x = "topright",                            
         legend = c("GLM Logit", "Fitted Probit"),  
         lty = c(1),                               
         col = c("red", "blue"),          
         lwd = 2)   


# 2a (estimate the amount of increase in the log(odds) when the bmi increases by 7)
library(faraway)
missing = with(pima, missing <- glucose==0 | diastolic==0 | triceps==0 | bmi==0)
pima_subset = pima[!missing, c(6,9)]
str(pima_subset)

pima_mdl = glm(test ~ bmi, family=binomial(link="logit"),
                  pima_subset)

phat = ilogit(pima_mdl$coefficients[2]*7)

log_odds = logit(phat)

as.numeric(log_odds)

# 2b (Compute a 95% CI for the log odds estimate)

phat_l = ilogit((pima_mdl$coefficients[2]*7 - qnorm(0.975) * summary(pima_mdl)$coefficients[2, 2] * 7))
                
phat_r = ilogit((pima_mdl$coefficients[2]*7 + qnorm(0.975) * summary(pima_mdl)$coefficients[2, 2] * 7))

CI_logodds = c(logit(phat_l), logit(phat_r)) 

as.numeric(CI_logodds)
