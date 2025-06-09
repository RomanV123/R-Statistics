
# Check the coverage of CIs for population mean with unknown standard deviation:
simN = 1000 # number of trials
n = 20 # number of observations per trial
alpha = 0.05 # 95% CI
population.mean = 1 # true population mean mu
flag = 0
for(i in 1:simN){
  sample.X = rnorm(n, mean = population.mean, sd = 2) # generate samples from Normal(1,4)
  sample.mean = mean(sample.X)
  sample.sd = sd(sample.X)
  lci = sample.mean - qt(1-alpha/2, df = n-1) * (sample.sd/sqrt(n))
  uci = sample.mean + qt(1-alpha/2, df = n-1) * (sample.sd/sqrt(n))
  if((lci<=population.mean) & (uci>=population.mean)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean
# We would expect the proportion is around 95% (1-alpha).


# Exponential distribution
simN = 1000 # number of trials
n = 20 # number of samples per trial
alpha = 0.05 # 95% CI
population.mean = 1 # true population mean mu
flag = 0
for(i in 1:simN){
  sample.X = rexp(n, rate = population.mean) # generate samples from Exp(1)
  sample.mean = mean(sample.X)
  sample.sd = sd(sample.X)
  lci = sample.mean - qt(1-alpha/2, df = n-1) * (sample.sd/sqrt(n))
  uci = sample.mean + qt(1-alpha/2, df = n-1) * (sample.sd/sqrt(n))
  if((lci<=population.mean) & (uci>=population.mean)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean



## t-pivot bootstrap

bootstrapCI = function(X,alpha,B=1000){
  Xbar = mean(X)
  Xsd = sd(X)
  n = length(X)
  term = rep(0,B)
  for (i in 1:B){
    Xnew = sample(X,n,replace=T)
    term[i] = (mean(Xnew)-Xbar)/(sd(Xnew)/sqrt(n))
  }
  lci = Xbar - quantile(term,1-alpha/2)*Xsd/sqrt(n)
  uci = Xbar - quantile(term,alpha/2)*Xsd/sqrt(n)
  return(c(lci,uci))
}

simN = 1000 # number of trials
n = 20 # number of samples per trial
alpha = 0.05 # 95% CI
population.mean = 1 # true population mean mu
flag = 0
for(i in 1:simN){
  sample.X = rexp(n, rate = population.mean) # generate samples from Exp(1)
  temp = bootstrapCI(sample.X, alpha=alpha)
  lci = temp[1]
  uci = temp[2]
  if((lci<=population.mean) & (uci>=population.mean)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean





# Check the coverage of CIs for population standard deviation:
simN = 1000 # number of trials
n = 20 # number of samples per trial
alpha = 0.05 # 95% CI
population.mean = 1 # true population mean mu
population.sd = 2
flag = 0
for(i in 1:simN){
  sample.X = rnorm(n, mean = population.mean, sd = population.sd) # generate samples from Normal(1,4)
  sample.var = var(sample.X)
  lci = sqrt((n-1)*sample.var/qchisq(1-alpha/2,n-1))
  uci = sqrt((n-1)*sample.var/qchisq(alpha/2,n-1))
  if((lci<=population.sd) & (uci>=population.sd)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean
# We would expect the proportion is around 95% (1-alpha).




# Check the coverage of CIs for population mean with unknown standard deviation:
simN = 1000 # number of trials
n = 5000 # number of samples per trial
alpha = 0.05 # 95% CI
flag = 0
for(i in 1:simN){
  sample.X = rexp(n, rate = 1) # generate samples from Exp(1): population mean is 1, population variance is 1
  sample.var = var(sample.X)
  lci = sqrt((n-1)*sample.var/qchisq(1-alpha/2,n-1))
  uci = sqrt((n-1)*sample.var/qchisq(alpha/2,n-1))
  if((lci<=1) & (uci>=1)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean
# We would expect the proportion is around 95% (1-alpha).



bootstrapCIsd = function(X,alpha,B=1000){
  Xvar = var(X)
  n = length(X)
  term = rep(0,B)
  for (i in 1:B){
    Xnew = sample(X,n,replace=T)
    term[i] = (n-1)*var(Xnew)/Xvar
  }
  lci = sqrt((n-1)*Xvar/quantile(term,1-alpha/2))
  uci = sqrt((n-1)*Xvar/quantile(term,alpha/2))
  return(c(lci,uci))
}

simN = 1000 # number of trials
n = 500 # number of samples per trial
alpha = 0.05 # 95% CI
flag = 0
for(i in 1:simN){
  sample.X = rexp(n, rate = 1) # generate samples from Exp(1)
  temp = bootstrapCIsd(sample.X, alpha=alpha)
  lci = temp[1]
  uci = temp[2]
  if((lci<=1) & (uci>=1)){
    flag = flag + 1 # count the number of the trials having CIs that covers the true population mean
  }
}
flag/simN # proportion of simulations having CIs that covers the true population mean





