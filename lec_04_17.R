###############################################
## power approximation by simulation (t-test)
###############################################
# mu_0 = 0
# H_0: mu = 0 vs H_a: mu != 0, alpha=0.05
# approximate power mu = 0.1
# true distribution of Y is N(0.1, 1)
# n = 100

# can change n, mu0, mu, sigma values

n = 100
mu0 = 0
mu = 0.2
sigma = 1

B = 10000
count = 0
for (i in 1:B){
  Y = rnorm(n,mu,sigma)
  tstar = (mean(Y)-mu0)/(sd(Y)/sqrt(n))
  if (abs(tstar)>qt(0.975,n- 1)) count = count+1
}
count/B

#####################################################
## power approximation by simulation (Binomial test)
#####################################################
# mu_0 = 0
# H_0: mu = 0 vs H_a: mu != 0, alpha=0.05
# approximate power mu = 0.1
# true distribution of Y is N(0.1, 1)
# n = 100

# can change n, mu0, mu, sigma values

n = 100
alpha = 0.05
0.5*n-0.5+qnorm(alpha/2)*sqrt(0.25*n)
k1 = 39
0.5*n+0.5+qnorm(1-alpha/2)*sqrt(0.25*n)
k2 = 61
# Po(X<=k1) + Po(X>=k2)
pbinom(k1,n,0.5)+1-pbinom(k2-1,n,0.5)

mu0 = 0
mu = 0.2
sigma = 1

B = 10000
count = 0
for (i in 1:B){
  Y = rnorm(n,mu,sigma)
  X = length(which(Y<mu0))
  if (X<=k1 || X>=k2) count = count+1
}
count/B

#####################################
## analytic approximation for power  
#####################################

## population distribution: N(mu,sigma^2)
## H0: mu=mu_0; Ha: mu\neq mu_0

# delta = (mu-mu_0)/sigma

# t-test
power.t = function(delta, n=1000, alpha=0.05){
  Cn = qt(1-alpha/2,n-1)
  1-pnorm(Cn-sqrt(n)*delta)+pnorm(-Cn-sqrt(n)*delta)
}

deltav = seq(-0.6,0.6,0.001)
pow1 = power.t(deltav, n=100)
pow2 = power.t(deltav, n=200)
pow3 = power.t(deltav, n=1000)

plot(deltav, pow1, type="l",ylim=c(0,1), xlab="Delta")
points(deltav, pow2, type="l", col="red")
points(deltav, pow3, type="l", col="green")

# Binomial test

power.b = function(delta, n=1000, alpha=0.05){
  p = pnorm(delta)
  pnorm(n*(0.5-p)/sqrt(n*p*(1-p))+qnorm(alpha/2)*sqrt(0.25/(p*(1-p)))) + 1-pnorm(n*(0.5-p)/sqrt(n*p*(1-p))+qnorm(1-alpha/2)*sqrt(0.25/(p*(1-p))))
}

deltav = seq(-1,1,0.001)
powb1 = power.b(deltav, n=100)
powb2 = power.b(deltav, n=200)
powb3 = power.b(deltav, n=1000)

plot(deltav, powb1, type="l",ylim=c(0,1), xlab="Delta")
points(deltav, powb2, type="l", col="red")
points(deltav, powb3, type="l", col="green")

# compare the power function between binomial test and the t-test

plot(deltav, powb1, type="l",ylim=c(0,1), xlab="Delta")
points(deltav, powt1, type="l", col="green")


