
###### Sign test

Dv = c(11,-3,12,-6,4,5)

SNplus.obs = length(which(Dv>0))

## use the paired-comparison permutation to get the exact p-value

SNplusv = rep(0,B)
for (i in 1:B){
  SNplusv[i] = length(which(signs[,i]>0))
}

# Ha: mu_X > mu_Y
length(which(SNplusv>=SNplus.obs))/B


## make use of the Binomial distribution to get the exact p-value

# P(SN+ >= 4) = 1 - P(SN+ <=3)
1 - pbinom(3,6,0.5)

## normal approximation
Z = (3.5-0.5*6)/sqrt(0.25*6)
1-pnorm(Z)


### larger sample size
Dv = read.table("Dv.txt")[,1]

n = length(Dv)

sort(Dv)

# some of the differences are 0
# only consider those non-zero differences
n1 = length(which(Dv!=0))
n1
SNplus.obs = length(which(Dv>0))

## make use of the Binomial distribution to get the exact p-value
# Ha: mu_X > mu_Y
# P(SN+ >= 29) = 1 - P(SN+ <= 28)
1-pbinom(28,45,0.5)

## normal approximation
Z = (29-0.5-0.5*45)/sqrt(0.25*45)
1-pnorm(Z)


## through random paired-comparison permutations (randomly assign pluses and minuses)

B = 100000
SNplusv = rep(0,B)

n = n1

for (i in 1:B){
  signs = sample(c(-1,1),n,replace=T)
  SNplusv[i] = length(which(signs>0))
}

length(which(SNplusv>=SNplus.obs))/B


####### Bootstrap 

X = c(7,11,15,16,20,22,24,25,29,33,34,37,41,42,49,57,66,71,84,90)

n = length(X)

Xbar = mean(X)
Xsd = sd(X)

# bootstrap sample
Xnew = sample(X,n,replace=TRUE)

mean(Xnew)
sd(Xnew)

## Estimate MSE for sample mean

B = 10000
Xbarv = rep(0,B)

for (b in 1:B){
  Xnew = sample(X,n,replace=TRUE)
  Xbarv[b] = mean(Xnew)
}

MSE = sum((Xbarv-Xbar)^2)/B

# standard error of Xbar
sd(X)/sqrt(n)

sqrt(MSE)

## Estimate MSE for sample standard deviation
B = 10000
Xsdv = rep(0,B)

for (b in 1:B){
  Xnew = sample(X,n,replace=TRUE)
  Xsdv[b] = sd(Xnew)
}

MSE = sum((Xsdv-Xsd)^2)/B

