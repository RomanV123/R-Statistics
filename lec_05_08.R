
## Kolmogorov-Smirnov test

X = c(37,49,55,57)
Y = c(23,31,46)

mydata = c(X,Y)
m = 4
n = 3

## Step 1: Compute test statistic for the observed data

Fxhat = ecdf(X)
Fyhat = ecdf(Y)

plot(Fxhat, main="Empirical CDF", xlim=c(min(mydata),max(mydata)))
points(sort(X), c(0:(m-1)/m))
lines(Fyhat, col="red")
points(sort(Y), c(0:(n-1)/n), col="red")


KS.obs = max(abs(Fxhat(mydata)-Fyhat(mydata)))


## Step 2: Obtain all possible assignments. For each assignment, compute the test statistic

indices = combn(1:(m+n),m)

B = dim(indices)[2]
KS = rep(0,B)

for (i in 1:B){
  ids = indices[,i]
  Fxhat = ecdf(mydata[ids])
  Fyhat = ecdf(mydata[-ids])
  KS[i] = max(abs(Fxhat(mydata)-Fyhat(mydata)))
}

## Step 3: Compute p-value

length(which(KS>=KS.obs))/B


####
m = n = 100
N = m+n
X = rnorm(m,0,1)
#Y = rnorm(n,0.25,1)
Y = rnorm(n,0,1.5)

# Ho: mu_x = mu_y vs Ha: mu_x != mu_y

# Wilcoxon rank-sum test
mydata = c(X,Y)
myrank = rank(mydata)
W = sum(myrank[1:m])

choose(m+n,m)

EW = m*(N+1)/2
VarW = m*n*(N+1)/12

z = (W-EW)/sqrt(VarW)

2*(1-pnorm(z))

B = 10000
Ws = rep(0,B)
for (i in 1:B){
  Xids = sample(1:N,m)
  Ws[i] = sum(myrank[Xids])
}

length(which(abs(Ws-EW)<=abs(W-EW)))/B

# K-S test
Fxhat = ecdf(X)
Fyhat = ecdf(Y)

plot(Fxhat, main="Empirical CDF", xlim=c(min(mydata),max(mydata)))
points(sort(X), c(0:(m-1)/m))
lines(Fyhat, col="red")
points(sort(Y), c(0:(n-1)/n), col="red")


KS.obs = max(abs(Fxhat(mydata)-Fyhat(mydata)))

B = 10000
KS = rep(0,B)

for (i in 1:B){
  ids = sample(1:N,m)
  Fxhat = ecdf(mydata[ids])
  Fyhat = ecdf(mydata[-ids])
  KS[i] = max(abs(Fxhat(mydata)-Fyhat(mydata)))
}

length(which(KS>=KS.obs))/B


