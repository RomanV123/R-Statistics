mydata = c(37, 49, 55, 57, 23, 31, 46)

# H0: mu_1 = mu_2 vs Ha: mu_1 > mu_2

### Mann-Whitney test
## Step 1: Compute test statistic for the observed data
X = mydata[1:4]
Y = mydata[5:7]
m = 4
n = 3
U = 0
for (i in 1:m){
  for (j in 1:n){
    if (X[i]<Y[j]) U = U+1
  }
}
U

getU = function(X, Y){
  U = 0
  m = length(X)
  n = length(Y)
  for (i in 1:m){
    for (j in 1:n){
      if (X[i]<Y[j]) U = U+1
    }
  }
  U
}

Uobs = getU(X,Y)

## Step 2: Obtain all possible assignments.
indices = combn(1:7,4)
B = dim(indices)[2]

# For each assignment, compute the test statistic
U = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  U[i] = getU(mydata[ids], mydata[-ids])
}

U
sort(U)

## Step 3: Compute p-value
length(which(U<=Uobs))/B


### Confidence interval

Fxhat = ecdf(X)
Fyhat = ecdf(Y)

plot(Fxhat, main="Empirical CDF", xlim=c(20,60))
points(sort(X), c(0:(m-1)/m))
lines(Fyhat, col="red")
points(sort(Y), c(0:(n-1)/n), col="red")

# 90% C.I 

0.1*35

# P(1 <= U <= 10) = 32/35 = 0.914 > 0.9

a = 1
b = 11

pwd = rep(0,m*n)
id = 1
for (i in 1:m){
  for (j in 1:n){
    pwd[id] = X[i] - Y[j]
    id = id+1
  }
}

pwd
pwd.s = sort(pwd)

pwd.s[a]
pwd.s[b]





### another example for constructing 90% confidence interval for Delta
# F_X(x) = F_Y(x-Delta)

mydata = c(16.21, 18.37, 21.18, 15.74, 17.84, 18.26, 21.42, 19.52, 23.97, 19.72, 20.84, 21.96)

m = 6
n = 6

X = mydata[1:6]
Y = mydata[7:12]

Fxhat = ecdf(X)
Fyhat = ecdf(Y)

plot(Fxhat, main="Empirical CDF", xlim=c(min(mydata),max(mydata)))
points(sort(X), c(0:(m-1)/m))
lines(Fyhat, col="red")
points(sort(Y), c(0:(n-1)/n), col="red")


## to obtain the distribution of U

indices = combn(1:(m+n),m)
B = dim(indices)[2]

# For each assignment, compute the test statistic
U = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  U[i] = getU(mydata[ids], mydata[-ids])
}

U
sort(U)

a = sort(U)[0.05*B]
b = sort(U)[0.95*B]+1

a 
b

length(which(U>=8 & U<=28))/B
length(which(U>=9 & U<=28))/B
length(which(U>=8 & U<=27))/B

pwd = rep(0,m*n)
id = 1
for (i in 1:m){
  for (j in 1:n){
    pwd[id] = X[i] - Y[j]
    id = id+1
  }
}

pwd
pwd.s = sort(pwd)

pwd.s[a]
pwd.s[b]



### We may also use the relationship between U and S1R 
# to obtain the distribution of S1R

indices = combn(1:(m+n),m)
B = dim(indices)[2]

# For each assignment, compute the test statistic S1R
myrank = 1:12
S1R = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  S1R[i] = sum(myrank[ids])
}

S1R
sort(S1R)

c1 = sort(S1R)[0.05*B]
c2 = sort(S1R)[0.95*B]

c1 # 29
c2 # 49

length(which(S1R>=c1 & S1R<=c2))/B
length(which(S1R>=(c1+1) & S1R<=c2))/B
length(which(S1R>=c1 & S1R<=(c2-1)))/B

# S1R = TR - n(n+1)/2 - U
# c(m,n) = TR - n(n+1)/2 = 12*13/2 - 6*7/2 = 57
# c1 = c(m,n) -(b-1)
# c2 = c(m,n) - a

a = 57-c2
b = 57-c1+1


a = 8
b = 29

pwd = rep(0,m*n)
id = 1
for (i in 1:m){
  for (j in 1:n){
    pwd[id] = X[i] - Y[j]
    id = id+1
  }
}

pwd
pwd.s = sort(pwd)

pwd.s[8]
pwd.s[29]




