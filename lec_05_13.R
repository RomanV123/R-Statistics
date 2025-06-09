
####### K-sample permutation test

# F statistic

X1 = c(1.5, 2.4)
X2 = c(2.3, 3.1, 3.5)
X3 = c(0.6, 1.8)

n1 = 2
n2 = 3
n3 = 2

Fstat = function(X1, X2, X3){
  X = c(X1, X2, X3)
  Xbar = mean(X)
  X1bar = mean(X1)
  X2bar = mean(X2)
  X3bar = mean(X3)
  n1 = length(X1)
  n2 = length(X2)
  n3 = length(X3)
  N = n1+n2+n3
  SST = n1*(X1bar-Xbar)^2 + n2*(X2bar-Xbar)^2 + n3*(X3bar-Xbar)^2
  SSE = sum((X1-X1bar)^2) + sum((X2-X2bar)^2) + sum((X3-X3bar)^2)
  (SST/(3-1))/(SSE/(N-3))
}

Fstat.obs = Fstat(X1, X2, X3)


N = n1+n2+n3
X = c(X1,X2,X3)
B = factorial(N)/(factorial(n1)*factorial(n2)*factorial(n3))

Fstatv = rep(0,B)

count = 1
index1 = combn(1:N,n1)
B1 = dim(index1)[2]
for (i in 1:B1){
  id1 = index1[,i]
  id23 = (1:N)[-index1[,i]]
  index2 = combn(id23,n2)
  B2 = dim(index2)[2]
  for (j in 1:B2){
    id2 = index2[,j]
    id3 = (1:N)[-c(id1,id2)]
    X1new = X[id1]
    X2new = X[id2]
    X3new = X[id3]
    Fstatv[count] = Fstat(X1new,X2new,X3new)
    count = count+1
  }
} 

length(which(Fstatv>=Fstat.obs))/B


# Kruskal-Wallis statistic

myrank = rank(X)
R1 = myrank[1:2]
R2 = myrank[3:5]
R3 = myrank[6:7]

KW = function(R1, R2, R3){
  R1bar = mean(R1)
  R2bar = mean(R2)
  R3bar = mean(R3)
  Rbar = mean(c(R1,R2,R3))
  n1 = length(R1)
  n2 = length(R2)
  n3 = length(R3)
  N = n1+n2+n3
  (12/(N*(N+1)))*(n1*(R1bar-Rbar)^2+n2*(R2bar-Rbar)^2+n3*(R3bar-Rbar)^2)
}

KW.obs = KW(R1,R2,R3)

KWv = rep(0,B)

count = 1
index1 = combn(1:N,n1)
B1 = dim(index1)[2]
for (i in 1:B1){
  id1 = index1[,i]
  id23 = (1:N)[-index1[,i]]
  index2 = combn(id23,n2)
  B2 = dim(index2)[2]
  for (j in 1:B2){
    id2 = index2[,j]
    id3 = (1:N)[-c(id1,id2)]
    R1new = myrank[id1]
    R2new = myrank[id2]
    R3new = myrank[id3]
    KWv[count] = KW(R1new,R2new,R3new)
    count = count+1
  }
} 

length(which(KWv>=KW.obs))/B


# number of random assignments

X1 = c(6.08, 22.29, 7.51, 34.36, 23.68)
X2 = c(30.45, 22.71, 44.52, 31.47, 36.81)
X3 = c(32.04, 28.03, 32.74, 23.84, 29.64)
n1 = n2 = n3 = 5

N = n1+n2+n3
factorial(N)/(factorial(n1)*factorial(n2)*factorial(n3))

# get a random permutation
sample(1:N, N)

B = 50000

Fstatv = rep(0,B)

for (i in 1:B){
  ids = sample(1:N, N)
  X1new = X[ids[1:n1]]
  X2new = X[ids[(n1+1):(n1+n2)]]
  X3new = X[ids[(n1+n2+1):N]]
  Fstatv[i] = Fstat(X1new,X2new,X3new)
}

length(which(Fstatv>=Fstat.obs))/B


n1 = 10
n2 = 10
n3 = 10

N = n1+n2+n3
factorial(N)/(factorial(n1)*factorial(n2)*factorial(n3))

# get a random permutation
sample(1:N, N)



####### paired-comparison permutation test

n = 6

signs = matrix(1,n,2^n)

count = 1
for (m in 1:n){
  indices = combn(1:n,m)
  temp = dim(indices)[2]
  for (j in 1:temp){
    signs[indices[,j],j+count] = -1
  }
  count = count+temp
}

B = 2^n


Dv = c(11,-3,12,-6,4,5)

Dmean.obs = mean(Dv)

Dmeans = rep(0,B)
for (i in 1:B){
  Dmeans[i] = mean(Dv*signs[,i])
}

length(which(Dmeans>=Dmean.obs))/B

