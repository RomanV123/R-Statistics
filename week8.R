# 1.
# (1)
X1 = c(78, 65, 63)
X2 = c(71, 66, 56)
X3 = c(57, 61, 58)
n1 = n2 = n3 = 3
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

Fstat.obs = Fstat(X1, X2, X3) # 1.754522

N = n1+n2+n3
X = c(X1,X2,X3)
B = factorial(N)/(factorial(n1)*factorial(n2)*factorial(n3)) # 1680

Fstatv = rep(0,B)

count = 1
index1 = combn(1:N,n1) # all possible assignments for group 1
B1 = dim(index1)[2]
for (i in 1:B1){
  id1 = index1[,i]
  id23 = (1:N)[-index1[,i]]
  index2 = combn(id23,n2) # all possible assignments for group 2 condition on the assignments for group 1
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

length(which(Fstatv>=Fstat.obs))/B # 0.2380952: not reject H0

# Kruskal-Wallis statistic

myrank = rank(X)
R1 = myrank[1:3]
R2 = myrank[4:6]
R3 = myrank[7:9]

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

KW.obs = KW(R1,R2,R3) # 2.755556

KWv = rep(0,B)

count = 1
index1 = combn(1:N,n1) # all possible assignments for group 1
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

length(which(KWv>=KW.obs))/B # 0.275: not reject H0

# 2.
n = 8
X = c(86, 70, 40, 65, 80, 78, 55, 20)
Y = c(75, 50, 50, 40, 30, 65, 40, 25)
Dv = X - Y
signs = matrix(1,n,2^n)
count = 1
for (m in 1:n){
  indices = combn(1:n,m) # pick the pairs having -1 sign
  temp = dim(indices)[2]
  for (j in 1:temp){
    signs[indices[,j],j+count] = -1
  }
  count = count+temp
}

B = 2^n
Dmean.obs = mean(Dv)
Dmeans = rep(0,B)
for (i in 1:B){
  Dmeans[i] = mean(Dv*signs[,i])
}

length(which(abs(Dmeans)>=abs(Dmean.obs)))/B # 0.0547: not reject H0

