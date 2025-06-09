
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


