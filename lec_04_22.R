
mydata = c(37, 49, 55, 57, 23, 31, 46)

## Step 1: Compute test statistic for the observed data
Dobs = mean(mydata[1:4]) - mean(mydata[5:7])

## Step 2: Obtain all possible assignments.
indices = combn(1:7,4)
B = dim(indices)[2]

# For each assignment, compute the test statistic
Ds = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  Ds[i] = mean(mydata[ids]) - mean(mydata[-ids])
}

## Step 3: Compute p-value
length(which(Ds>=Dobs))/B


m = 20 
n = 20
choose(m+n,m)

## draw one random assignment
sample(1:(m+n),m)

