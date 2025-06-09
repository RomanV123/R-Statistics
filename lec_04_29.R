
mydata = c(37, 49, 55, 57, 23, 31, 46)

### Mean difference
## Step 1: Compute test statistic for the observed data
Dobs = mean(mydata[1:4]) - mean(mydata[5:7])

# Dobs = median(mydata[1:4]) - median(mydata[5:7]) # median difference
# Sobs = sum(mydata[1:4]) # sum of observations in sample 1

## Step 2: Obtain all possible assignments.
indices = combn(1:7,4)
B = dim(indices)[2]

# For each assignment, compute the test statistic
D = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
   D[i] = mean(mydata[ids]) - mean(mydata[-ids])
  # D[i] = median(mydata[ids]) - median(mydata[-ids])
}

S = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  S[i] = sum(mydata[ids])
}

## Step 3: Compute p-value
length(which(D>=Dobs))/B

# length(which(S>=Sobs))/B


###########################
# tests based on ranks
###########################
mydata = c(37, 49, 55, 57, 23, 31, 46)

myrank = rank(mydata)

### Mean rank difference
## Step 1: Compute test statistic for the observed data
DRobs = mean(myrank[1:4]) - mean(myrank[5:7])

## Step 2: Obtain all possible assignments.
indices = combn(1:7,4)
B = dim(indices)[2]

# For each assignment, compute the test statistic
DR = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  DR[i] = mean(myrank[ids]) - mean(myrank[-ids])
}

## Step 3: Compute p-value for Ha: mu_1 > mu_2
length(which(DR>=DRobs))/B


# mydata = c(10, 21, 30, 96, 14, 1213130, 5)
# myrank = rank(mydata)

### Test statistic: sum of ranks in sample 1
## Step 1: Compute test statistic for the observed data
S1Robs = sum(myrank[1:4])

## Step 2: Obtain all possible assignments.
indices = combn(1:7,4)
B = dim(indices)[2]

myrank = 1:8
indices = combn(1:8,4)
B = dim(indices)[2]

# For each assignment, compute the test statistic
S1R = rep(0,B)
for (i in 1:B){
  ids = indices[,i]
  S1R[i] = sum(myrank[ids])
}

## Step 3: Compute p-value for Ha: mu_1 > mu_2
length(which(S1R>=S1Robs))/B


# When m and n are fixed, then the null distribution of S1R is known

# now we can determine the rejection region

# alpha = 0.1, H0: mu_1 = mu_2 vs Ha: mu_1 > mu_2  ==> R.R = {21, 22} <=> We reject if S1R >= 21

# if R.R. = {22}, Type I error: 1/35 = 0.028
# if R.R. = {21, 22}, Type I error: 2/35 = 0.057
# if R.R. = {20, 21, 22}, Type I error: 4/35 = 0.11 > 0.1

