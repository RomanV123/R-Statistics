

####### paired-comparison permutation test

## mean difference

Dv = c(11,-3,12,-6,4,5)

Dmean.obs = mean(Dv)


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


Dmeanv = rep(0,B)
for (i in 1:B){
  Dmeanv[i] = mean(abs(Dv)*signs[,i])
}

# Ha: mu_X > mu_Y
length(which(Dmeanv>=Dmean.obs))/B

# Ha: mu_X < mu_Y
length(which(Dmeanv<=Dmean.obs))/B

# Ha: mu_X != mu_Y
length(which(abs(Dmeanv)>=abs(Dmean.obs)))/B


## S+

ids = which(Dv>0)

Spos.obs = sum(Dv[ids])

Sposv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]>0)
  Sposv[i] = sum(abs(Dv)[ids])
}

# Ha: mu_X > mu_Y
length(which(Sposv>=Spos.obs))/B

# Ha: mu_X < mu_Y
length(which(Sposv<=Spos.obs))/B


## centered S+

ids = which(Dv>0)
Sposc.obs = sum(Dv[ids]) - 0.5*sum(abs(Dv))

Sposcv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]>0)
  Sposcv[i] = sum(abs(Dv)[ids]) - 0.5*sum(abs(Dv))
}

# Ha: mu_X > mu_Y
length(which(Sposcv>=Sposc.obs))/B

# Ha: mu_X < mu_Y
length(which(Sposcv<=Sposc.obs))/B

# Ha: mu_X != mu_Y
length(which(abs(Sposcv)>=abs(Sposc.obs)))/B


## S_

ids = which(Dv<0)

Sneg.obs = sum(Dv[ids])

Snegv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]<0)
  Snegv[i] = -sum(abs(Dv)[ids])
}

# Ha: mu_X > mu_Y
length(which(Snegv>=Sneg.obs))/B

# Ha: mu_X < mu_Y
length(which(Snegv<=Sneg.obs))/B


## centered S-

ids = which(Dv<0)
Snegc.obs = sum(Dv[ids]) + 0.5*sum(abs(Dv))

Snegcv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]>0)
  Snegcv[i] = -sum(abs(Dv)[ids]) + 0.5*sum(abs(Dv))
}

# Ha: mu_X > mu_Y
length(which(Snegcv>=Snegc.obs))/B

# Ha: mu_X < mu_Y
length(which(Snegcv<=Snegc.obs))/B

# Ha: mu_X != mu_Y
length(which(abs(Snegcv)>=abs(Snegc.obs)))/B


### signed-rank 

SR = sign(Dv)*rank(abs(Dv))

## SRmean
SRmean.obs = mean(SR)

SRmeanv = rep(0,B)
for (i in 1:B){
  SRmeanv[i] = mean(abs(SR)*signs[,i])
}

# Ha: mu_X > mu_Y
length(which(SRmeanv>=SRmean.obs))/B

# Ha: mu_X < mu_Y
length(which(SRmeanv<=SRmean.obs))/B

# Ha: mu_X != mu_Y
length(which(abs(SRmeanv)>=abs(SRmean.obs)))/B


## SR+

ids = which(SR>0)
SRpos.obs = sum(SR[ids])

SRposv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]>0)
  SRposv[i] = sum(abs(SR)[ids])
}

# Ha: mu_X > mu_Y
length(which(SRposv>=SRpos.obs))/B

# Ha: mu_X < mu_Y
length(which(SRposv<=SRpos.obs))/B


## centered SR+

ids = which(Dv>0)
SRposc.obs = sum(SR[ids]) - 0.5*sum(abs(SR))

SRposcv = rep(0,B)
for (i in 1:B){
  ids = which(signs[,i]>0)
  SRposcv[i] = sum(abs(SR)[ids]) - 0.5*sum(abs(SR))
}

# Ha: mu_X > mu_Y
length(which(SRposcv>=SRposc.obs))/B

# Ha: mu_X < mu_Y
length(which(SRposcv<=SRposc.obs))/B

# Ha: mu_X != mu_Y
length(which(abs(SRposcv)>=abs(SRposc.obs)))/B


####### paired-comparison permutation test with a larger number of paired observations

## mean difference

Dv = read.table("Dv.txt")[,1]

n = length(Dv)

Dmean.obs = mean(Dv)

Z = Dmean.obs/sqrt(sum(Dv^2)/n^2)

1-pnorm(Z)


# through random permutations

B = 100000
Dmeanv = rep(0,B)

for (i in 1:B){
  signs = sample(c(-1,1),n,replace=T)
  Dmeanv[i] = mean(abs(Dv)*signs)
}

length(which(Dmeanv>=Dmean.obs))/B


## S+

ids = which(Dv>0)
Spos.obs = sum(Dv[ids])

Z = (Spos.obs - sum(abs(Dv))/2)/sqrt(sum(Dv^2)/4)
1-pnorm(Z)

# through random permutations

B = 100000
Sposv = rep(0,B)

for (i in 1:B){
  signs = sample(c(-1,1),n,replace=T)
  ids = which(signs>0)
  Sposv[i] = sum(abs(Dv)[ids])
}

length(which(Sposv>=Spos.obs))/B



## S-

ids = which(Dv<0)
Sneg.obs = sum(Dv[ids])

Z = (Sneg.obs + sum(abs(Dv))/2)/sqrt(sum(Dv^2)/4)
1-pnorm(Z)

# through random permutations

B = 100000
Snegv = rep(0,B)

for (i in 1:B){
  signs = sample(c(-1,1),n,replace=T)
  ids = which(signs<0)
  Snegv[i] = -sum(abs(Dv)[ids])
}

length(which(Snegv>=Sneg.obs))/B



### signed-rank 

SR = sign(Dv)*rank(abs(Dv))

## SR+

ids = which(SR>0)
SRpos.obs = sum(SR[ids])

Z = (SRpos.obs - sum(abs(SR))/2)/sqrt(sum(SR^2)/4)
1-pnorm(Z)

# through random permutations

B = 100000
SRposv = rep(0,B)

for (i in 1:B){
  signs = sample(c(-1,1),n,replace=T)
  ids = which(signs>0)
  SRposv[i] = sum(abs(SR)[ids])
}

length(which(SRposv>=SRpos.obs))/B


