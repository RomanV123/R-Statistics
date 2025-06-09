

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


