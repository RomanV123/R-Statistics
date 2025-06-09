data <- c(7, 11, 15, 16, 17,20, 22, 24, 25,28, 29, 33, 34, 36,37,39,41,42, 45,49, 57, 66, 71, 84, 90)

###bootstrap for sample mean
Xbar_ori <- mean(data)

B=10000
Xbar_boot <- vector(length = B)
for(i in 1:B){
  data_boot <- sample(data,size = length(data),replace = TRUE)
  Xbar_boot[i]<-mean(data_boot)
}

##calculate mse
mse = sum((Xbar_boot-Xbar_ori)^2)/B
mse


################################
####get sample standard deviation mse
sd_ori <- sd(data)

B=10000
sd_boot <- vector(length = B)
for(i in 1:B){
  data_boot <- sample(data,size = length(data),replace = TRUE)
  sd_boot[i]<-sd(data_boot)
}
##calculate mse
mse = sum((sd_boot-sd_ori)^2)/B
mse
