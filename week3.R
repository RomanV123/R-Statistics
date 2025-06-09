# Power by simulation
# Hypothesis: 
# H0: mu is 0 vs. H1: mu is not 0
alpha = 0.05 # level of the test
n = 500 # sample size
set.seed(12345) 
# Normal setting:
# single run:
sample1 = rnorm(n, mean = 0.3, sd = 2) # underlying distribution
# t-test
mu0 = 0 # population mean under the null
sample1.mean = mean(sample1); sample1.sd = sd(sample1)
T1 = (sample1.mean-mu0)/(sample1.sd/sqrt(n))
t1 = qt(1-alpha/2, df = n-1) # critical value
(abs(T1)>t1)
# Binomial test
# define X be the number of samples less than 0
# under the null, X follows Bin(n,0.5), symmetric
# we'd like to reject the null if X is extremely small or large
# rejection region: {X<=k} or {X>=n-k}
qbinom(alpha/2, n, 0.5)
pbinom(229, n, 0.5) # 0.027>0.025
pbinom(228, n, 0.5) # 0.022<0.025 
# we'd like to control the type-I error smaller than 0.05, meanwhile achieve the highest power
# So, the rejection region is: {X<=228} or {X>=500-228=272}

# 10000 runs:
B = 10000
k = 228
count.ttest = count.btest = 0
for (i in 1:B){
  y = rnorm(n, mean = 0.3, sd = 2) # Normal with median 0.3
  # H0: Median = 0
  # Method 1: t-test
  t = (mean(y)-0)/(sd(y)/sqrt(n))
  if(abs(t)>t1){
    count.ttest = count.ttest + 1
  }
  # Method 2: Binomial Test
  X = sum(y<0)
  if(X<=k || X>=(n-k)){
    count.btest = count.btest + 1
  }
}
count.ttest/B;count.btest/B
# The t-test has higher power under normal setting.

