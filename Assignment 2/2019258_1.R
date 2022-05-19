rm(list=ls())

# Q1

# sample size 
n<-28
# sample mean
sample_mean <- 1.0
# sample standard deviation
sample_standard_deviation <- 0.3
# given mean response time
mu.null<-0.8

# H0: mu = 0.8 versus H1: mu /= 0.8
# This is a two - tailed test

# we find the test statistic first
test_statistic<-(sample_mean - mu.null)/(sample_standard_deviation/sqrt(n))
cat("Test statistic: ",test_statistic,"\n")

# we calculate the critical value
# given alpha = 0.05, so for two-tailed we divide by 2
test_critical_value<-(qt(p = 0.05/2,df = n - 1,lower.tail = FALSE))
cat("Test Critical Value: ",test_critical_value,"\n")
# Here the test_critical_value lies in the rejection region thus, we reject H0

p_value<-2 * pt(q = test_statistic, df = n - 1, lower.tail = FALSE)
cat("P value: ",p_value,"\n")
# Here p value < alpha, thus we reject the H0


