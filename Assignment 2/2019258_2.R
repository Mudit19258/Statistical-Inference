rm(list=ls())
# Q2
# sample size
n<-10
# data
x<-c(5.728, 5.731, 5.722, 5.719, 5.727, 5.724, 5.718, 5.726, 5.723, 5.722)
# sample mean
sample_mean<-mean(x)
# calculating sample variance
sample_variance <- 0
for (i in x){
  sample_variance <- sample_variance + (i - sample_mean)^2
}
sample_variance <- sample_variance * (1/(n-1))

# standard deviation
sigma.null <- 0.4

# H0: var>=(0.04)^2 versus H1: var<(0.04)^2
# we will do chi square test

# test statistic 
chisq_test_statistic<-((n-1)*(sample_variance))/(sigma.null^2)
cat("Test statistic: ",chisq_test_statistic,"\n")

# chi square critical value
chisq_critical_value<-qchisq(p = 0.05, df = n-1, lower.tail = TRUE)
cat("Test Critical Value: ",chisq_critical_value,"\n")
# statistic value lies in rejection region

# p value
chisq_p_value<-pchisq(q = chisq_test_statistic, df = (n-1), lower.tail = TRUE)
cat("P value: ",chisq_p_value,"\n")
# p value < aplha, we reject H0