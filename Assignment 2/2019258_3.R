rm(list=ls())

# Q3
# sample size
n<-25

# data of smokers
smokers<-c(124, 134, 136, 125, 133, 127, 135, 131, 133, 125, 118)

# data of non-smokers
non_smokers<-c(130, 122, 128, 129, 118, 122, 116, 127, 135, 120, 122, 120, 115, 123)

# this is a two-tailed test
# data is given so we run inbuilt t-test
test.ans<-t.test(x = smokers, y = non_smokers, paired = FALSE, var.equal=FALSE, alternative = "two.sided")

# find test statistic
test_statistic <- test.ans$statistic
cat("Test statistic: ",test_statistic,"\n")

# find test critical value
test_critical_value<-qt(0.05/2, df = 21.636, lower.tail = FALSE)
cat("Test Critical Value: ",test_critical_value,"\n")
# test statistic lies in rejection region

# find p value
p_value <- test.ans$p.value
cat("P value: ",p_value,"\n")
# p value < alpha, we reject H0