rm(list=ls())
set.seed(123)

# (a)
# random data generated using Gamma distribution
data = rgamma(n = 1000, shape = 5, rate = 2)

# for log-likelihood function of gamma
mle.gamma <- function(theta, data){
  r <- theta[1]
  lambda <- theta[2]
  log_likelihood <- 1000 * r * log(lambda) + (r - 1) * sum(log(data)) - lambda * sum(data) - 1000 * log(gamma(r))
  return(-log_likelihood)
}

# (b)
# first set of values
r <- 5
lambda <- 2
theta <- c(r,lambda)
mlevalue.gamma <- optim(theta, mle.gamma, data = data)
print(mlevalue.gamma)

# second set of values -> Method of Moments
MOM_r <- (mean(data) * mean(data))/var(data)
MOM_lambda <- mean(data)/var(data)
theta2 <- c(MOM_r,MOM_lambda)
mlevalue.gamma2 <- optim(theta2, mle.gamma, data = data)
print(mlevalue.gamma2)

# third set of values
r3 <- 4
lambda3 <- 1
theta3 <- c(r3,lambda3)
mlevalue.gamma3 <- optim(theta3, mle.gamma, data = data)
print(mlevalue.gamma3)

# (c)
# r = 5 constant, lambda varied
lambda_vals <- seq(1,100,1)
z <- c()
# NLL
for(i in lambda_vals){
  z <- c(z, mle.gamma(c(r,i), data))
}
z <- -1*matrix(z, 1, 100)
# plotting
plot(lambda_vals,z,xlab="lambda",ylab="Log-Likelihood", main = "Log-Likelihood v/s lambda plot",xlim=c(1,100),ylim=c(min(z),max(z)))

# (d)
# both values varied
lambda_vals2 <- seq(1,10,by = 0.5)
r_vals <- seq(1,10,by = 0.5)
# NLL
z_new <- c()
for(i in r_vals){
  for(j in lambda_vals2) {
    curr_val <- -1*mle.gamma(c(i,j),data)
    z_new <- c(z_new, curr_val)
  }
}
#print(z)
z_new <- matrix(z_new, length(r_vals), length(lambda_vals2))
#plotting
persp(r_vals, lambda_vals2, z_new, col = 'orange',theta = -75, ticktype = 'detailed',xlab = 'r', ylab = 'lambda', zlab = 'NLL', xlim = c(1,10), ylim = c(1,10), zlim = c(min(z_new),max(z_new)), main = "Plot NLL vs lambda vs r")