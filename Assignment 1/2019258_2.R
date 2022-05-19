rm(list=ls())
# read csv file
data = read.csv("data_Q2.csv")
# store the heights
data = data[,c(2)]
# mean of data
m = mean(data)
# variance of data
v = var(data)

theta <- c(m,v)

# log likelihood
mle.normal <- function(theta, data){
  mean <- theta[1]
  var <- theta[2]
  ll <- -.5*1000*log(2*pi) -.5*1000*log(var) - (1/(2*var))*sum((data - mean)**2)
  return(-ll)
}
# mle value
mlevalue.normal <- optim(theta,mle.normal,data = data)
print(mlevalue.normal)

# (b)
z <- c()
# various values of mean and variance to calculate 
mu <- seq(69.5,70.5,by = 0.1)
vari <- seq(8.5,9.5,by = 0.1)
# NLL
for(i in mu){
  for(j in vari) {
    curr_val <- -1*mle.normal(c(i,j),data)
    z <- c(z, curr_val)
  }
}
z <- matrix(z, length(mu), length(vari))
# plotting
persp(mu,vari,z, col = "blue", theta = 120, ticktype = "detailed", xlab = "mean", ylab = "variance", zlab = "NLL", xlim = c(69.5,70.5), ylim = c(8.5,9.5), zlim = c(min(z), max(z)), main = "Plot of NLL vs mean vs variance")

# for extra explanation
# fixed variance
z1 <- c()
for(i in mu){
  curr_val <- -1*mle.normal(c(i,v),data)
  z1 <- c(z1,curr_val)
}
z1 <- matrix(z1, 1, length(mu))
plot(mu, z1, xlab = "mean", ylab = "Log-Likelihood", main = "NLL vs mean with fixed variance")
#fixed mean
z2 <- c()
for(i in vari){
  curr_val <- -1*mle.normal(c(m,i),data)
  z2 <- c(z2,curr_val)
}
z2 <- matrix(z2, 1, length(vari))
plot(vari, z2, xlab = "variance", ylab = "Log-Likelihood", main = "NLL vs variance with fixed mean")


