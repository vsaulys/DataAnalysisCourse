#
# Quiz 8
#

# question 2

set.seed(3343)
pValues = rep(NA,100)
for(i in 1:100){
  z = rnorm(20)
  x = rnorm(20)
  y = rnorm(20,mean=0.5*x)
  pValues[i] = summary(lm(y ~ x))$coef[2,4]
}

alpha <- 0.1

sum(pValues < alpha)

# Controls FWER
sum(p.adjust(pValues,method="bonferroni") < alpha)

# Controls FDR
sum(p.adjust(pValues,method="BH") < alpha)

# Question 4

set.seed(44333)
betaNorm_Int <- rep(NA,1000)
betaNorm_X <- rep(NA,1000)
R_betaNorm_Int <- rep(NA,1000)
R_betaNorm_X <- rep(NA,1000)

for(i in 1:1000){
  x <- rnorm(50)
  e <- rnorm(50)
  
  b0 <- 1
  b1 <- 2
  
  y <- b0 + b1*x + e
  df <- data.frame(x=x, y=y)

  
  betaNorm_Int[i] <- lm(data=df, y ~ x)$coeff[1]
  betaNorm_X[i] <- lm(data=df, y ~ x)$coeff[2]

# Case 1  
#  q9 <- quantile(x, probs=.9)
#  df <- subset(df, x<q9)

# Case 2
  q9 <- quantile(y, probs=.9)
  df <- subset(df, y<q9)

  R_betaNorm_Int[i] <- lm(data=df, y ~ x)$coeff[1]
  R_betaNorm_X[i] <- lm(data=df, y ~ x)$coeff[2]
}
quantile(betaNorm_Int)
quantile(R_betaNorm_Int)
quantile(betaNorm_X)
quantile(R_betaNorm_X)

# Question 5

set.seed(44333)
betaNorm_Int <- rep(NA,1000)
betaNorm_X <- rep(NA,1000)
R_betaNorm_Int <- rep(NA,1000)
R_betaNorm_X <- rep(NA,1000)

for(i in 1:1000){
  x <- rnorm(50)
  e <- rnorm(50)
  
  b0 <- 1
  b1 <- 2
  
  y <- b0 + b1*x + e
  df <- data.frame(x=x, y=y)
  
  
  betaNorm_Int[i] <- rlm(data=df, y ~ x)$coeff[1]
  betaNorm_X[i] <- rlm(data=df, y ~ x)$coeff[2]

  # Case 1  
#  q9 <- quantile(x, probs=.95)
#  df <- subset(df, x<q9)
  
  # Case 2
  q9 <- quantile(y, probs=.95)
  df <- subset(df, y<q9)
  
  R_betaNorm_Int[i] <- rlm(data=df, y ~ x)$coeff[1]
  R_betaNorm_X[i] <- rlm(data=df, y ~ x)$coeff[2]
}
quantile(betaNorm_Int)
quantile(R_betaNorm_Int)
quantile(betaNorm_X)
quantile(R_betaNorm_X)


