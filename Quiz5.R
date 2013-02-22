#'
#' Quiz 5
#'
#'

data(warpbreaks)

summary(warpbreaks)


# Question 1
f <- breaks ~ wool + tension
aov1 <- aov(f, data=warpbreaks)

summary(aov1)

# Question 2
# see binay outcomes page 5
log(.2/(1-.2))

# Question 3
library(glm2)
data(crabs)

summary(crabs)

f <- Satellites ~ Width
pModel <- glm(f, data=crabs, family='poisson')

summary(pModel)
pModel$coefficients

# Question 4


# Question 5
library(MASS)
data(quine) 
lm1 = lm(log(Days + 2.5) ~ . , data=quine)

step1 <- step(lm1)

summary(step1)





