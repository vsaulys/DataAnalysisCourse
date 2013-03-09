#
# Quiz 7
#

# Quetion 2

set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

library(splines)

ns1 <- ns(yValues, df=3)

# question 3
library(simpleboot) 
data(airquality)
attach(airquality)

quantile(airquality$Wind, probs=0.75)

MyQFunc <- function(data) {
  quantile(data, probs=0.75)
}

MyQFunc(airquality$Wind)

set.seed(883833)
bf <- one.boot(airquality$Wind, FUN=MyQFunc, R=1000)
apply(bf$t, 2, sd)


# Question 4
data(Cars93,package="MASS")
set.seed(7363)

newdata = data.frame(Type = "Large",Price = 20)


