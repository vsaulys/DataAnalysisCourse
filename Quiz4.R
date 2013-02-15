#
# Quiz Week 4
# -----------------------------------



download.file('https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt', './data/movies.txt', method='curl')

movies <- read.delim('./data/movies.txt', header=TRUE)

str(movies)

names(movies)[names(movies) == 'X'] <- 'title'

# Q1
lm1 <- lm(score ~ box.office, data=movies)
summary(lm1)

# Q2
lm2 <- lm(score ~ box.office, data=movies)
confint(lm2, level=0.9)

# Q3 
lm3 <- lm(score ~ box.office + running.time, data=movies)
summary(lm3)

# Q4
lm4a <- lm(score ~ box.office + running.time, data=movies)
summary(lm4a)

anova(lm4a)

lm4b <- lm(score ~ box.office, data=movies)
summary(lm4b)

lm4c <- lm(score ~ running.time, data=movies)
summary(lm4c)

# Q5
lm5a <- lm(score ~ box.office + running.time, data=movies)
summary(lm5a)

qplot(data=movies, aes(x=running.time, y=score)) + geom_point()
plot(movies$running.time, movies$score)

movies2 <- subset(movies, running.time<200)
plot(movies2$running.time, movies2$score)

lm5b <- lm(score ~ box.office + running.time, data=movies2)
summary(lm5a)
summary(lm5b)

# Q6
lm6 <- lm(score ~ box.office + running.time, data=movies)
summary(lm6)

# Q7
lm7 <- lm(score ~ rating + running.time + rating*running.time, data=movies)
summary(lm7)

# Q8
movies$rating <- relevel(factor(movies$rating), ref='PG')
lm8 <- lm(score ~ rating + running.time + rating*running.time, data=movies)
summary(lm8)

# Q9
data(warpbreaks)
warpbreaks$tension <- relevel(factor(warpbreaks$tension), ref='H')
lm9 <- lm(breaks ~ tension, data=warpbreaks)
summary(lm9)
anova(lm9)
confint(lm9, level=0.95)




