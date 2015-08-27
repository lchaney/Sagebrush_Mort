http://stackoverflow.com/questions/11291845/plot-the-results-of-a-multivariate-logistic-regression-model-in-r
http://stackoverflow.com/questions/8662018/ggplot2-stat-smooth-for-logistic-outcomes-with-facet-wrap-returning-full-or



set.seed(12345)
dataset <- expand.grid(Temp = rnorm(30), Age = runif(10))
dataset$Truth <- with(dataset, plogis(2 * Temp - 3 * Age))
dataset$Sample <- rbinom(nrow(dataset), size = 1, prob = dataset$Truth)
model <- glm(Sample ~ Temp + Age, data = dataset, family = binomial)
newdata <- expand.grid(
  Temp = pretty(dataset$Temp, 20), 
  Age = pretty(dataset$Age, 5))
newdata$Sample <- predict(model, newdata = newdata, type = "response")
library(ggplot2)
ggplot(newdata, aes(x = Temp, y = Sample)) + geom_line() + facet_wrap(~Age)
ggplot(newdata, aes(x = Temp, y = Sample, colour = Age, group = Age)) + geom_line()

############
preds <- predict(g, newdata = new.data, type = 'response',se = TRUE)
new.data$pred.full <- preds$fit

new.data$ymin <- new.data$pred.full - 2*preds$se.fit
new.data$ymax <- new.data$pred.full + 2*preds$se.fit  

ggplot(df,aes(x = score, y = pass)) + 
  facet_wrap(~location) + 
  geom_point() + 
  geom_ribbon(data = new.data,aes(y = pred.full, ymin = ymin, ymax = ymax),alpha = 0.25) +
  geom_line(data = new.data,aes(y = pred.full),colour = "blue")

#############
library(boot)    # needed for inv.logit function
library(ggplot2) # version 0.8.9

set.seed(42)
n <- 100

df <- data.frame(location = rep(LETTERS[1:4], n),
                 score    = sample(45:80, 4*n, replace = TRUE))

df$p    <- inv.logit(0.075 * df$score + rep(c(-4.5, -5, -6, -2.8), n))
df$pass <- sapply(df$p, function(x){rbinom(1, 1, x)}) 

gplot <- ggplot(df, aes(x = score, y = pass)) + 
  geom_point() + 
  facet_wrap( ~ location) + 
  stat_smooth(method = 'glm', family = 'binomial') 

# 'full' logistic model
g <- glm(pass ~ location + score, data = df, family = 'binomial')
summary(g)

# new.data for predicting new observations
new.data <- expand.grid(score    = seq(46, 75, length = n), 
                        location = LETTERS[1:4])

new.data$pred.full <- predict(g, newdata = new.data, type = 'response')

pred.sub <- NULL
for(i in LETTERS[1:4]){
  pred.sub <- c(pred.sub,
                predict(update(g, formula = . ~ score, subset = location %in% i), 
                        newdata = data.frame(score = seq(46, 75, length = n)), 
                        type = 'response'))
}

new.data$pred.sub <- pred.sub

gplot + 
  geom_line(data = new.data, aes(x = score, y = pred.full), color = 'green') + 
  geom_line(data = new.data, aes(x = score, y = pred.sub),  color = 'red')