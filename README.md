# classification-regression-trees-R
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


-----
library(rpart)
install.packages("rpart")
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)  
fit <- rpart(y ~ ., data = dat)
prune(fit, cp = 0.01)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
----------
  library(randomForest)
install.packages("randomForest")
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
fit1 <-   randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  plot(fit1)
  -------
  #data tissue_gene example
library(caret)   
library(rpart)
library(dslabs)
set.seed(1991, sample.kind="Rounding")

train_part <- with(tissue_gene_expression ,train(x, y , method= "rpart" , 
      tuneGrid= data.frame(cp= seq(0, 0.1, 0.01)),
      control = rpart.control(minsplit = 0)))
plot(train_part$finalModel)
text(train_part$finalModel)
confusionMatrix(train_part)
