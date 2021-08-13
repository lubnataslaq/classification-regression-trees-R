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
---------------
 #data tissue_gene example
#Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. That means that it is difficult to have a node in which #placentas are the majority. Rerun the analysis you did in Q1 with caret::train(), but this time with method = "rpart" and allow it to split any node by using the argument control # =rpart.control(minsplit = 0). Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.

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
-----
#We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. Use #the train() function and the rf method to train a Random Forest model and save it to an object called fit. Try out values of mtry ranging from seq(50, 200, 25) (you can also #explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, use the following argument: #nodesize = 1. , varImp() on the output of train() and save it to an object called imp

#Note: This exercise will take some time to run. If you want to test out your code first, try using smaller values with ntree. Set the seed to 1991 again.
library(caret)   
library(rpart)
library(dslabs)
set.seed(1991, sample.kind="Rounding")

fit0 <- with(tissue_gene_expression ,train(x, y , method= "rf" , 
      tuneGrid= data.frame(mtry= seq(50, 200, 25)),
      nodesize=1 ))

plot(fit0)
confusionMatrix(fit0)
imp <- varImp(fit0)
