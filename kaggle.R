# STATS 202 Kaggle Competition
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

idx <- sample(100, 70)
training <- train[idx, ]
testing <- train[-idx, ]

# Exploratory examination of the data
pairs(train)
cor(train)

set.seed(123)

# Simple multiple linear regression'
train.dat <- train[-1, ]
mlm.fit <- lm(Value ~., data = train.dat)
par(mfrow=c(2,2))
plot(mlm.fit) # Not sure what to make of the residuals distribution, thoughts?

# We have some awful outliers (point 5 I think), so maybe we should remove those?

par(mfrow=c(1,1))

# ---> Dan, 
#   I don't think the relationships look linear in any sense, so I am going to punt on trying Ridge/Lasso
#   I set up some cross validation stuff, but I don't think it is working out.   

# Cross validation for best subset selection
library(leaps)
regfit.best <- regsubsets(Value ~., data = training[, -1], nvmax = 7)
test.mat <- model.matrix(Value ~., data = testing[, -1])
val.errors <- rep(NA, 7)
for(i in 1:7){
  this.coef <- coef(regfit.best, id = i)
  preds <- test.mat[, names(this.coef)] %*% this.coef
  val.errors[i] = mean((testing$Value - preds)^2)
}

# PLS might be a good idea because it will let us fit more smoothly while still maintaining that simplistic linear
# relationship.

# Partial least squares regression
library(pls)
pls.fit <- plsr(Value ~., data = training, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, testing, ncomp = 3) # using 3 PC's b/c no longer increases by > 1% for more
pls.mse <- mean((pls.pred - testing$Value)^2)

# Dan,
# I think I am going to implement a decision tree to carve up the data into more manageable sections based on
# some of the factor variables such as stories, fireplaces, zip code, and bathrooms. Then I think we should 
# do partial least squares or local linear regression within each section. Then we can stitch them all together to
# form our final model. Thoughts?



