# STATS 202 Kaggle Competition
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Exploratory examination of the data
pairs(train)
cor(train)

# Simple multiple linear regression'
train.dat <- train[-1, ]
mlm.fit <- lm(Value ~., data = train.dat)
par(mfrow=c(2,2))
plot(mlm.fit) # Not sure what to make of the residuals distribution, thoughts?
par(mfrow=c(1,1))

