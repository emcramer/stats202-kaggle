# Bereket Kaggle Script
library(tree)
library(leaps)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

idx <- sample(nrow(train), 60)
training <- train[idx, ]
testing <- train[-idx, ]

set.seed(123)
tree.training <- training[, -1]

tree.training$Value <- log(tree.training$Value) # log transform the values to produce a more normal distribution
tree.training$Story <- as.factor(tree.training$Story)
tree.training$Baths <- as.factor(tree.training$Baths)
tree.training$Fireplaces <- as.factor(tree.training$Fireplaces)
tree.training$Zip <- as.factor(tree.training$Zip)
tree.training <- na.omit(tree.training)

# preliminary tree fit to get an idea of effectiveness
houses.tree <- tree(Value ~ ., data = tree.training[, ])
tree.s <- summary(houses.tree)
tree.s
plot(houses.tree)
text(houses.tree, pretty = 1)
mtext("Preliminary Regression Tree", side = 3, padj = -2, font = 2)
mtext(paste("MSE: ", mean(tree.s$residuals^2)), side = 1, padj = 1)

# Use to classify observations
tree.training$treeclass <- predict(houses.tree,newdata=tree.training)
tree.training$treeclass <- as.factor(tree.training$treeclass)

# Data frames for each class
classone <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[1],]
classtwo <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[2],]
classthree <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[3],]
classfour <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[4],]
classfive <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[5],]
classsix <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[6],]
classseven <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[7],]
classeight <- tree.training[tree.training$treeclass==levels(tree.training$treeclass)[8],]


# Model for class one
onevarselect <- regsubsets(Value~.,data=classone,method='forward')
classonelm <- lm(Value~SqFt+Zip,data=classone)
summary(classonelm)
predictions <- predict(classonelm)

# Model for class two
twovarselect <- regsubsets(Value~.,data=classtwo,method='forward')
classtwolm <- lm(Value~Baths+Zip,data=classtwo)
