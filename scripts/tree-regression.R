library(here)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tree)

boston <- here("data", "Data_Boston.csv") %>% 
  read_csv() %>% 
  print()

glimpse(boston)
summary(boston)

#regression tree
set.seed(1234)
train_ind <- sample.int(n = nrow(boston), 
                        size = floor(.75*nrow(boston)), 
                        replace = FALSE)

training <- boston[train_ind, ]
test <- boston[-train_ind, ]

boston_tree <- rpart(target ~ CRIM + ZN + INDUS + CHAS + NOX + RM +
                       AGE + DIS + RAD + TAX + PTRATIO + B + 
                       LSTAT, data = training, method = "anova")

boston_tree$cptable
## @knitr plotcp
plotcp(boston_tree)
#0.035 is where the variance falls below the relevant level
tree_prune <- prune(boston_tree, cp = 0.035)
## @knitr firsttree
prp(tree_prune, extra = 1, box.col = "olivedrab3")
## @knitr stop
prune_pred <- predict(tree_prune, newdata = test)
RMSE_prune <- round(sqrt(mean((prune_pred - test$target)^2)), digits = 2)
print(RMSE_prune)
#create partition to visualize splits
#it only works with 2 predictor variables
#so it only applies to tree_prune, which isn't the best model
#but its pretty cool nonetheless!
## @knitr partitiontree
plot(boston$LSTAT, boston$RM, col = "olivedrab3", pch=20, xlab="LSTAT",ylab="RM")
partition.tree(prune.tree(tree(target ~ RM + LSTAT, data = training), best = 5), ordvars=c("LSTAT","RM"), add=TRUE, cex = 1.15)

#use minimum relative x-error as pruning criteria
pdtree <- prune(boston_tree, cp=boston_tree$cptable[which.min(boston_tree$cptable[,"xerror"]),"CP"])
## @knitr secondtree
prp(pdtree, extra = 1, box.col = "olivedrab3")
## @knitr stop
tree_pred <- predict(pdtree, newdata = test)
RMSE_tree <- round(sqrt(mean((tree_pred - test$target)^2)), digits = 2)
print(RMSE_tree)
#results in a tree with 4 (tree_prune) 
#or 7 (pdtree) internal nodes
#room size seems to be most important predictor 

#Now random forest
rf_boston <- randomForest(target ~ CRIM + ZN + INDUS + CHAS + NOX + RM +
                            AGE + DIS + RAD + TAX + PTRATIO + B + 
                            LSTAT, data = training, ntree = 1000, importance = TRUE)

rf_predict <- predict(rf_boston, test, type = "response")
rf_rmse <- round(sqrt(mean((rf_predict - test$target)^2)), digits = 2)
print(rf_rmse)
importance(rf_boston)
## @knitr importanceplot
varImpPlot(rf_boston, sort = TRUE, main = NULL)
## @knitr stop
boston_imp <- as.data.frame(importance(rf_boston))
boston_imp$variables <- row.names(boston_imp)
boston_imp[order(boston_imp$"%IncMSE", decreasing = "TRUE"), ]
#this shows us that LSTAT and RM are still our two most
#important variables, which aligns with the tree

#plot predictions vs actual
plot(test$target, rf_predict, pch = 20, col = "purple")
abline(lm(test$target ~ rf_predict, data = test))
linear_model <- lm(test$target ~ rf_predict, data = test)
summary(linear_model)

