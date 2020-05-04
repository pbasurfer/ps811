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
plotcp(boston_tree)
#0.035 is where the variance falls below the relevant level
tree_prune <- prune(boston_tree, cp = 0.035)
prp(tree_prune, extra = 1, box.col = "olivedrab3")
#create partition to visualize splits
#it only works with 2 predictor variables
#so it only applies to tree_prune, which isn't the best model
#but its pretty cool nonetheless!
plot(boston$LSTAT, boston$RM, col = "olivedrab3", pch=20, xlab="LSTAT",ylab="RM")
partition.tree(prune.tree(tree(target ~ RM + LSTAT, data = training), best = 5), ordvars=c("LSTAT","RM"), add=TRUE, cex = 1.15)

tree_pred <- predict(tree_prune, newdata = test)
RMSE_tree <- sqrt(mean((tree_pred - test$target)^2))
print(RMSE_tree)
#results in a tree with 4 (tree_prune) 
#room size seems to be most important predictor 