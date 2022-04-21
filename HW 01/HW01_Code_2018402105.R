# IE 425 HW01 - Spring 2022
# Ahmet Yiðit Doðan
# 2018402105

# Library imports

library(rpart)
library(rpart.plot)
library(caret)
library(tree)
library(caTools)
library(dplyr)
library(Metrics)

# Question 1

# Data set import & adjustment

fd <- read.csv("FinancialDistress-cat.csv")
fd$Financial.Distress=as.factor(fd$Financial.Distress)

# Part A

# Splitting the data set into training and test

set.seed(500)

split1  <- sample.split(fd$Financial.Distress, SplitRatio=0.75)

fdtrain <- subset(fd, split1==TRUE)
fdtest  <- subset(fd, split1==FALSE)

prcntfd   <- nrow(dplyr::filter(fd,      Financial.Distress %in% 1)) / nrow(fd)      
prcnttr   <- nrow(dplyr::filter(fdtrain, Financial.Distress %in% 1)) / nrow(fdtrain)
prcnttest <- nrow(dplyr::filter(fdtest,  Financial.Distress %in% 1)) / nrow(fdtest)

percentages        <- 100*c(prcntfd  , prcnttr   , prcnttest )
names(percentages) <-     c("overall", "training", "test"    )

percentages

# The percentages look pretty much the same, which implies that
# the data set is suitable for a homogeneous split with high precision.

# Part B

# Generating the tree (Attributes are chosen intuitively.)

treePartB <- rpart(Financial.Distress~., data=fdtrain, minsplit=30, minbucket=8)

prp(treePartB, type=5, extra=1, tweak=1)

cpTableQ1 <- printcp(treePartB)

# Reporting the number of terminal nodes in the tree with the lowest cv-error, 
# which is equal to [the number of splits performed to create the tree] + 1

optIndexQ1 <- which.min(unname(treePartB$cptable[, "xerror"]))

cpTableQ1[optIndexQ1, 2] + 1

# Pruning the tree to the optimized cp value

optTreePartB <- prune.rpart(tree = treePartB, cp = cpTableQ1[optIndexQ1, 1])

prp(optTreePartB)

# Part C

# Making predictions in the test set and tabulating the results

predfdPartC <- predict(optTreePartB, newdata=fdtest, type="class")

tblPartC    <- table(fdtest$Financial.Distress, predfdPartC)

tblPartC

# Generating the required confusion matrix

cmPartC <- confusionMatrix(predfdPartC, fdtest$Financial.Distress, positive = "1")
    
# Reporting the metrics

tbl <- matrix(c(cmPartC[["overall"]][["Accuracy"]]   ,
                cmPartC[["byClass"]][["Sensitivity"]],
                cmPartC[["byClass"]][["Specificity"]],
                cmPartC[["byClass"]][["Precision"]]  ,
                rep(NaN, times=4                      )), ncol=2)

colnames(tbl) <- c('PartC', 'PartE')
rownames(tbl) <- c('Accuracy', 'Sensitivity', "Specificity", "Precision")

tbl
    
# Part D

# Creating a tree with terminal nodes that all have zero deviance

treePartD <- tree(Financial.Distress~., data=fdtrain, minsize=2, mindev=0.0)

# Reporting the number of terminal nodes

summary(treePartD)[["size"]]

# Part E

# Making predictions in the test set and tabulating the results

predfdPartE <- predict(treePartD, newdata=fdtest, type="class")

tblPartE    <- table(fdtest$Financial.Distress, predfdPartE)

tblPartE

# Generating the confusion matrix

cmPartE <- confusionMatrix(predfdPartE, fdtest$Financial.Distress, positive = "1")

# Reporting the metrics and comparison with part C

tbl[,2] <- c(cmPartE[["overall"]][["Accuracy"]]   ,
             cmPartE[["byClass"]][["Sensitivity"]],
             cmPartE[["byClass"]][["Specificity"]],
             cmPartE[["byClass"]][["Precision"]]   )

tbl

# Question 2

tc <- read.csv("ToyotaCorolla.csv")

# Part A

set.seed(500)

split2  <- sample.split(tc$Price, SplitRatio=0.80)

tctrain <- subset(tc, split2==TRUE)
tctest  <- subset(tc, split2==FALSE)

nrow(tctrain)
nrow(tctest)

# Part B

treeQ2 <- rpart(Price~., data=tctrain)

prp(treeQ2, type=5, extra=1, tweak=1)

cpTableQ2 <- printcp(treeQ2)

# Reporting the number of terminal nodes in the tree with the lowest cv-error, 
# which is equal to [the number of splits performed to create the tree] + 1

optIndexQ2 <- which.min(unname(treeQ2$cptable[, "xerror"]))

cpTableQ2[optIndexQ2, 2] + 1

# Pruning the tree to the optimized cp value

optTreeQ2 <- prune.rpart(tree = treeQ2, cp = cpTableQ2[optIndexQ2, 1])

prp(optTreeQ2)

# Part C

# Making predictions in the test set

predQ2 <- predict(optTreeQ2, newdata=tctest)

# Reporting the metrics

rmse(actual=tctest$Price, predicted=predQ2)
mae (actual=tctest$Price, predicted=predQ2)


