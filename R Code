---
title: "Irene Hsueh's BS 806 Homework 8"
author: "Irene Hsueh"
date: "10/29/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tree)
library(rpart)
library(rpart.plot)
```

### Reading in CSV File 
```{r}
framingham <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 8 - Classification and Regression Trees/Homework 8/FHS_data.csv")
head(framingham, 10)
```


### Question 1 - Part 3
```{r}
#Simulating a Sample of FVC Scores for 4 groups
stdv <- sqrt(6194)
male_young_fvc_sim <- rnorm(n=521, mean=574.8, sd=stdv)
male_old_fvc_sim <- rnorm(n=517, mean=512.3, sd=stdv)
female_young_fvc_sim <- rnorm(n=890, mean=444.9, sd=stdv)
female_old_fvc_sim <- rnorm(n=609, mean=366.6, sd=stdv)
fvc_simulation <- c(male_young_fvc_sim, male_old_fvc_sim, female_young_fvc_sim, female_old_fvc_sim)
```


### Question 1 - Parts 4 & 5
```{r}
#Comparing Distribution of Real and Simulated Data
hist(fvc_simulation, col="hotpink")
hist(framingham$FVC, col="cyan2")

#Pulling Real Data for Each Group 
male_young <- framingham[framingham$SEX < 1.5 & framingham$AGE < 48.5,]
male_old <- framingham[framingham$SEX < 1.5 & framingham$AGE > 48.5,]
female_young <- framingham[framingham$SEX > 1.5 & framingham$AGE < 51.5,]
female_old <- framingham[framingham$SEX > 1.5 & framingham$AGE > 51.5,]

#Adding Simulated FVC Scores to Each Group
male_young$fvc_sim_scores <- male_young_fvc_sim
male_old$fvc_sim_scores <- male_old_fvc_sim
female_young$fvc_sim_scores <- female_young_fvc_sim
female_old$fvc_sim_scores <- female_old_fvc_sim

#Merging Real Data with Simulated FVC Scores
framingham_simulation <- rbind(male_young, male_old, female_young, female_old)
```


### Question 1 - Part 5
```{r}
#Creating Regression Tree
tree1 <- tree(fvc_sim_scores ~ SEX + AGE + Smoke + SPF + T2D, data=framingham_simulation, control=tree.control(nobs=nrow(framingham_simulation), mindev=0.001))
summary(tree1)

plot(tree1); text(tree1, pretty=0, cex=1.0)

#Pruning Tree by Cross Validation
tree1_prune <- cv.tree(tree1, FUN=prune.tree, K=5)
plot(tree1_prune$size, tree1_prune$dev, col="hotpink")

tree1_best_prune <- prune.tree(tree1, best=4)
plot(tree1_best_prune); text(tree1_best_prune, pretty=0, cex=1.0)
tree1_best_prune
summary(tree1_best_prune)
```


### Question 2 - Part A
```{r}
#Creating Classification Tree
tree2 <- tree(dth ~ SEX + AGE + Smoke + FVC + SPF + T2D, framingham)
summary(tree2)

plot(tree2)
text(tree2, pretty=0, cex=1.0)
```


### Question 2 - Part D
```{r}
#Creating Classification Tree for Training Data 
set.seed(1)
training_size <- sample(1:nrow(framingham), (2/3)*nrow(framingham))
tree3 <- tree(dth ~ SEX + AGE + Smoke + FVC + SPF + T2D, framingham[training_size,])

summary(tree3)
plot(tree3)
text(tree3, pretty=0, cex=1.0)
```


### Question 2 - Part D 
```{r}
#Testing Predictions
predicted_death <- predict(tree3, newdata = framingham[-training_size,], type = "class")

predicted_death_table <- table(framingham[-training_size,]$dth, predicted_death)

missclassed_cells <- predicted_death_table[1,2] + predicted_death_table[2,1]

misclassificiation_error <- missclassed_cells/sum(table(framingham[-training_size,]$dth, predicted_death))
```


### Question 3
```{r}
tree4 <- rpart(dth ~ SEX + AGE + Smoke + FVC + SPF + T2D, method="class", data=framingham)
rpart.plot(tree4)
```
