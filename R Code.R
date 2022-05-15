---
title: "Irene Hsueh's BS 806 Homework 6"
author: "Irene Hsueh"
date: "10/13/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(leaps)
```

### Reading in CSV File
```{r}
divorce <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 6 - Variable Selection/Homework 6/divusa.csv")
head(divorce, 10)
```


### Backwards Elimination
```{r}
full_model <- lm(divorce ~ ., data=divorce)
summary(full_model)

backwards_step1 <- update(full_model, .~. -unemployed)
summary(backwards_step1)
```


### Forward Selection  
```{r}
scope <- ~(year + unemployed + femlab + marriage + birth + military)
null_model <- lm(divorce ~ 1, data=divorce)
n <- nrow(divorce)

forward_aic <- step(null_model, scope=scope, direction="forward", k=2)

forward_bic <- step(null_model, scope=scope, direction="forward", k=log(n))
```


### Adjusted R2 and Mallows Cp 
```{r}
leaps <- regsubsets(divorce ~ ., data=divorce)

#Adjusted R2 Criteria
plot(2:7, 
     summary(leaps)$adjr2, 
     xlab="Number of Parameters",
     ylab="Adjusted R-Squared",
     pch=20, 
     col="hotpink")
summary(leaps)$adjr2

#Cp Statistic Criteria
plot(2:7, 
     summary(leaps)$cp, 
     xlab="Number of Parameters",
     ylab="CP Statistic",
     pch=20, 
     col="hotpink")
abline(0,1)
summary(leaps)$cp
```


