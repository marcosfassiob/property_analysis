library(tidyverse)
library(olsrr)
library(MASS)
library(dplyr)
library(leaps)
setwd("C:/Users/marco/Desktop/CMDA 4654/Projects/Project 1")

# read/clean data
df <- read.csv("data/property.csv", header=TRUE)
wake_cols <- c(10, 19:21, 24:29, 34, 43, 48, 50, 53, 59, 61)

df_w <- df %>%
    dplyr::select(all_of(wake_cols)) %>%
    mutate_if(is.character, list(~na_if(.,""))) %>%
    drop_na()

## multiple linear regression
# what maximizes the total sale value of a piece of property?
mlr_cols <- c(2:4, 8, 10:11, 13, 15, 17)
df_mlr <- df_w %>%
    dplyr::select(-all_of(mlr_cols))

full_model <- lm(Total.Sale.Value ~ ., data=df_mlr)
best_step_model <- ols_step_both_aic(full_model)

# best subsets?
best_subsets <- regsubsets(Total.Sale.Value ~ ., data=df_mlr)
best_subsets_results <- summary(best_subsets)
best_subsets_results$adjr2 
best_subsets_results$cp 
best_subsets_results$bic

## naive bayes
# can we classify whether a piece of land is residential?

## logistic regression
# is there a way i can track whether or not the sale was profitable or not
# time? difference between land and total sale?

