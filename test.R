library(tidyverse)
library(olsrr)
library(leaps)
library(e1071)
library(gmodels)
setwd("C:/Users/marco/Desktop/CMDA 4654/Projects/Project 1")

# read/clean data
df <- read.csv("data/property.csv", header=TRUE)
wake_cols <- c(10, 19:21, 24:29, 34, 43, 48, 50, 53, 59, 61)

df_w <- df %>%
    dplyr::select(all_of(wake_cols)) %>%
    mutate_if(is.character, list(~na_if(.,""))) %>%
    filter(Land.Value != 0 & Building.Value != 0) %>%
    drop_na()
    
# ------- MULTIPLE LINEAR REGRESSION ---------------------------------

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

# ------- NAIVE BAYES ------------------------------------------------

# can we classify a piece of land's site description based on
# the acerage, structures + buildings, building and land value,
# square footage and area?

nb_cols <- c(1, 3:6, 13, 14, 16)
df_nb <- df_w %>%
    dplyr::select(all_of(nb_cols))

# split training + testing data in a 70/30 split
n <- dim(df_nb)[1]
nb_train <- df_nb[1:48000, ]
nb_test <- df_nb[48001:n, ]

model <- naiveBayes(APA.Site.Description ~ ., data=nb_train)
yhat <- predict(model, newdata=nb_test)

# matrix table
tab <- table(yhat, nb_test$APA.Site.Description)
round(prop.table(tab), 3)

# misclass
misclass <- (sum(tab) - sum(diag(tab))) / sum(tab)
misclass # holy shit this is bad - like REALLY bad

# ------- LOGISTIC REGRESSION ----------------------------------------

# is there a way i can track whether or not the sale was profitable or not
# time? difference between land and total sale?
log_cols <- c(5, 6, 7)
df_log <- df_w %>%
    dplyr::select(Building.Value, Land.Value, Total.Sale.Value) %>%
    mutate(Profited = ifelse(Building.Value + Land.Value < Total.Sale.Value, 1, 0))
log_model <- glm(Profited ~ log(Building.Value) + log(Land.Value), data=df_log, family="binomial")
summary(log_model)

ggplot(df_log, aes(x = log(Building.Value) + log(Land.Value), y = Profited)) +
    geom_point(size=0.03, aes(color = Profited), position = position_jitter(height = 0.03, width = 0)) +
    geom_smooth(method = "glm", method.args = list(family="binomial"), se = F) +
    labs(y = "P(Profited)") + theme_bw() +
    scale_y_continuous(breaks = seq(0,1, by =0.05))

# alternate model?
pihat <- predict(log_model, type = "response")
etahat <- predict(log_model, type = "link")

ggplot(df_log, aes(x = log(Building.Value), y = log(Land.Value), colour=Profited)) + 
    geom_point(size=0.03) +
    # geom_abline(slope = -coef(log_model)[3]/coef(log_model)[2],
    #             intercept = (log(.4/.6) - coef(log_model)[1] ) / coef(log_model)[2] ) +
    scale_y_continuous(breaks = seq(200, 800, by = 50)) + theme_bw()

# could there be more factors to the profiltability of a building?
