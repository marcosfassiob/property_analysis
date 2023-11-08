library(tidyverse)
library(olsrr)
library(leaps)
library(e1071)
library(gmodels)
library(caret)
library(viridis)
library(GGally)
library(car)
setwd("C:/Users/marco/Desktop/CMDA 4654/Projects/Project 1")

# read/clean data
df <- read.csv("data/property.csv", header=TRUE)
wake_cols <- c(10, 18:21, 24:29, 34, 43:46, 48, 50, 52, 53, 59, 61) # columns i want in cleaned data
useless_APA_cols <- c("Site In Natural State", "Developing Site") # rows i DONT want in cleaned data
useless_land_class_cols <- c("Part Exempt", "State Assessed", "Vacant Land", "Manufactured Home", 
                       "Manufactured Home Park")
useless_ownership_cols <- c("County, Parish, Province, Etc.", "Federal Government")

df_w <- df %>%
    dplyr::select(all_of(wake_cols)) %>%
    mutate_if(is.character, list(~na_if(.,""))) %>%
    filter(Land.Value != 0 & Building.Value != 0 & Land.Sale.Value > 0 & Total.Sale.Value > 0) %>%
    filter(!APA.Site.Description %in% useless_APA_cols) %>% 
    filter(!Land.Class %in% useless_land_class_cols) %>% 
    filter(!APA.Ownership.Description %in% useless_ownership_cols) %>%
    # only one instance found - essentially an exception
    drop_na()
    
# ------- MULTIPLE LINEAR REGRESSION ---------------------------------

# what maximizes the total sale value of a piece of property?
mlr_cols <- c(1, 4:8, 10, 18)
df_mlr <- df_w %>%
    select(all_of(mlr_cols))

# stepwise selection to find best MLR model
full_model <- lm(Total.Sale.Value ~ ., data=df_mlr)
summary(full_model)

best_step_model <- ols_step_both_aic(full_model)
best_subsets <- regsubsets(Total.Sale.Value ~ ., data=df_mlr)
best_subsets_results <- summary(best_subsets)
best_subsets_results$adjr2 
best_subsets_results$cp 
best_subsets_results$bic

# check for collinearity
cor(df_mlr, method="pearson")
ggpairs(df_mlr)
vif(full_model)

# all of these include a ton of colinear vars so they disappear
best_model <- lm(Total.Sale.Value ~ Calculated.Acreage + Total.Structures + 
                     Total.Units + Land.Sale.Value, data=df_mlr)
summary(best_model)
vif(best_model)
anova(best_model)

# UP TO NOW: chose above vars for full model and best_step_model chose several
# vars but they all have high collinearity. used ggpairs to see that 
# land value and TBSF have extremely high pearson correlation numbers
# so with that + variance inflation factor i ended up with this model

plot(best_model) 
# woah holy shit the assumptions are VIOLATED
# time to log transform methinks

# TOTAL UNITS HAS A LOT OF ZEROS... remove column and filter out all rows including a zero
best_log_model <- lm(log(Total.Sale.Value) ~ log(Calculated.Acreage) + log(Total.Structures) + 
                         log(Land.Sale.Value), data=df_mlr)
summary(best_log_model)
vif(best_log_model)
plot(best_log_model) # normal distribution is still violated
anova(best_log_model)

# ------- NAIVE BAYES ------------------------------------------------

# can we classify the potential rent of a piece of (vacant) land
# based on acerage, land value and total sale vale intervals?
# we're assuming that we're demolishing the structure in this land
# and building a new single-family house
# (total structures = 1)

nb_cols <- c(1, 2, 4:8, 10, 15) 
df_nb <- df_w %>%
    select(all_of(nb_cols)) %>%
    filter(APA.Activity.Description %in% "Household Activities" & 
               Land.Class %in% "Residential < 10 Acres" & Total.Structures == 1) %>%
    mutate(Calculated.Acerage.Class = case_when(Calculated.Acreage > 0.4 ~ "Large", 
                                                Calculated.Acreage <= 0.4 ~ "Small")) %>%
    mutate(Land.Value.Class = case_when(Land.Value < 50000 ~ "<50k",
                                  Land.Value >= 50000 & Land.Value < 100000 ~ "50k-100k",
                                  Land.Value >= 100000 ~ ">100k")) %>%
    mutate(Total.Sale.Class = case_when(Total.Sale.Value < 200000 ~ "<200k",
                                        Total.Sale.Value >= 200000 & Total.Sale.Value < 400000 ~ "200k-400k",
                                        Total.Sale.Value >= 400000 ~ ">400k")) %>%
    mutate(Estimated.Rent = round(abs(Land.Value - Total.Sale.Value) / 36), .before=10) %>%
    mutate(Estimated.Rent.Class = case_when(Estimated.Rent < 5000 ~ "<5k",
                                         Estimated.Rent >= 5000 & Estimated.Rent < 10000 ~ "5k-10k",
                                         Estimated.Rent >= 10000 ~ ">10k"))

# split training + testing data in a 70/30 split
n <- createDataPartition(y=df_nb$Estimated.Rent.Class, p=0.7, list=FALSE)
nb_train <- df_nb[n, ]
nb_test <- df_nb[-n, ]

model <- naiveBayes(Estimated.Rent.Class ~ Calculated.Acerage.Class + Land.Value.Class + Total.Sale.Class,
                    data=nb_train, laplace=0.5)
yhat <- predict(model, newdata=nb_test)
tab <- table(yhat, nb_test$Estimated.Rent.Class)
proportions <- round(prop.table(tab), 3)
misclass <- (sum(tab) - sum(diag(tab))) / sum(tab)
misclass # it's kinda high..

confusion_matrix <- confusionMatrix(factor(yhat), factor(nb_test$Estimated.Rent.Class), 
                                    dnn=c("Predicted", "Actual"))
temp <- as.data.frame(confusion_matrix$table)
temp$Predicted <- factor(temp$Predicted, levels=rev(levels(temp$Predicted)))

ggplot(temp, aes(Predicted, Actual, fill=Freq)) + theme_bw() +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="pink")
 
# ------- LOGISTIC REGRESSION ----------------------------------------

log_cols <- c(1, 3:7, 18, 19, 21)
df_log <- df_w %>%
    select(all_of(log_cols)) %>%
    filter(Land.Class.Code %in% c("G") & Type.And.Use.Description %in% c("ELEV APT", "GRDN APT")) %>%
    mutate(Type.And.Use.Description = ifelse(Type.And.Use.Description == "ELEV APT", 1, 0)) %>%
    select(-Land.Class.Code)
    # no houses or apts

# step selection w/ null and full model for glm
# and then use best model selection
# and then collinearity?
null_model <- glm(Type.And.Use.Description ~ 1, data=df_log, family="binomial")
full_model <- glm(Type.And.Use.Description ~ ., data=df_log, family="binomial")
step(full_model, scope=list(lower=null_model, full=full_model), direction="both", k=log(nrow(df_log))) # BIC

log_model <- glm(Type.And.Use.Description ~ Total.Structures + Building.Value, data=df_log, family="binomial")
summary(log_model)
plot(log_model)

# TODO CHECK DIAGNOSTICS AND SORT THIS OUT IN OFFICE HOURS TOMORROW
