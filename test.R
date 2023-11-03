library(tidyverse)
setwd("C:/Users/marco/Desktop/CMDA 4654/Projects/Project 1")

df <- read.csv("data/property.csv", header=TRUE)
wake_cols <- c(10, 19:21, 24:29, 34, 43, 48, 50, 53, 59:61)

df_w <- df %>%
    select(all_of(wake_cols)) %>%
    mutate_if(is.character, list(~na_if(.,""))) %>%
    drop_na()
