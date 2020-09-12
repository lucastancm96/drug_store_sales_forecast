library(naniar)
library(ggplot2)
library(mice)

store <- read.csv('csv/DA1920_store.csv')
train <- read.csv('csv/DA1920_train.csv')
test <- read.csv('csv/DA1920_test.csv')

# Missing data in store
gg_miss_var(store)

# Density plot of missing data in store

