library(naniar)
library(ggplot2)
library(mice)
library(gridExtra)

store <- read.csv('csv/DA1920_store.csv')
train <- read.csv('csv/DA1920_train.csv')
test <- read.csv('csv/DA1920_test.csv')

# Missing data in store
gg_miss_var(store)

# Replace blank column with null
store_copy <- store
store_copy$PromoInterval[store_copy$PromoInterval == ""] <- NA

# Density plot of missing data for promo in store
x = store_copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = Promo2SinceYear_NA)) + 
  facet_wrap(~StoreType, ncol = 4) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

y = store_copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = Promo2SinceWeek_NA)) + 
  facet_wrap(~StoreType, ncol = 4) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

z = store_copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = PromoInterval_NA)) + 
  facet_wrap(~StoreType, ncol = 4) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

grid.arrange(x,y,z, nrow=3, ncol=1, top='Distribution of missing data in Promo2SinceYear, Promo2SinceWeek, and PromoInterval')

# Density plot of missing data for competition in store
x = store_copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = CompetitionOpenSinceMonth_NA)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

y = store_copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = CompetitionOpenSinceYear_NA)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

grid.arrange(x,y, nrow=2, ncol=1, top='Distribution of missing data in CompetitionOpenSinceMonth, CompetitionOpenSinceYear')
