library(naniar)
library(ggplot2)
library(mice)
library(gridExtra)

# ******************** Import data ********************
store <- read.csv('csv/DA1920_store.csv')
train <- read.csv('csv/DA1920_train.csv')
test <- read.csv('csv/DA1920_test.csv')

# ******************** Plot data ********************
# Missing data in store
gg_miss_var(store)

# Replace blank column with null
store_copy <- store
store_copy$PromoInterval[store_copy$PromoInterval == ""] <- NA

# ========= Promo ==========
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

# ========== Competition ==========
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

# Competition Distance
store <- store[, 1:(ncol(store)-2)]

# ---> Create 5 level of competition distance (quntile 1 = closest to competitors)
store$CompDistBin <- ntile(store$CompetitionDistance, 5)
store$CompDistBin <- paste(store$CompDistBin, "quintile")
store$CompDistBin[store$CompDistBin == "NA quintile"] <- NA
store$CompOpenDate <- ymd(paste(store$CompetitionOpenSinceYear, 
                                store$CompetitionOpenSinceMonth, 
                                "01"))

# ---> Create competitors open date
store$CompOpenDate <- as.Date(store$CompOpenDate)
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, 
                                               sep = "-"))

# ---> Feature engineering AvgSales and AvgCustomers
store <- left_join(store, summarize(group_by(train[train$Open == 1,], Store), 
                                     AvgSales = mean(Sales, na.rm = TRUE), 
                                     AvgCustomers = mean(Customers, na.rm = TRUE)), "Store")

# Merge store and train
mtrain <- merge(train, store, by = "Store")
mtrain$Date <- as.Date(mtrain$Date)
mtrain$CompetitionOpenSinceYear <- as.factor(mtrain$CompetitionOpenSinceYear)

# Plot competition distance with average sales and customers
compdist_Sales <- aggregate(mtrain[Sales != 0 & !is.na(CompetitionDistance)]$Sales,
                      by = list(mtrain[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), 
                      mean)

compdist_Cust <- aggregate(mtrain[Customers != 0 & !is.na(CompetitionDistance)]$Customers, 
                           by = list(mtrain[Customers != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), 
                           mean)

colnames(compdist_Sales) <- c("CompetitionDistance", "Mean")

x <- ggplot(compdist_Sales, 
           aes(x = CompetitionDistance, y = Mean)) + 
  geom_point(color='#1F545A') + 
  geom_smooth(color='#F83C33') + 
  xlab('Competition distance') + 
  ylab('Average Sales') + 
  theme_minimal()

colnames(compdist_Cust) <- c("CompetitionDistance", "Mean")

y <- ggplot(compdist_Cust, 
           aes(x = CompetitionDistance, y = Mean)) + 
  geom_point(color='#1F545A') + 
  geom_smooth(color='#F83C33') + 
  xlab('Competition distance') + 
  ylab('Average Customers') + 
  theme_minimal()

grid.arrange(x, y, top='Competition Distance vs Avg Sales and Customers')

# ******************** Data Imputation ********************
# ---> Import store
store <- read.csv('csv/DA1920_store.csv')
store <- store[, 1:(ncol(store)-2)]

# ---> Impute data
impvar <- store[, c('CompetitionDistance', 'CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear')]
impval <- mice(impvar, method='pmm', seed=42)
compimp <- complete(impval, 5)

# ---> Replace empty data with imputed data
impcol <- list(colnames(compimp))
for (col in impcol){
  store[col] <- compimp[col]
}

# ---> Visualize imputed values (red=imputed, blue=original)
densityplot(impval, data = ~ CompetitionDistance + CompetitionOpenSinceMonth + CompetitionOpenSinceYear)

write.csv(store, 'csv/imp_store.csv', row.names=FALSE)