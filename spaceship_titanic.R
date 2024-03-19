##install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("Amelia")

##load packages
library(ggplot2)
library(dplyr)
library(Amelia)

## Importing train dataset
train <- read.csv('train.csv')

head(train)

summary(train)

str(train)

## EDA

ggplot(train, aes(Age)) + geom_histogram()

ggplot(train, aes(HomePlanet)) + geom_bar(aes(fill=VIP))

table(is.na(train))

## Missing Values
missmap(train,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

## Removing rows for blnk cryosleep and VIP
train_clean <- na.omit(subset(train, VIP != "" & CryoSleep != ""))

## Feature Engineering

# Imputation for age by VIP Status
pl <- ggplot(train, aes(VIP, Age)) + geom_boxplot(aes(group=VIP, fill=factor(VIP), alpha=0.4))
pl + scale_y_continuous(breaks = seq(min(0), max(80), by=2)) + theme_bw()

#function
impute_age <- function(age, vip) {
  out <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (vip[i] == 'True'){
        out[i] <- 34
      } else {
        out[i] <- 27
      }
    } else {
      out[i] <- age[i]
    } 
  } 
  return(out)
}

fixed.ages <- impute_age(train_clean$Age, train_clean$VIP)

train_clean$Age <- fixed.ages

colnames(train)

## Replacing NA values with 0 in amount columns
col_to_zero <- c("ShoppingMall", "Spa", "VRDeck", "RoomService", "FoodCourt")
train_clean[col_to_zero] <- apply(train_clean[col_to_zero], 2, function(x) ifelse(is.na(x), 0, x))

## Creating a total spend metric
train_clean$Total_Spend <- train_clean$ShoppingMall + train_clean$Spa + train_clean$VRDeck + train_clean$RoomService + train_clean$FoodCourt

## changing chr to factors
# Assuming df is your dataframe
columns_to_factor <- c("HomePlanet", "CryoSleep", "Destination", "VIP", "Transported")

# Convert specific columns to factor
train[columns_to_factor] <- lapply(train[columns_to_factor], factor)

str(train_clean)

### Creating Models ###

##Logistic Regression
log.model <- glm(Transported ~ HomePlanet + CryoSleep + Destination + Age + VIP + Total_Spend , family = binomial(link = 'logit'), data = train_clean)

summary(log.model)

ggplot(log.model, aes(log.model$residuals)) + geom_histogram()

# Test dataset
test <- read.csv("test.csv")

# Fixing ages
fixed.ages.test <- impute_age(test$Age, test$VIP)

test$Age <- fixed.ages.test

## Removing rows for blnk cryosleep and VIP
test_clean <- na.omit(subset(test, VIP != "" & CryoSleep != ""))

## Replacing NA values with 0 in amount columns
col_to_zero <- c("ShoppingMall", "Spa", "VRDeck", "RoomService", "FoodCourt")
test[col_to_zero] <- apply(test[col_to_zero], 2, function(x) ifelse(is.na(x), 0, x))

## Creating a total spend metric
test$Total_Spend <- test$ShoppingMall + test$Spa + test$VRDeck + test$RoomService + test$FoodCourt

# Predicting on test
fitted.values <- predict(log.model, test_clean, type = 'response')
fitted.results <- ifelse(fitted.values>0.5, 'True', 'False')

test_clean$Transported <- fitted.results

write.csv(test_clean, file = 'titanic_kaggle_log.csv')


### For submission
##Logistic Regression
log.model.sub <- glm(Transported ~ HomePlanet + CryoSleep + Destination + Age + VIP + Total_Spend , family = binomial(link = 'logit'), data = train)

summary(log.model)

ggplot(log.model, aes(log.model$residuals)) + geom_histogram()

# Fixing ages
fixed.ages.test <- impute_age(test$Age, test$VIP)

test$Age <- fixed.ages.test

## Removing rows for blnk cryosleep and VIP
test_clean <- na.omit(subset(test, VIP != "" & CryoSleep != ""))

## Replacing NA values with 0 in amount columns
col_to_zero <- c("ShoppingMall", "Spa", "VRDeck", "RoomService", "FoodCourt")
test[col_to_zero] <- apply(test[col_to_zero], 2, function(x) ifelse(is.na(x), 0, x))

## Creating a total spend metric
test$Total_Spend <- test$ShoppingMall + test$Spa + test$VRDeck + test$RoomService + test$FoodCourt

# Predicting on test
fitted.values.sub <- predict(log.model.sub, test, type = 'response')
fitted.results.sub <- ifelse(fitted.values.sub>0.5, TRUE, FALSE)

test$Transported <- fitted.results.sub

write.csv(test, file = 'titanic_kaggle_log.csv')

