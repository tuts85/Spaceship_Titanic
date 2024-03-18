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

fixed.ages <- impute_age(train$Age, train$VIP)

train$Age <- fixed.ages

colnames(train)

## Replacing NA values with 0 in amount columns
col_to_zero <- c("ShoppingMall", "Spa", "VRDeck", "RoomService", "FoodCourt")
train[col_to_zero] <- apply(train[col_to_zero], 2, function(x) ifelse(is.na(x), 0, x))

## Creating a total spend metric
train$Total_Spend <- train$ShoppingMall + train$Spa + train$VRDeck + train$RoomService + train$FoodCourt

## changing chr to factors
# Assuming df is your dataframe
columns_to_factor <- c("HomePlanet", "CryoSleep", "Destination", "VIP", "Transported")

# Convert specific columns to factor
train[columns_to_factor] <- lapply(train[columns_to_factor], factor)

str(train)

### Creating Models ###

##Linear Regression
model_linear <- lm(Transported ~. - Name , data = train)

summary(model_linear)
