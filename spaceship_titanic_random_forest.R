##install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("Amelia")
install.packages("tidyr")

##load packages
library(ggplot2)
library(dplyr)
library(Amelia)
library(tidyr)

## Importing spaceship_train dataset
spaceship_train <- read.csv('train.csv')

head(spaceship_train)

summary(spaceship_train)

str(spaceship_train)

## EDA

#ggplot(spaceship_train, aes(Age)) + geom_histogram()

#ggplot(spaceship_train, aes(HomePlanet)) + geom_bar(aes(fill=VIP))

#table(is.na(spaceship_train))

## Missing Values
#missmap(spaceship_train,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

## Removing rows for blnk cryosleep and VIP
spaceship_train <- na.omit(subset(spaceship_train, VIP != "" | CryoSleep != ""))

## Feature Engineering

# Split 'Cabin' values into three separate columns and drop the original 'Cabin' column
spaceship_train <- separate(spaceship_train, Cabin, into = c("Deck", "Cabin_num", "Side"), sep = "/", remove = TRUE)


# Imputation for age by VIP Status
#pl <- ggplot(spaceship_train, aes(VIP, Age)) + geom_boxplot(aes(group=VIP, fill=factor(VIP), alpha=0.4))
#pl + scale_y_continuous(breaks = seq(min(0), max(80), by=2)) + theme_bw()

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

fixed.ages <- impute_age(spaceship_train$Age, spaceship_train$VIP)

spaceship_train$Age <- fixed.ages


## Replacing NA values with 0 in amount columns
col_to_zero <- c("ShoppingMall", "Spa", "VRDeck", "RoomService", "FoodCourt")
spaceship_train[col_to_zero] <- apply(spaceship_train[col_to_zero], 2, function(x) ifelse(is.na(x), 0, x))

## Creating a total spend metric
spaceship_train$Total_Spend <- spaceship_train$ShoppingMall + spaceship_train$Spa + spaceship_train$VRDeck + spaceship_train$RoomService + spaceship_train$FoodCourt

# Replace blank or null values with 'No Planet' in the 'HomePlanet' variable
spaceship_train$HomePlanet <- ifelse(spaceship_train$HomePlanet == "" | is.na(spaceship_train$HomePlanet), 'No Planet', spaceship_train$HomePlanet)

colnames(spaceship_train)

# Dropping not needed columns using indexing notation
spaceship_train <- spaceship_train[, !names(spaceship_train) %in% c("Name", "PassengerId", "Cabin_num", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")]

#converting columns to factor
columns_to_factor <- c("HomePlanet", "CryoSleep", "Destination", "VIP", "Transported", "Deck", "Side")

# Convert specific columns to factor
spaceship_train[columns_to_factor] <- lapply(spaceship_train[columns_to_factor], factor)

spaceship_train <- na.omit(spaceship_train)

str(spaceship_train)

### Random Forest
install.packages("randomForest")
library(randomForest)

# Train the random forest model
rf.model <- randomForest(Transported ~ . , data = spaceship_train)

rf.model

