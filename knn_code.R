# KNN Approach #

library(tidyverse)
# Read in data #
train <- read_csv('train.csv')
test <- read_csv('test.csv')

# A group of variables I want to investigate #
zoning <- train['MSZoning']
building <- train['BldgType']
quality <- train['OverallQual']
lot_area <- train['LotArea']
price <- train['SalePrice']

# I chose to only look at lot_area and quality, so I will use column bind to create a subset of my
# training data to have only 3 rows: price, lot_area, and quality
sample_train <- cbind(price, lot_area, quality)

# I will do the same for the test data. Because we are trying to predict price, the test subset will have
# two columns: lot_area and quality
lot_area_test <- test['LotArea']
quality_test <- test['OverallQual']
sample_test <- cbind(lot_area_test, quality_test)


# Setting values before beginning the for-loop #
n <- nrow(sample_train) # number of training observations
m <- nrow(sample_test) # number of test observations
x <- 1:n # creates a list of numbers 1 to n. Will come in handy in the for-loop
k <- 3 # setting a value for k

# Creating empty vectors that will be needed in the for-loop #
lot_distance <- vector(mode="numeric", length=n) # https://stackoverflow.com/questions/12614953/how-to-create-a-numeric-vector-of-zero-length-in-r
quality_distance <- vector(mode="numeric", length=n)
overall_distance <- vector(mode="numeric", length=n)
pred_sales_prices <- vector(mode='numeric',length=h)


# Creating the for-loop #
for (j in 1:m){ # will create a loop to look at each observation in the test dataset
  full_samp_area <- sample_test['LotArea'][j,]
  full_samp_quality <- sample_test['OverallQual'][j,]
  for (i in 1:n) { # creates a nested for loop, finding distances of one test observation with all of the training observations
    lot_distance[i] <- full_samp_area - sample_train['LotArea'][i,]
    quality_distance[i] <- full_samp_quality - sample_train['OverallQual'][i,]
    overall_distance[i] <- sqrt((lot_distance[i])^2 + (quality_distance[i])^2)
  } # ends nested for loop
  sample_distances <- data.frame(x, overall_distance) # Creates a dataframe. Importing in finding the nearest neighbors row numbers
  sorted_distances <- sample_distances[order(sample_distances$overall_distance),] # sorts the neighbors
  desired_rows <- sorted_distances[1:k,] # takes the k nearest neighbor row numbers
  pred_sales_prices[j] <- (sum(sample_train[desired_rows$x,]$SalePrice)/k) # creates a vector of all predicted sales prices
}

## Explanation of for-loop ##
# 1) I create the first for-loop to look at the data of each test observation
# 2) I created the nested for-loop to find distances for each observation in the test dataset, comparing the
# test value to all of the training observations
# 3) I create 'sample_distances' to create a dataframe where x is the row number
# 4) We sort the distances, so that we can find the k closest neighbors
# 5) I then find the k nearest neighbors, and find the sale price of each of these neighbors, summing
# the prices and dividing by k to find the average.
# 6) In the end, pred_sales_prices creates a vector of sales prices, with the first price being the 
# predicted sales price of the first observation in the test dataset, and the last price being the 
# predicted sales price of the last observation in the test dataset.

## Note ##
# I chose two random variables, but the same idea applies to other variables. If we want to include categorical
# variables, we will want to find a way to standardize the variables so that the quantitative variables
# are not more important in the distance formula. We can talk about this part tomorrow.
# If you guys could use the for-loop and try different variables or make it more efficient, go for it!



