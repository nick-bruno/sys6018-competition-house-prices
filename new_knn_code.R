##### Data Exploration #####
### Read in data ###
train <- read_csv('train.csv')
test <- read_csv('test.csv')

# First, I want to convert the categorical variables to factors #
train$MSSubClass <- as.factor(train$MSSubClass)
train$MSZoning <- as.factor(train$MSZoning)
train$Street <- as.factor(train$Street)
train$Alley <- as.factor(train$Alley)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)
train$Utilities <- as.factor(train$Utilities)
train$LotConfig <- as.factor(train$LotConfig)
train$LandSlope <- as.factor(train$LandSlope)
train$Neighborhood <- as.factor(train$Neighborhood)
train$Condition1 <- as.factor(train$Condition1)
train$Condition2 <- as.factor(train$Condition2)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$OverallCond <- as.factor(train$OverallCond)
train$YearBuilt <- as.factor(train$YearBuilt)
train$YearRemodAdd <- as.factor(train$YearRemodAdd)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$MasVnrType <- as.factor(train$MasVnrType)
train$ExterQual <- as.factor(train$ExterQual)
train$ExterCond <- as.factor(train$ExterCond)
train$Foundation <- as.factor(train$Foundation)
train$BsmtQual <- as.factor(train$BsmtQual)
train$BsmtCond <- as.factor(train$BsmtCond)
train$BsmtExposure <- as.factor(train$BsmtExposure)
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)
train$Heating <- as.factor(train$Heating)
train$HeatingQC <- as.factor(train$HeatingQC)
train$CentralAir <- as.factor(train$CentralAir)
train$Electrical <- as.factor(train$Electrical)
train$KitchenQual <- as.factor(train$KitchenQual)
train$Functional <- as.factor(train$Functional)
train$FireplaceQu <- as.factor(train$FireplaceQu)
train$GarageType <- as.factor(train$GarageType)
train$GarageYrBlt <- as.factor(train$GarageYrBlt) # can keep as numeric possibly
train$GarageFinish <- as.factor(train$GarageFinish)
train$GarageQual <- as.factor(train$GarageQual)
train$GarageCond <- as.factor(train$GarageCond)
train$PavedDrive <- as.factor(train$PavedDrive)
train$PoolQC <- as.factor(train$PoolQC)
train$Fence <- as.factor(train$Fence)
train$MiscFeature <- as.factor(train$MiscFeature)
train$MoSold <- as.factor(train$MoSold)
train$YrSold <- as.factor(train$YrSold) # could keep as numeric
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)

### POSSIBLE CATEGORICAL VARIABLES ###
# To deal with some null values, we felt it would be interesting to create dummy variables that indicate
# whether some of the categorical characteristics are present in a data observation.
train['has_pool'] <- ifelse(train$PoolArea==0,0,1) # each value equal to one means there is a pool at that house
train['has_alley_access'] <- ifelse(train$Alley=='NA',0,1)
train['has_basement'] <- ifelse(train$BsmtQual=='<NA>',0,1)
train['has_fireplace'] <- ifelse(train$FireplaceQu=='NA',0,1)
train['has_garage'] <- ifelse(train$GarageQual=='NA',0,1)


##### KNN Statistical Modeling Method #####


# A group of variables I want to investigate #
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

### Setting up the cross-validation ###
total_training_observations <- nrow(sample_train)
samp <- sample(1:total_training_observations,size=(total_training_observations*0.8))
train_set <- sample_train[samp,]
valid_set <- sample_train[-samp,]
n <- nrow(train_set)
m <- nrow(valid_set)
x <- 1:n
k <- 3

# Creating empty vectors that will be needed in the for-loop #
lot_distance <- vector(mode="numeric", length=n) # https://stackoverflow.com/questions/12614953/how-to-create-a-numeric-vector-of-zero-length-in-r
quality_distance <- vector(mode="numeric", length=n)
overall_distance <- vector(mode="numeric", length=n)
valid_pred_sales_prices <- vector(mode='numeric',length=m)


### Creating the for-loop ###
for (j in 1:m){ # will create a loop to look at each observation in the test dataset
  full_samp_area <- valid_set['LotArea'][j,]
  full_samp_quality <- valid_set['OverallQual'][j,]
  for (i in 1:n) { # creates a nested for loop, finding distances of one test observation with all of the training observations
    lot_distance[i] <- full_samp_area - train_set['LotArea'][i,]
    quality_distance[i] <- full_samp_quality - train_set['OverallQual'][i,]
    overall_distance[i] <- sqrt((lot_distance[i])^2 + (quality_distance[i])^2)
  } # ends nested for loop
  sample_distances <- data.frame(x, overall_distance) # Creates a dataframe. Importing in finding the nearest neighbors row numbers
  sorted_distances <- sample_distances[order(sample_distances$overall_distance),] # sorts the neighbors
  desired_rows <- sorted_distances[1:k,] # takes the k nearest neighbor row numbers
  valid_pred_sales_prices[j] <- (sum(train_set[desired_rows$x,]$SalePrice)/k) # creates a vector of all predicted sales prices
}

# Now I will compare my predicted results with the actual sales prices
valid_set$pred_sale_price <- valid_pred_sales_prices # adds the predicted prices to the 'valid' subset

# Calculating the MSE #
mse <- (sum(valid_set$SalePrice - valid_set$pred_sale_price)^2 / m)
  # 320443520


# Now I will test the same cross-validation using a normalized approach called min-max normalizing #
# This will use the formula (x-min(x)/(min(x)+max(x))) #
# source: https://www.quora.com/What-are-the-best-normalization-techniques-in-data-mining #

### First, I will standardize the training and validation set ###
# Normalizing training set #
min_lot_train <- min(train_set$LotArea) 
max_lot_train <- max(train_set$LotArea) 
min_quality_train <- min(train_set$OverallQual) 
max_quality_train <- max(train_set$OverallQual) 

# Normalizing validation set #
min_lot_test <- min(valid_set$LotArea)
max_lot_test <- max(valid_set$LotArea)
min_quality_test <- min(valid_set$OverallQual)
max_quality_test <- max(valid_set$OverallQual)

# Creating empty vectors for our for-loop #
standardized_train_samp_area <-vector(mode="numeric", length=n)
standardized_train_samp_quality  <- vector(mode="numeric", length=n)
standardized_valid_samp_area <-vector(mode="numeric", length=m)
standardized_valid_samp_quality  <- vector(mode="numeric", length=m)
normalized_pred_sales_prices <- vector(mode="numeric", length=m)

# Finding the normalized values for the training set #
for (i in (1:n)){
  standardized_train_samp_area[i] <- (train_set['LotArea'][i,] - min_lot_train)/(max_lot_train - min_lot_train)
  standardized_train_samp_quality[i] <- (train_set['OverallQual'][i,] - min_quality_train)/(max_quality_train - min_quality_train)
}

# Finding the normalized values for the validation set #
for (i in (1:m)) {
  standardized_valid_samp_area[i] <- (valid_set['LotArea'][i,] - min_lot_test)/(max_lot_test - min_lot_test)
  standardized_valid_samp_quality[i] <- (valid_set['OverallQual'][i,] - min_quality_test) / (max_quality_test - min_quality_test)
}

# Adding the normalized values to the datasets #
train_set$Stand_LotArea <- standardized_train_samp_area
train_set$Stand_Quality <- standardized_train_samp_quality
valid_set$Stand_LotArea <- standardized_valid_samp_area
valid_set$Stand_Quality <- standardized_valid_samp_quality

# Creating a for-loop to find the k-nearest neighbors and predict sales prices
for (j in 1:m){ 
  full_samp_area <- valid_set['Stand_LotArea'][j,]
  full_samp_quality <- valid_set['Stand_Quality'][j,]
  for (i in 1:n) { # creates a nested for loop, finding distances of one test observation with all of the training observations
    lot_distance[i] <- full_samp_area - train_set['Stand_LotArea'][i,]
    quality_distance[i] <- full_samp_quality - train_set['Stand_Quality'][i,]
    overall_distance[i] <- sqrt((lot_distance[i])^2 + (quality_distance[i])^2) # still use distance formula
  } # ends nested for loop
  sample_distances <- data.frame(x, overall_distance) # Creates a dataframe. Importing in finding the nearest neighbors row numbers
  sorted_distances <- sample_distances[order(sample_distances$overall_distance),] # sorts the neighbors
  desired_rows <- sorted_distances[1:k,] # takes the k nearest neighbor row numbers
  normalized_pred_sales_prices[j] <- (sum(sample_train[desired_rows$x,]$SalePrice)/k) # creates a vector of all predicted sales prices
}

# Now I will compare my predicted results with the actual sales prices
valid_set$normalized_pred_sale_price <- normalized_pred_sales_prices 

# Calculating the MSE #
mse_normalized <- (sum(valid_set$SalePrice - valid_set$normalized_pred_sale_price)^2 / m)
  # 452254619

difference_in_mse <- mse_normalized - mse 
  # 131811099

# Because the MSE in the first model was smaller, the regular KNN method seemed to predict price more
# accurately compared to the min-max normalization approach.

### Comparing results ###
# It seems that using min-max normalization seemed more ideal in theory, we found that using the regular
# KNN method utilizing manhattan distance provided more accurate results compared to the normalization
# method. Because there is a much greater variance in LotArea compared to OverallQuality in these observations,
# the OverallQual variable may have been given greater emphasis in the min-max normalization technique, 
# while it seemed to be under emphasized in the normal, manhattan distance version of KNN.

### Applying the results to predict the test data ###
# Lastly, I will apply my first KNN non-parametric approach to the test data to create predicted prices
# for each observation, using the 'sample_train' and 'sample_test' subsets from earlier

# Create vectors for the for-loops and set values #
k <- 3
p <- nrow(sample_test)
q <- nrow(sample_train)
x <- 1:q
lot_distance <- vector(mode="numeric", length=q)
quality_distance <- vector(mode="numeric", length=q)
overall_distance <- vector(mode="numeric", length=q)
actual_pred_sales_prices <- vector(mode="numeric", length=p)


# Creating the for-loop to predict the sales prices of the test data based off of the normal KNN approach
# using the LotArea and OverallQual variables
for (j in 1:p){ 
  full_samp_area <- sample_test['LotArea'][j,]
  full_samp_quality <- sample_test['OverallQual'][j,]
  for (i in 1:q) { 
    lot_distance[i] <- full_samp_area - sample_train['LotArea'][i,]
    quality_distance[i] <- full_samp_quality - sample_train['OverallQual'][i,]
    overall_distance[i] <- sqrt((lot_distance[i])^2 + (quality_distance[i])^2)
  } # ends nested for loop
  sample_distances <- data.frame(x, overall_distance) 
  sorted_distances <- sample_distances[order(sample_distances$overall_distance),] # sorts the neighbors
  desired_rows <- sorted_distances[1:k,] # takes the k nearest neighbor row numbers
  actual_pred_sales_prices[j] <- (sum(sample_train[desired_rows$x,]$SalePrice)/k) # creates a vector of all predicted sales prices
}

# Adding the predictions to create a final dataset
test$predicted_prices <- actual_pred_sales_prices

## Write results to a csv ##
write.table(test, file='Sale_Prices_Predictions.csv', row.names=F, sep=',')




