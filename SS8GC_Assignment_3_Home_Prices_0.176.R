#Name: Sameer Singh, Nick Bruno, Wenxi Zhao
#Assignment: Assignment 3, Home Pricing
#Team Name: C1-2

#Provides "read_csv" function
library(readr)
#library(dplyr)
library(caret)

#Setting up the working directory
setwd("~/Desktop/UVA/Fall/Data Mining/Assignment/HW3 Home Prices")

#Reading training data
tr = read_csv("train.csv")
summary(tr)

#Analyzing the number of missing values
sum(is.na(tr))

#Finding columns which have missing values
na_cnt = as.data.frame(apply(tr,2,function(x) sum(is.na(x))))
names(na_cnt) = "Value"
na_cnt$Name = rownames(na_cnt)
na_cnt1 = arrange(na_cnt,na_cnt$Value)
na_cnt1 = na_cnt1[na_cnt1$Value!=0,]

#Creating a function to find mode of a character variable
Mode <- function(x) {
       ux <- unique(x)
       ux[which.max(tabulate(match(x, ux)))]
  }

#Substituting median value for the numerical missing values, and mode for character values
tr$Electrical[is.na(tr$Electrical)]= Mode(tr$Electrical)
tr$MasVnrArea[is.na(tr$MasVnrArea)]= median(tr$MasVnrArea,na.rm = T)
tr$BsmtQual[is.na(tr$BsmtQual)]= Mode(tr$BsmtQual)
tr$MasVnrType[is.na(tr$MasVnrType)]= Mode(tr$MasVnrType)
tr$BsmtCond[is.na(tr$BsmtCond)]= Mode(tr$BsmtCond)
tr$BsmtFinType1[is.na(tr$BsmtFinType1)]= Mode(tr$BsmtFinType1)
tr$BsmtExposure[is.na(tr$BsmtExposure)]= Mode(tr$BsmtExposure)
tr$BsmtFinType2[is.na(tr$BsmtFinType2)]= Mode(tr$BsmtFinType2)
tr$GarageType[is.na(tr$GarageType)]= Mode(tr$GarageType)
tr$GarageYrBlt[is.na(tr$GarageYrBlt)]= median(tr$GarageYrBlt,na.rm = T)
tr$GarageFinish[is.na(tr$GarageFinish)]= Mode(tr$GarageFinish)
tr$GarageQual[is.na(tr$GarageQual)]= Mode(tr$GarageQual)
tr$GarageCond[is.na(tr$GarageCond)]= Mode(tr$GarageCond)

#Finding columns which have missing values
na_cnt = as.data.frame(apply(tr,2,function(x) sum(is.na(x))))
names(na_cnt) = "Value"
na_cnt$Name = rownames(na_cnt)
na_cnt1 = arrange(na_cnt,na_cnt$Value)
na_cnt1 = na_cnt1[na_cnt1$Value!=0,]

# Convert categorical variables to factors in the training set
tr$Alley = as.factor(tr$Alley)
tr$BldgType = as.factor(tr$BldgType)
tr$BsmtCond = as.factor(tr$BsmtCond)
tr$BsmtExposure = as.factor(tr$BsmtExposure)
tr$BsmtFinType1 = as.factor(tr$BsmtFinType1)
tr$BsmtFinType2 = as.factor(tr$BsmtFinType2)
tr$BsmtQual = as.factor(tr$BsmtQual)
tr$CentralAir = as.factor(tr$CentralAir)
tr$Condition1 = as.factor(tr$Condition1)
tr$Condition2 = as.factor(tr$Condition2)
tr$Electrical = as.factor(tr$Electrical)
tr$ExterCond = as.factor(tr$ExterCond)
tr$ExterQual = as.factor(tr$ExterQual)
tr$Exterior1st = as.factor(tr$Exterior1st)
tr$Exterior2nd = as.factor(tr$Exterior2nd)
tr$Fence = as.factor(tr$Fence)
tr$FireplaceQu = as.factor(tr$FireplaceQu)
tr$Foundation = as.factor(tr$Foundation)
tr$Functional = as.factor(tr$Functional)
tr$GarageCond = as.factor(tr$GarageCond)
tr$GarageFinish = as.factor(tr$GarageFinish)
tr$GarageQual = as.factor(tr$GarageQual)
tr$GarageType = as.factor(tr$GarageType)
tr$Heating = as.factor(tr$Heating)
tr$HeatingQC = as.factor(tr$HeatingQC)
tr$HouseStyle = as.factor(tr$HouseStyle)
tr$KitchenQual = as.factor(tr$KitchenQual)
tr$LandContour = as.factor(tr$LandContour)
tr$LandSlope = as.factor(tr$LandSlope)
tr$LotConfig = as.factor(tr$LotConfig)
tr$LotShape = as.factor(tr$LotShape)
tr$MSZoning = as.factor(tr$MSZoning)
tr$MasVnrType = as.factor(tr$MasVnrType)
tr$MiscFeature = as.factor(tr$MiscFeature)
tr$Neighborhood = as.factor(tr$Neighborhood)
tr$PavedDrive = as.factor(tr$PavedDrive)
tr$PoolQC = as.factor(tr$PoolQC)
tr$RoofMatl = as.factor(tr$RoofMatl)
tr$RoofStyle = as.factor(tr$RoofStyle)
tr$SaleCondition = as.factor(tr$SaleCondition)
tr$SaleType = as.factor(tr$SaleType)
tr$Street = as.factor(tr$Street)
tr$Utilities = as.factor(tr$Utilities)
                             
# Possible categorical variables for analysis 
tr['has_pool'] <- ifelse(tr$PoolArea==0,0,1) # each value equal to one means there is a pool at that house
tr['has_alley_access'] <- ifelse(tr$Alley=='NA',0,1)
tr['has_basement'] <- ifelse(tr$BsmtQual=='<NA>',0,1)
tr['has_fireplace'] <- ifelse(tr$FireplaceQu=='NA',0,1)
tr['has_garage'] <- ifelse(tr$GarageQual=='NA',0,1) 
# After exploring these dummy variables, we found that none of them were statistically significant in our regression models.

#Keeping relevant columns in dataframe
tr_mod = as.data.frame(tr[,!names(tr) %in% na_cnt1$Name])

tr_mod$SalePrice = log(tr_mod$SalePrice,2)

#Dividing dataset into test and training
smp = sample(1:nrow(tr_mod),nrow(tr_mod)*.7)
tr_tr = tr_mod[smp,]
tr_tst = tr_mod[-smp,]

#Performing linear regression using all variables
reg = lm(SalePrice ~. -Id, data = tr_mod)
summary(reg)

#Checking the accuracy of the dataset on test data
reg_pr = predict(reg, newdata = tr_tst)
mse = mean((tr_tst$SalePrice-reg_pr)^2)

#Performing linear regression using limited variables
reg_ad = lm(SalePrice ~ MSZoning + LotArea + Street + LandContour + 
              LotConfig + LandSlope + Neighborhood + Condition1  + 
              BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + 
              YearRemodAdd + RoofStyle  + MasVnrType + 
              MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinType1 + 
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + `1stFlrSF` + `2ndFlrSF` + 
              FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
              Functional + Fireplaces + GarageFinish + GarageCars + GarageArea + 
              GarageQual + GarageCond + WoodDeckSF + ScreenPorch + PoolArea + 
              SaleCondition, 
   data = tr_tr)
summary(reg_ad)

#Checking the accuracy of the dataset on test data
reg_pr = predict(reg_ad, newdata = tr_tst)
mse = mean((tr_tst$SalePrice-reg_pr)^2)

#Running final linear regression model on the entire dataset
reg_all = lm(SalePrice ~ `1stFlrSF`+   `2ndFlrSF`+   BedroomAbvGr+   BsmtFinSF1+   BsmtFinSF2+   BsmtUnfSF+   Fireplaces+   GarageArea+   KitchenAbvGr+   LotArea+   MasVnrArea+   OverallCond+   OverallQual+   ScreenPorch+   YearBuilt+  WoodDeckSF + Functional+   MSZoning+   Condition1+  Condition2+  Exterior1st+   Neighborhood+   CentralAir+  Heating+  KitchenQual+  LandSlope+  RoofMatl+  SaleCondition, data = tr_mod)
summary(reg_all)

"
MSZoning + LotArea + Street + LandContour + 
               LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + 
YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinType1 + 
BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + `1stFlrSF` + `2ndFlrSF` + 
FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
Functional + Fireplaces + GarageFinish + GarageCars + GarageArea + 
GarageQual + GarageCond + WoodDeckSF + ScreenPorch + PoolArea + 
SaleCondition
"

#Reading test data
test = read_csv("test.csv")
summary(test)

#Analyzing the number of missing values
sum(is.na(test))
test_mod = test

na_cnt_test = as.data.frame(apply(test_mod,2,function(x) sum(is.na(x))))

#Substituting median value for the numerical missing values, and mode for character values
test_mod$LotFrontage[is.na(test_mod$LotFrontage)] = median(test_mod$LotFrontage,na.rm = T)
test_mod$GarageYrBlt[is.na(test_mod$GarageYrBlt)] = median(test_mod$GarageYrBlt,na.rm = T)
test_mod$MasVnrArea[is.na(test_mod$MasVnrArea)] = median(test_mod$MasVnrArea,na.rm = T)
test_mod$TotalBsmtSF[is.na(test_mod$TotalBsmtSF)] = median(test_mod$TotalBsmtSF,na.rm = T)
test_mod$PoolQC[is.na(test_mod$PoolQC)] = Mode(test_mod$PoolQC)
test_mod$MiscFeature[is.na(test_mod$MiscFeature)] = Mode(test_mod$MiscFeature)
test_mod$Alley[is.na(test_mod$Alley)] = Mode(test_mod$Alley)
test_mod$Fence[is.na(test_mod$Fence)] = Mode(test_mod$Fence)
test_mod$FireplaceQu[is.na(test_mod$FireplaceQu)] = Mode(test_mod$FireplaceQu)
test_mod$GarageFinish[is.na(test_mod$GarageFinish)] = Mode(test_mod$GarageFinish)
test_mod$GarageQual[is.na(test_mod$GarageQual)] = Mode(test_mod$GarageQual)
test_mod$GarageCond[is.na(test_mod$GarageCond)] = Mode(test_mod$GarageCond)
test_mod$GarageType[is.na(test_mod$GarageType)] = Mode(test_mod$GarageType)
test_mod$BsmtCond[is.na(test_mod$BsmtCond)] = Mode(test_mod$BsmtCond)
test_mod$BsmtQual[is.na(test_mod$BsmtQual)] = Mode(test_mod$BsmtQual)
test_mod$BsmtExposure[is.na(test_mod$BsmtExposure)] = Mode(test_mod$BsmtExposure)
test_mod$BsmtFinType1[is.na(test_mod$BsmtFinType1)] = Mode(test_mod$BsmtFinType1)
test_mod$BsmtFinType2[is.na(test_mod$BsmtFinType2)] = Mode(test_mod$BsmtFinType2)
test_mod$MasVnrType[is.na(test_mod$MasVnrType)] = Mode(test_mod$MasVnrType)
test_mod$MSZoning[is.na(test_mod$MSZoning)] = Mode(test_mod$MSZoning)
test_mod$Utilities[is.na(test_mod$Utilities)] = Mode(test_mod$Utilities)
test_mod$BsmtFullBath[is.na(test_mod$BsmtFullBath)] = Mode(test_mod$BsmtFullBath)
test_mod$BsmtHalfBath[is.na(test_mod$BsmtHalfBath)] = Mode(test_mod$BsmtHalfBath)
test_mod$Functional[is.na(test_mod$Functional)] = Mode(test_mod$Functional)
test_mod$Exterior1st[is.na(test_mod$Exterior1st)] = Mode(test_mod$Exterior1st)
test_mod$Exterior2nd[is.na(test_mod$Exterior2nd)] = Mode(test_mod$Exterior2nd)
test_mod$BsmtFinSF1[is.na(test_mod$BsmtFinSF1)] = Mode(test_mod$BsmtFinSF1)
test_mod$BsmtFinSF2[is.na(test_mod$BsmtFinSF2)] = Mode(test_mod$BsmtFinSF2)
test_mod$BsmtUnfSF[is.na(test_mod$BsmtUnfSF)] = Mode(test_mod$BsmtUnfSF)
test_mod$KitchenQual[is.na(test_mod$KitchenQual)] = Mode(test_mod$KitchenQual)
test_mod$GarageCars[is.na(test_mod$GarageCars)] = Mode(test_mod$GarageCars)
test_mod$GarageArea[is.na(test_mod$GarageArea)] = Mode(test_mod$GarageArea)
test_mod$SaleType[is.na(test_mod$SaleType)] = Mode(test_mod$SaleType)

# Convert categorical variables to factors in the test set
test_mod$Alley = as.factor(test_mod$Alley)
test_mod$BldgType = as.factor(test_mod$BldgType)
test_mod$BsmtCond = as.factor(test_mod$BsmtCond)
test_mod$BsmtExposure = as.factor(test_mod$BsmtExposure)
test_mod$BsmtFinType1 = as.factor(test_mod$BsmtFinType1)
test_mod$BsmtFinType2 = as.factor(test_mod$BsmtFinType2)
test_mod$BsmtQual = as.factor(test_mod$BsmtQual)
test_mod$CentralAir = as.factor(test_mod$CentralAir)
test_mod$Condition1 = as.factor(test_mod$Condition1)
test_mod$Condition2 = as.factor(test_mod$Condition2)
test_mod$Electrical = as.factor(test_mod$Electrical)
test_mod$ExterCond = as.factor(test_mod$ExterCond)
test_mod$ExterQual = as.factor(test_mod$ExterQual)
test_mod$Exterior1st = as.factor(test_mod$Exterior1st)
test_mod$Exterior2nd = as.factor(test_mod$Exterior2nd)
test_mod$Fence = as.factor(test_mod$Fence)
test_mod$FireplaceQu = as.factor(test_mod$FireplaceQu)
test_mod$Foundation = as.factor(test_mod$Foundation)
test_mod$Functional = as.factor(test_mod$Functional)
test_mod$GarageCond = as.factor(test_mod$GarageCond)
test_mod$GarageFinish = as.factor(test_mod$GarageFinish)
test_mod$GarageQual = as.factor(test_mod$GarageQual)
test_mod$GarageType = as.factor(test_mod$GarageType)
test_mod$Heating = as.factor(test_mod$Heating)
test_mod$HeatingQC = as.factor(test_mod$HeatingQC)
test_mod$HouseStyle = as.factor(test_mod$HouseStyle)
test_mod$KitchenQual = as.factor(test_mod$KitchenQual)
test_mod$LandContour = as.factor(test_mod$LandContour)
test_mod$LandSlope = as.factor(test_mod$LandSlope)
test_mod$LotConfig = as.factor(test_mod$LotConfig)
test_mod$LotShape = as.factor(test_mod$LotShape)
test_mod$MSZoning = as.factor(test_mod$MSZoning)
test_mod$MasVnrType = as.factor(test_mod$MasVnrType)
test_mod$MiscFeature = as.factor(test_mod$MiscFeature)
test_mod$Neighborhood = as.factor(test_mod$Neighborhood)
test_mod$PavedDrive = as.factor(test_mod$PavedDrive)
test_mod$PoolQC = as.factor(test_mod$PoolQC)
test_mod$RoofMatl = as.factor(test_mod$RoofMatl)
test_mod$RoofStyle = as.factor(test_mod$RoofStyle)
test_mod$SaleCondition = as.factor(test_mod$SaleCondition)
test_mod$SaleType = as.factor(test_mod$SaleType)
test_mod$Street = as.factor(test_mod$Street)
test_mod$Utilities = as.factor(test_mod$Utilities)

#Finding columns which have missing values
na_cnt_test = as.data.frame(apply(test_mod,2,function(x) sum(is.na(x))))

#Keeping relevant columns in dataframe
test_mod = as.data.frame(test_mod[,!names(test_mod) %in% na_cnt1$Name])

#Using regression to predict values for training dataset
sale_p = predict(reg_all, newdata = test_mod)
prediction = as.data.frame(cbind(test_mod$Id,sale_p))

prediction$sale_p = 2^prediction$sale_p

#Exporting the predictions as csv
write.table(prediction, file = "C1-2-house-prices.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")

###-----------------------------------------------------------------------------------###

#KNN Implementation

#Updating column names
colnames(tr_mod)[colnames(tr_mod)=="1stFlrSF"] = "firstFlrSF"
colnames(tr_mod)[colnames(tr_mod)=="2ndFlrSF"] = "secFlrSF"

colnames(test_mod)[colnames(test_mod)=="1stFlrSF"] = "firstFlrSF"
colnames(test_mod)[colnames(test_mod)=="2ndFlrSF"] = "secFlrSF"

#Combining values for different categorical variables
tr_mod$Condition1 = ifelse(tr_mod$Condition1!="Norm","Oth","Norm")
tr_mod$Exterior1st = ifelse(tr_mod$Exterior1st=="BrkComm"|tr_mod$Exterior1st=="BrkFace","Brk","Oth")
tr_mod$Heating = ifelse(tr_mod$Heating=="GasA","GasA","Oth")
tr_mod$RoofMatl = ifelse(tr_mod$RoofMatl=="CompShg","CompShg","Oth")
tr_mod$SaleCondition = ifelse((tr_mod$SaleCondition=="Partial"),"Partial",ifelse((tr_mod$SaleCondition=="Normal"),"Normal","Oth"))

test_mod$Condition1 = ifelse(test_mod$Condition1!="Norm","Oth","Norm")
test_mod$Exterior1st = ifelse(test_mod$Exterior1st=="BrkComm"|test_mod$Exterior1st=="BrkFace","Brk","Oth")
test_mod$Heating = ifelse(test_mod$Heating=="GasA","GasA","Oth")
test_mod$RoofMatl = ifelse(test_mod$RoofMatl=="CompShg","CompShg","Oth")
test_mod$SaleCondition = ifelse((test_mod$SaleCondition=="Partial"),"Partial",ifelse((test_mod$SaleCondition=="Normal"),"Normal","Oth"))

#Creating sample and training sets
smp = sample(1:nrow(tr),nrow(tr)*.7)
tr_tr = tr_mod[smp,]
tr_tst = tr_mod[-smp,]

#Creating array of relevant quantitative variables
rel_fact1 = c("firstFlrSF", "secFlrSF", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "Fireplaces", "GarageArea", "KitchenAbvGr", "LotArea", "MasVnrArea", "OverallCond", "OverallQual", "ScreenPorch", "YearBuilt","WoodDeckSF")

#Creating array of relevant qualitative variables 
rel_fact2 = c("Id", "Functional", "MSZoning", "Condition1","Exterior1st", "Neighborhood", "CentralAir","Heating","KitchenQual","LandSlope","RoofMatl","SaleCondition")

#Creating a subset of data for modelling, with only the relevant quantitative variables
tr_knn = tr_tr[,names(tr_tr) %in% rel_fact1]
test_knn = tr_tst[,names(tr_tst) %in% rel_fact1]

#Creating a subset of data for modelling, with ID and Sale Prices 
tr_knn_n = tr_tr[,names(tr_tr) %in% c("Id","SalePrice")] 

#Creating a subset of data for modelling, with only the relevant quantitative variables
tr_knn_c = tr_tr[,names(tr_tr) %in% rel_fact2]
test_knn_c = tr_tst[,names(tr_tst) %in% rel_fact2]

#Creating dummy values for each of the qualitative variables
tr_knnc = as.data.frame(model.matrix(Id ~ . -1, tr_knn_c))
test_knnc = as.data.frame(model.matrix(Id ~ . -1, test_knn_c))

#Combining quantitative and qualitative for both test and train datasets
tr_knn = cbind(tr_knn,tr_knnc)
test_knn = cbind(test_knn,test_knnc)

#Creating a function to standardize all the variables on scale of maximum and minimum values for the column
strd <- function(x) 
  {
  return ((x - min(x)) / (max(x) - min(x))) 
  }

#Calling the function to standardize all variables
tr_knn = as.data.frame(apply(tr_knn,2,strd))
test_knn = as.data.frame(apply(test_knn,2,strd))

#Substituting all NA values as 0
test_knn[is.na(test_knn)]=0
tr_knn[is.na(tr_knn)]=0

#Checking whether there are any records with NA values
sum(is.na(test_knn))

#Declaring k and prediction variable
k=3
pred_sp = NA

#Iterating through all records and performing KNN clustering
for(i in 1:nrow(test_knn))
  {
  tot_dis = NA
  dis = sweep(as.matrix(tr_knn),2,as.matrix(test_knn[i,]),"-")
  dis = dis^2
  tot_dis = apply(dis,1,sum)
  tot_dis = sqrt(tot_dis)
  tot_dis = cbind(tot_dis,tr_knn_n)
  tot_dis = tot_dis[order(tot_dis$tot_dis),]
  pred_sp[i] = mean(tot_dis[1:k,3])
}

#Calculating the sale price from prediction and train data, as we performed log transformation of the dependent variable earlier
tr_err = 2^tr_tst$SalePrice
pred_sp = 2^pred_sp

#Calculating the R-squared for the model
SSt = sum((tr_err - mean(tr_err))^2)
SSr = sum((pred_sp - mean(tr_err))^2)
SSr/SSt

###-----------------------------------------------------------------------------------###

#Performing clustering for the entire dataset using both quantitative and qualitative variables
tr_tr = tr_mod
tr_tst = test_mod

#Creating a subset of data for modelling, with only the relevant quantitative variables
tr_knn = tr_tr[,names(tr_tr) %in% rel_fact1]
test_knn = tr_tst[,names(tr_tst) %in% rel_fact1]

#Creating a subset of data for modelling, with ID and Sale Prices 
tr_knn_n = tr_tr[,names(tr_tr) %in% c("Id","SalePrice")] 

#Creating a subset of data for modelling, with only the relevant quantitative variables
tr_knn_c = tr_tr[,names(tr_tr) %in% rel_fact2]
test_knn_c = tr_tst[,names(tr_tst) %in% rel_fact2]

#Creating dummy values for each of the qualitative variables
tr_knnc = as.data.frame(model.matrix(Id ~ . -1, tr_knn_c))
test_knnc = as.data.frame(model.matrix(Id ~ . -1, test_knn_c))

#Combining quantitative and qualitative for both test and train datasets
tr_knn = cbind(tr_knn,tr_knnc)
test_knn = cbind(test_knn,test_knnc)

#Creating a function to standardize all the variables on scale of maximum and minimum values for the column
strd <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))) 
}

#Calling the function to standardize all variables
tr_knn = as.data.frame(apply(tr_knn,2,strd))
test_knn = as.data.frame(apply(test_knn,2,strd))

#Substituting all NA values as 0
test_knn[is.na(test_knn)]=0
tr_knn[is.na(tr_knn)]=0

#Checking whether there are any records with NA values
sum(is.na(test_knn))

#Declaring k and prediction variable
k=3
pred_sp = NA

#Iterating through all records and performing KNN clustering
for(i in 1:nrow(test_knn))
{
  tot_dis = NA
  dis = sweep(as.matrix(tr_knn),2,as.matrix(test_knn[i,]),"-")
  dis = dis^2
  tot_dis = apply(dis,1,sum)
  tot_dis = sqrt(tot_dis)
  tot_dis = cbind(tot_dis,tr_knn_n)
  tot_dis = tot_dis[order(tot_dis$tot_dis),]
  pred_sp[i] = mean(tot_dis[1:k,3])
}

#Calculating the sale price from prediction and train data, as we performed log transformation of the dependent variable earlier
tr_err = 2^tr_tst$SalePrice
pred_sp = 2^pred_sp

#Calculating the R-squared for the model
SSt = sum((tr_err - mean(tr_err))^2)
SSr = sum((pred_sp - mean(tr_err))^2)
SSr/SSt

#Creating a data frame with prediction and the unique ID for each record
pred_val = as.data.frame(cbind(test_mod$Id,pred_sp))

#Exporting the predictions as csv
write.table(pred_val, file = "C1-2-house-prices-knn-quan+qual.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")

###-----------------------------------------------------------------------------------###

#Performing clustering for the entire dataset using only quantitative variables
tr_tr = tr_mod
tr_tst = test_mod

#Creating a subset of data for modelling, with only the relevant quantitative variables
tr_knn = tr_tr[,names(tr_tr) %in% rel_fact1]
test_knn = tr_tst[,names(tr_tst) %in% rel_fact1]

#Creating a subset of data for modelling, with ID and Sale Prices 
tr_knn_n = tr_tr[,names(tr_tr) %in% c("Id","SalePrice")] 

#Creating a function to standardize all the variables on scale of maximum and minimum values for the column
strd <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))) 
}

#Calling the function to standardize all variables
tr_knn = as.data.frame(apply(tr_knn,2,strd))
test_knn = as.data.frame(apply(test_knn,2,strd))

#Substituting all NA values as 0
test_knn[is.na(test_knn)]=0
tr_knn[is.na(tr_knn)]=0

#Checking whether there are any records with NA values
sum(is.na(test_knn))

#Declaring k and prediction variable
k=3
pred_sp = NA

#Iterating through all records and performing KNN clustering
for(i in 1:nrow(test_knn))
{
  tot_dis = NA
  dis = sweep(as.matrix(tr_knn),2,as.matrix(test_knn[i,]),"-")
  dis = dis^2
  tot_dis = apply(dis,1,sum)
  tot_dis = sqrt(tot_dis)
  tot_dis = cbind(tot_dis,tr_knn_n)
  tot_dis = tot_dis[order(tot_dis$tot_dis),]
  pred_sp[i] = mean(tot_dis[1:k,3])
}

#Calculating the sale price from prediction and train data, as we performed log transformation of the dependent variable earlier
tr_err = 2^tr_tst$SalePrice
pred_sp = 2^pred_sp

#Calculating the R-squared for the model
SSt = sum((tr_err - mean(tr_err))^2)
SSr = sum((pred_sp - mean(tr_err))^2)
SSr/SSt

#Creating a data frame with prediction and the unique ID for each record
pred_val = as.data.frame(cbind(test_mod$Id,pred_sp))

#Exporting the predictions as csv
write.table(pred_val, file = "C1-2-house-prices-knn-quan.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")
