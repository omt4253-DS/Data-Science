library(readr)
library(naniar)
train <- read.csv("C:\\Algoritmo\\Property_Price_Train.csv")
test <- read.csv("C:\\Algoritmo\\Property_Price_Test.csv")
head(train)


# Getting rid of the Id column
test_labels <- test$Id

test$Id <- NULL
train$Id <- NULL

test$Sale_Price <- NA
all <- rbind(train, test)
dim(all)



# Missing Values Analysis



# Check if there are missing values in the data frame using heatmap
vis_miss(all)



str(all)
#levels(all$Zoning_Class)

# Checking variables that has missing values
# FALSE tells us that the variable has missing values
!colSums(is.na(all))


Variables corresponding to FALSE has missing values.




#Printing the number of missing values for each variable
colSums(is.na(all))



Numbers corresponding to each variable indicates number of missing values for that variable. 


# Printing only those column names that has missing values along with the missing value counts

columns_with_missing_values <- sapply(all, function(x) sum(is.na(x))) 
columns_with_missing_values[columns_with_missing_values>0]



# Replacing NA Values with 'No Access' for Lane_Type variable
# Change the factor class type to character class first
all$Lane_Type<- as.character(all$Lane_Type)

# Filling NA with No Access
all$Lane_Type[is.na(all$Lane_Type)] <- "No Access"



# Replacing NA values with 'No Basement' for Basement_Height variable
# Check the class of the variable Basement_Height
class(all$Basement_Height)
# Change the factor class type to character class first
all$Basement_Height <- as.character(all$Basement_Height)
# Filling NA with No Access
all$Basement_Height[is.na(all$Basement_Height)] <- "No Basement"



# Replacing NA values with 'No Basement' for Basement_Condition variable
# Check the class of the variable Basement_Condition
class(all$Basement_Condition)
# Change the factor class type to character class first
all$Basement_Condition <- as.character(all$Basement_Condition)
# Filling NA with No Access
all$Basement_Condition[is.na(all$Basement_Condition)] <- "No Basement"



# Replacing NA values with 'No Basement' for Exposure_Level variable
# Check the class of the variable Exposure_Level
class(all$Exposure_Level)
# Change the factor class type to character class first
all$Exposure_Level <- as.character(all$Exposure_Level)
# Filling NA with No Access
all$Exposure_Level[is.na(all$Exposure_Level)] <- "No Basement"


# Replacing NA values with 'No Basement' for BsmtFinType1 variable
# Check the class of the variable BsmtFinType1
class(all$BsmtFinType1)
# Change the factor class type to character class first
all$BsmtFinType1 <- as.character(all$BsmtFinType1)
# Filling NA with No Access
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "No Basement"



# Replacing NA values with 'No Basement' for BsmtFinType2 variable
# Check the class of the variable BsmtFinType2
class(all$BsmtFinType2)
# Change the factor class type to character class first
all$BsmtFinType2 <- as.character(all$BsmtFinType2)
# Filling NA with No Access
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "No Basement"



# Replacing NA values with 'No Fireplace' for Fireplace_Quality variable
# Check the class of the variable Fireplace_Quality
class(all$Fireplace_Quality)
# Change the factor class type to character class first
all$Fireplace_Quality <- as.character(all$Fireplace_Quality)
# Filling NA with No Access
all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- "No Fireplace"



# Replacing NA values with 'No Garage' for Garage variable
# Check the class of the variable Garage
class(all$Garage)
# Change the factor class type to character class first
all$Garage <- as.character(all$Garage)
# Filling NA with No Access
all$Garage[is.na(all$Garage)] <- "No Garage"



# Replacing NA values with '0' for Garage_Built_Year variable
# Check the class of the variable Garage_Built_Year
class(all$Garage_Built_Year)
# Change the factor class type to character class first
#train$Garage_Built_Year <- as.character(train$Garage_Built_Year)
# Filling NA with No Access
all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- 0



# Replacing NA values with 'No Garage' for Garage_Finish_Year variable
# Check the class of the variable Garage_Finish_Year
class(all$Garage_Finish_Year)
# Change the factor class type to character class first
all$Garage_Finish_Year <- as.character(all$Garage_Finish_Year)
# Filling NA with No Access
all$Garage_Finish_Year[is.na(all$Garage_Finish_Year)] <- "No Garage"



# Replacing NA values with 'No Garage' for Garage_Quality variable
# Check the class of the variable Garage_Quality
class(all$Garage_Quality)
# Change the factor class type to character class first
all$Garage_Quality <- as.character(all$Garage_Quality)
# Filling NA with No Access
all$Garage_Quality[is.na(all$Garage_Quality)] <- "No Garage"


# Replacing NA values with 'No Garage' for Garage_Condition variable
# Check the class of the variable Garage_Condition
class(all$Garage_Condition)
# Change the factor class type to character class first
all$Garage_Condition <- as.character(all$Garage_Condition)
# Filling NA with No Access
all$Garage_Condition[is.na(all$Garage_Condition)] <- "No Garage"


# Replacing NA values with 'No Pool' for Pool_Quality variable
# Check the class of the variable Pool_Quality
class(all$Pool_Quality)
# Change the factor class type to character class first
all$Pool_Quality <- as.character(all$Pool_Quality)
# Filling NA with No Access
all$Pool_Quality[is.na(all$Pool_Quality)] <- "No Pool"



# Replacing NA values with 'No Fence' for Fence_Quality variable
# Check the class of the variable Fence_Quality
class(all$Fence_Quality)
# Change the factor class type to character class first
all$Fence_Quality <- as.character(all$Fence_Quality)
# Filling NA with No Access
all$Fence_Quality[is.na(all$Fence_Quality)] <- "No Fence"



# Replacing NA values with 'None' for Miscellaneous_Feature variable
# Check the class of the variable Fence
class(all$Miscellaneous_Feature)
# Change the factor class type to character class first
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature)
# Filling NA with No Access
all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- "None"



# Replacing missing values in Lot_Extent with its median value
# Check the class of the variable Fence
class(all$Lot_Extent)
# Lot_Extent is a continuous variable
# Filling missing values with median
all$Lot_Extent[is.na(all$Lot_Extent)] <- median(all$Lot_Extent, na.rm = TRUE)


# Using crosstab to generate the count of Brick_Veneer_Type by type of Brick_Veneer_Area
cross <- table(all$Brick_Veneer_Area,all$Brick_Veneer_Type)
cross_margin <- addmargins(cross)

head(cross_margin,10)



It can be observed that that when MasVnrArea is zero, we have
MasVnrType as None in the majority of cases



#impute the missing values in Brick_Veneer_Type with None and Brick_Veneer_Area with zero
all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <- 0
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature)
all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- "None"


#Let's see the distribution of the Electrical_System type by Building_Class
cross_elec <- table(all$Electrical_System,all$Building_Class)
cross_elec_margin <- addmargins(cross_elec)

head(cross_elec_margin,10)


class(all$Electrical_System)
all$Electrical_System <- as.character(all$Electrical_System)
all$Electrical_System[is.na(all$Electrical_System)] <- "SBrkr"


colSums(is.na(all))



columns_with_missing_values <- sapply(all, function(x) sum(is.na(x))) 
columns_with_missing_values[columns_with_missing_values>0]



From the above observation we can see that there are no missing values in our dataset



vis_miss(all)



<br>From the above plot notice that the plot shows no missing values in our data frame </br>


#Checking the data type for each variable
sapply(all, class)






numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars)
cat('There are', length(numericVars), 'numeric variables')


# Building the correlation matrix

library(corrplot)
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with Sale_Price
#cor_sorted <- as.matrix(sort(cor_numVar[,'Sale_Price'], decreasing = TRUE))
 #select only high corelations
#CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
#cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")



cor_numVar_sign <- round(cor_numVar,2)
cor_numVar_sign

## Building full multiple linear regression model 

# Full multiple linear regression model 
model1 <- lm(Sale_Price ~ ., all)

## Checking the summary model

summary(model1)


# predict the Sale_Price with all the X-values
#pdct1 = predict(model1, test)
#length(pdct1)
pdct1 = predict(model1, type="response", se.fit=FALSE)

length(pdct1)

# table showing the difference between the actual and predicted values
actual_Sale_Price = train$Sale_Price
length(actual_Sale_Price)

pred_Sale_Price = pdct1
difference = actual_Sale_Price - pred_Sale_Price
df_Sale_Price = data.frame(actual_Sale_Price, pred_Sale_Price,difference)
View(df_Sale_Price)




# Collect statistics associated with each variabel
# Loading the broom library to use the tidy function
library(broom)
tidy_model = tidy(model1)
options(scipen = 999)
# This gives statistics associated with all the variables
tidy_model


## Collecting all the significant variables 

#Collecting all the variables that has p-value less than 0.05
tidy_model$term[tidy_model$p.value < 0.05]
sigvar<-tidy_model$term[tidy_model$p.value < 0.05]
sigvar



Lets build another model with these significant variables

sigdf<-c(sigvar,"Sale_Price")
data1 <- all[,names(all) %in% sigdf]
sigvar_model <- lm(Sale_Price ~., data = data1)


summary(sigvar_model)

# predict the Sale_Price with all the significant variables

pdct_sig = predict(sigvar_model, type="response", se.fit=FALSE)

# table showing the difference between the actual and predicted values
actual_Sale_Price_sig = train$Sale_Price
length(actual_Sale_Price)

pred_Sale_Price_sig = pdct_sig

difference = actual_Sale_Price_sig - pred_Sale_Price_sig

df_Sale_Price = data.frame(actual_Sale_Price_sig, pred_Sale_Price_sig,difference)

View(df_Sale_Price)

fit <- lm(Sale_Price ~ Lot_Size + Overall_Material * House_Condition * Construction_Year + Brick_Veneer_Area+ First_Floor_Area*Second_Floor_Area *  Kitchen_Above_Grade, data = data1)

all$Construction_Year<-NULL

summary(fit)





