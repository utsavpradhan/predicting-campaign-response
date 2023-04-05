#------------------------------------------------------
#####      Final Project        ###########
#-------------------------------------------------------
#### Marketing Campaign #########
#---------------------------------------------------------

# Clearing the environment
rm(list=ls())

#Setting Working Directory
setwd("D:/Drexel2021/Course/Spring/Data Mining/Final Project")


#Reading target file, importing blank spaces as NA
mkt <- read.csv(file = "MarketingCampaign.csv",
                na.strings = c("", " "),
                stringsAsFactors = FALSE)



#Loading packages
library(caret)
library("RColorBrewer")
library(NeuralNetTools)

## Previewing the data
head(x = mkt)
tail(x = mkt)

##View the structure of the dataset
str(mkt)

#Viewing the rows and columns

nrow(mkt) # number of rows = 9134

ncol(mkt) # number of columns = 22
colnames(mkt) # obtain column names

##Conversion to Alternate Variable Types

# Setting up vectors to  convert
# groups of variables at the same time.

#Nominal categorical variables as facs
facs <- c( "State", "EmploymentStatus","Gender",
           "Location.Code","Marital.Status", "Policy.Type", 
           "Policy","Renew.Offer.Type", "Sales.Channel",
           "Vehicle.Class", "Response")

#Ordinal Variables as ords
ords<- c("Education", "Coverage", "Vehicle.Size")

#Numerical Variable as nums
nums <- c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto",
          "Months.Since.Last.Claim","Number.of.Policies","Total.Claim.Amount")

mkt <- mkt[ ,-c(1,6)]

##Excluded variable "Customer" as it is only a unique identifier
##Not relevant to the scope of our analysis.
##Excluded "Effective to Date" variable for the same.

#Checking the unique values that the variables can take on.

#For all facs and ords variables
lapply(X = mkt[ ,c(facs, ords)], 
       FUN = unique)

# Converting facs variables to factors
mkt[ ,facs] <- lapply(X = mkt[ , facs], 
                      FUN = factor)

#Converting to Ordinal
mkt$Coverage <- factor(x = mkt$Coverage, 
                       levels = c("Basic","Extended","Premium"),
                       ordered = TRUE)
mkt$Vehicle.Size <- factor(x = mkt$Vehicle.Size,
                           levels = c("Small", "Medsize","Large"),
                           ordered = TRUE)
mkt$Education <- factor(x = mkt$Education,
                        levels = c("High School or Below", 
                                   "College","Bachelor",
                                   "Master","Doctor"),
                        ordered = TRUE)

#----------------------------------------------------------
#########   Data Exploration   ############
#----------------------------------------------------------
#Rechecking the structure
str(mkt)

#Mode function from Professor Hill's codes
modefun <- function(x){
  if(any(tabulate(match(x, unique(x))) > 1)){
    outp <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
  } else {
    outp <- "No mode exists!"}
  return(outp)
}

# Applying mode to all categorical variables in the mkt dataframe
lapply(X = mkt[ ,c(facs, ords)], 
       FUN = modefun)

# Statistical Summary of Numerical variables from mkt dataframe
sapply(X = mkt[,nums], 
       FUN = summary)

#Standard deviation of numerical variables
sapply(X = mkt[ , nums], 
       FUN = sd)
#CLV and Income have high SD.

#Exploring all categorical variables through 1-way frequency table
lapply(X = mkt[ ,c(facs,ords)], FUN = table)

#What variables should we compare/explore?
## 2-Way Frequency Table
#Response and Gender
ct1 <- table(Response = mkt$Response, 
             Gender = mkt$Gender)
ct1
#Seemingly no relationship between response and gender.

#Response and State
ct2 <- table(Response = mkt$Response, 
             State = mkt$State)
ct2
#California and Oregon had higher responses than others.
#Nevada has lowest

#Response and Employment Status
ct3 <- table(Response = mkt$Response, 
             Employment = mkt$EmploymentStatus)
ct3
#More yes and than no from Retired 
#Low response among unemployed.

#Response and Vehicle Size
ct4 <- table(Response = mkt$Response, 
             VehicleSize = mkt$Vehicle.Class)
ct4
#Nothing stands out.

#Response and Vehicle Class
ct5 <- table(Response = mkt$Response, 
             Offer = mkt$Renew.Offer.Type)
ct5
#Offer 4 had no responses. Offer 3 had very low response rate.

#Response and location code
ct6 <- table(Response = mkt$Response, 
             Location = mkt$Location.Code)
ct6
#Suburban has a higher response rate 17%. Rural has 
#9% and urban has 8%.

###Grouped Summary Information

##CLV and State
aggregate(formula = Customer.Lifetime.Value ~ State, 
          data = mkt, 
          FUN = summary)
#No significant fluctuation in CLV by state

## CLV and Location Code
aggregate(formula = Customer.Lifetime.Value ~ Location.Code, 
          data = mkt, 
          FUN = summary)
#No significant difference in CLV. High difference in claim amount. 

#Check Claim Amount
aggregate(formula = Total.Claim.Amount ~ Location.Code, 
          data = mkt, 
          FUN = summary)
#Suburban has significantly higher claim amount. Rural has lowest.

#Check Premium
aggregate(formula = Monthly.Premium.Auto ~ Location.Code, 
          data = mkt, 
          FUN = summary)
#Suburban pay slightly more premium.

##CLV and Policy Type
aggregate(formula = Customer.Lifetime.Value ~ Policy.Type, 
          data = mkt, 
          FUN = summary)
#No significant fluctuation in CLV

##Clv and response to last campaign
aggregate(formula = Customer.Lifetime.Value ~ Response, 
          data = mkt, 
          FUN = summary)
#Can't see a strong relation. CLV seems higher in people
#who responded no.

##Total claim amount and response to last campaign
aggregate(formula = Total.Claim.Amount ~ Response, 
          data = mkt, 
          FUN = summary)
#Up until the mean/median, claim amount is higher among
#those who responded yes.

##Claim amount and Gender 
aggregate(formula = Total.Claim.Amount ~ Gender, 
          data = mkt, 
          FUN = summary)
#Slightly higher among males.

##Claim amount and State
aggregate(formula = Total.Claim.Amount ~ State, 
          data = mkt, 
          FUN = summary)
#Similar in all states.

##Claim amount and employment status
aggregate(formula = Total.Claim.Amount ~ EmploymentStatus, 
          data = mkt, 
          FUN = summary)
#Unemployed people have noticeably higher claim amount.
#Retired people too
#Lowest claim is from employed people.

#Check CLV
aggregate(formula = Customer.Lifetime.Value ~ EmploymentStatus, 
          data = mkt, 
          FUN = summary)
#CLV mean/median is not significantly different.

#Check Premium amount
aggregate(formula = Monthly.Premium.Auto ~ EmploymentStatus, 
          data = mkt, 
          FUN = summary)
#Premium is about the same.(mean/median)

##Claim Amount and Marital Status
aggregate(formula = Total.Claim.Amount ~ Marital.Status, 
          data = mkt, 
          FUN = summary)
#Singles have high claim amounts than other groups.
#Check CLV
aggregate(formula = Customer.Lifetime.Value ~ Marital.Status, 
          data = mkt, 
          FUN = summary)
#Singles have lowest median and mean CLV.
#Check Premium
aggregate(formula = Monthly.Premium.Auto ~ Marital.Status, 
          data = mkt, 
          FUN = summary)
#Singles also don't pay more premium monthly. 

##Claim Amount and Number of Policies
aggregate(formula = Total.Claim.Amount ~ Number.of.Policies, 
          data = mkt, 
          FUN = summary)
#Having more number of policies does not increase claim amount.

##Claim Amount and Vehicle Class
aggregate(formula = Total.Claim.Amount ~ Vehicle.Class, 
          data = mkt, 
          FUN = summary)
#Luxury cars and luxury suvs significantly higher claim amount.
#Check CLV.
aggregate(formula = Customer.Lifetime.Value ~ Vehicle.Class, 
          data = mkt, 
          FUN = summary)
#Luxury cars and luxury SUVs also have high CLV. 
#Check Premium
aggregate(formula = Monthly.Premium.Auto ~ Vehicle.Class, 
          data = mkt, 
          FUN = summary)
#Luxury Cars and Luxury Suvs also pay a higher premium.


##Income and Response
aggregate(formula = Income ~ Response, 
          data = mkt, 
          FUN = summary)
#Median mean and income do not vary among people who responded
#and people who did not.

##Income and number of policies
aggregate(formula = Income ~ Number.of.Policies, 
          data = mkt, 
          FUN = summary)
#No specific pattern

##Data Visualization 
#----------------------------------

##Boxplots

### In relation to Total Claim Amount 
#Location Code
ggplot(data = mkt, 
       mapping = aes(x = Location.Code, y = Total.Claim.Amount,
                     col = "coral4",
                     )) +
  ggtitle("Location and Total Claim Amount")+
  geom_boxplot()
#Employment Status
ggplot(data = mkt, 
       mapping = aes(x = EmploymentStatus, y = Total.Claim.Amount, color = "darkolivegreen"))+
                     ggtitle("Employment Status and Total Claim Amount") +
  geom_boxplot()
#Marital Status
ggplot(data = mkt, 
       mapping = aes(x = Marital.Status, y = Total.Claim.Amount,
                     color = "cyan3")) +
  ggtitle("Marital Status and Total Claim Amount")+
  geom_boxplot()
#Vehicle Class
ggplot(data = mkt, 
       mapping = aes(x = Vehicle.Class, y = Total.Claim.Amount )) +
  geom_boxplot()

###In Relation to CLV
#Location Code
ggplot(data = mkt, 
       mapping = aes(x = Location.Code, y = Customer.Lifetime.Value,
                     color = "purple")) +
  ggtitle("Location Code and Customer Lifetime Value")+
  geom_boxplot()
#Marital Status
ggplot(data = mkt, 
       mapping = aes(x = Marital.Status, y = Customer.Lifetime.Value,
                     color = "accent")) +
  ggtitle("Marital Status and Customer Lifetime Value")+
  geom_boxplot()
#Vehicle Class
ggplot(data = mkt, 
       mapping = aes(x = Vehicle.Class, y = Customer.Lifetime.Value )) +
  geom_boxplot()


##Barplots
#Response and Gender
ggplot(data = mkt, mapping = aes(x = Response,
                                 fill = Gender)) +
  ggtitle("Response By Gender")+
  geom_bar()
#Respose and Location
ggplot(data = mkt, mapping = aes(x = Response,
                                 fill = Location.Code)) +
  ggtitle("Response By Location Code")+
  geom_bar()
#Response and Marital Status
ggplot(data = mkt, mapping = aes(x = Response,
                                 fill = Marital.Status)) +
  ggtitle("Response By Marital Status")+
  geom_bar()
#Response and Renew offer type
ggplot(data = mkt, mapping = aes(x = Response,
                                 fill = Renew.Offer.Type)) +
  ggtitle("Response By Renew Offer Type")+
  geom_bar()


##Histograms
#CLV
hist(x = mkt$Customer.Lifetime.Value, 
     main = "Customer Lifetime Value", 
     xlab = "", 
     col = "cyan4")
#Total Claim Amount
hist(x = mkt$Total.Claim.Amount, 
     main = "Total Claim Amount", 
     xlab = "", 
     col = "cyan4")
#Monthly Premium
hist(x = mkt$Monthly.Premium.Auto, 
     main = "Monthly Premium Amount", 
     xlab = "", 
     col = "darkgreen")
#Months Since Last Claim
hist(x = mkt$Months.Since.Last.Claim, 
     main = "Months Since Last Claim", 
     xlab = "", 
     col = "darkgreen")
#Customer Income
hist(x = mkt$Income, 
     main = "Customer Income", 
     xlab = "", 
     col = "cyan4")


#----------------------------------------------------
#########    Data Pre-processing    #########
#----------------------------------------------------

library(caret)

View(mkt)

# Discretization
# Using the cut() function to perform the 
# discretization, breaking the Income variable 
# into 7 categories

##Income variable has a lot of 0s, which makes the data
#heavily skewed. 

summary(object = mkt$Income)
mkt$Income_disc <- cut(x = mkt$Income, 
                   breaks = 7, 
                   labels = c(1,2,3,4,5,6,7),
                   ordered_result = TRUE)
head(mkt[ ,c("Income", "Income_disc")])


# We can visualize the Continuous and
# Discretized Variables side-by-side
par(mfrow = c(1,2))
hist(x = mkt$Income, 
     main = "Continuous Income", 
     xlab = "",
     col = "darkcyan")
plot(x = mkt$Income_disc, 
     main = "Discrete Income",
     xlab = "",
     col = "cyan3")
par(mfrow = c(1,1))


###Binarization
#----------------------------------------------------
# Using the class2ind() function from the caret package
#to create a single dummy variable and apply the function 
#to one variable

mkt$Response <- factor(x = mkt$Response)

mkt$Response_bin <- class2ind(x = mkt$Response, 
                          drop2nd = TRUE)
mkt$Response

##Yes =0, No = 1

mkt$Gender <- factor(x = mkt$Gender)

mkt$Gender_bin <- class2ind(x = mkt$Gender, 
                              drop2nd = TRUE)
#Female = 1, male = 0


# Creating categorical variables with more than
# 2 class levels by using dummyVars() 
# function from the caret package. 

##Marital Status
mkt$Marital.Status <- factor(x = mkt$Marital.Status)
mats <- dummyVars(formula = ~ Marital.Status,
                  data = mkt)

mats_dums <- predict(object = mats, 
                     newdata = mkt)

##Location Code
mkt$Location.Code <- factor( x = mkt$Location.Code)
mats1 <- dummyVars(formula = ~ Location.Code,
                   data = mkt)
mats1_dums <- predict(object = mats1, 
                     newdata = mkt)
##Employment Status
mats2 <- dummyVars(formula = ~EmploymentStatus,
                   data = mkt)
mats2_dums <- predict(object = mats2,
                      newdata = mkt)

##State
mats3 <- dummyVars(formula = ~State,
                   data = mkt)
mats3_dums <- predict(object = mats3,
                      newdata = mkt)

##Offer 
mats4 <- dummyVars(formula = ~Renew.Offer.Type,
                   data = mkt)
mats4_dums <- predict(object = mats4,
                      newdata = mkt)

##Sales Channel
mats5 <- dummyVars(formula = ~Sales.Channel,
                   data = mkt)
mats5_dums <- predict(object = mats5,
                      newdata = mkt)

##Vehicle Class
mats6 <- dummyVars(formula = ~Vehicle.Class,
                   data = mkt)
mats6_dums <- predict(object = mats6,
                      newdata = mkt)

# For ordinal variables, we need to convert 
# them to unordered first before binarization.

#Coverage
mkt$Coverage <- factor(mkt$Coverage, 
                       ordered = FALSE)
mats7 <- dummyVars(formula =  ~ Coverage,
                  data = mkt)
mats7_dums <- predict(object = mats7,
                      newdata = mkt)

#Education
mats8 <- dummyVars(formula = ~Education,
                   data = mkt)
mats8_dums <- predict(object = mats8,
                      newdata = mkt)


##Combining all dummy variables with our original dataset
ms_dum <- cbind(mkt, mats_dums, mats1_dums, mats2_dums,
                mats3_dums, mats4_dums, mats5_dums,
                mats6_dums, mats7_dums, mats8_dums)



# We will want to drop the factor 
# variables from the dataframe

View(ms_dum)

##Dropping unnecessary variables and factor variables
ms_dum <- ms_dum[ ,-c(1,3:8,12:15,17,18,19)] 
ms_dum <- ms_dum[ , -c(2,5)]

## Dropping irrelevant factors
#ms_dum <- ms_dum[ ,-c(1, 6)]

##Dropped Customer ID and Date. 



###Variable transformation
#----------------------------------------------------------
# Transformations will only be applied to
# numeric variables.

## Combining center scale and yeo-johnson 

cen_yjsc <- preProcess(x = mkt,
                       method = c("YeoJohnson", "center", "scale"))
ms_yjsc <- predict(object = cen_yjsc,
                   newdata = mkt)

par(mfrow = c(1,2))
# Before
hist(mkt$Total.Claim.Amount, 
     main = "Total Claim Amount",
     xlab = "",
     col = "coral4")
# After
hist(ms_yjsc$Total.Claim.Amount,
     main="YJSC Transform Total Claim Amount",
     xlab = "",
     col = 'coral2')

# Before
hist(mkt$Customer.Lifetime.Value, 
     main = "Customer Lifetime Value",
     xlab = "",
     col = "darkolivegreen")
# After
hist(ms_yjsc$Customer.Lifetime.Value,
     main="YJSC Transform Customer Lifetime Value",
     xlab = "",
     col = 'darkkhaki')



#----------------------------------------------------
#########    Data Quality     #########
#-------------------------------------------------------


## Duplicate Values
# We use the duplicated() function to identify 
# duplicate observations 

mkt[duplicated(x = mkt), ]
#No duplicate values

###Checking for missing Values using complete cases

mkt[!complete.cases(mkt), ]
na_rows <- rownames(mkt)[!complete.cases(mkt)]
na_rows

#There are no missing values in the dataset.

#We will check to see if there the 0 values are 
#meaningful, and not missing values.

mkt[which(mkt == 0, arr.ind = TRUE), ]

#0 values are either binary values, or income of the
#unemployed. Both make absolute sense. No missing Value


## Checking for Outliers

# Identifying Outliers by Z-Score Method
# wWe use the abs() function 
# to identify observations in which the
# absolute value of the Z-Score is greater 
# than 3.

outs <- sapply(ms_yjsc[ ,nums], function(x) which(abs(x) > 3))
outs
#Outliers only in total claim amount. Only 46 observations. 

#Viewing rows with outliers
ms_yjsc[unique(unlist(outs)),]

##Removing Outliers from original data frame and standardized 
#datagrame
ms_yjsc <- ms_yjsc[-(unique(unlist(outs))),]
mkt <- mkt[-(unique(unlist(outs))),]

#-----------------------------------------------------------
###End of Data Preprocess########
#-----------------------------------------------------------

#-----------------------------------------------------------
#######   Cluster Analysis    #########
#-----------------------------------------------------------

##Checking first for Variable Association

# Correlation
# Correlation matrix of numeric variables
round(cor(x = mkt[ ,nums]), digits = 2)

##Significant correlations: 
#CLV and monthly premium auto .39
#CLV and TCA .22
#Income negatively with TCA -.36
#Monthly premium with TCA .60



# Scatterplot
#CLV and Premium, CLV and TCA, MPA and TCA

plot(formula = Customer.Lifetime.Value ~ Monthly.Premium.Auto, # Y ~ X
     data = mkt, # dataframe name
     xlab = "CLV", # x-axis label
     ylab = "Monthly premium auto", # y-axis label
     pch = 16,
     col = "darkseagreen") # plot point
abline(lm(mkt$Customer.Lifetime.Value ~ mkt$Monthly.Premium.Auto))
#Not a very strong relation

plot(formula = Total.Claim.Amount ~ Monthly.Premium.Auto, # Y ~ X
     data = mkt, # dataframe name
     xlab = "TCA", # x-axis label
     ylab = "Monthly premium auto", # y-axis label
     pch = 16,
     col = "darksalmon") # interesting pattern?
abline(lm(mkt$Total.Claim.Amount ~ mkt$Monthly.Premium.Auto))
###Choosing to not treat TCA and Monthly premium auto for
#this analysis

#install.packages(c("cluster","factoextra", "fpc", "NbClust"))
## Load libraries
library(NbClust)
library(cluster) # clustering
library(factoextra) # cluster validation, plots
library(fpc) # cluster validation

?NbClust

#-----------------------------------------------------------
##Pre-process
#########vars <- c(facs, ords, nums)

## Distance
# Using Gower distance- daisy() function
# from the cluster package as our data is mixed.
# Using the normalized dataset ms_yjsc


# Gower distance
hdist <- daisy(x = ms_yjsc, 
               metric = "gower")
summary(hdist)


# Apply Ward's Clustering
wards <- hclust(d = hdist, 
                method = "ward.D2")

#Cophenetic correlation
cor(x = hdist, y = cophenetic(x = wards))

## Within-cluster sum of squares for validation
wss_plot(dist_mat = hdist, # distance matrix
         method = "hc", # HCA
         hc.type = "ward.D2", # linkage method
         max.k = 15)

# We can see an elbow at 4.

#Using Silhouette Method for confirmation
sil_plot(dist_mat = hdist, 
         method = "hc", 
         hc.type = "ward.D2", 
         max.k = 15)

# k = 4 is the optimal solution.

##We will move ahead with 4 clusters for our analysis.
# Plotting the dendrogram
par(mfrow = c(1,1))
plot(wards, 
     xlab = NA, sub = NA, 
     main = "Wards Method")

#We can also spot 4 distinct clusters in the dendogram.
# Overlaying boxes identifying clusters
# for a k = 4 clustering solution
rect.hclust(tree = wards, 
            k = 4, 
            border = hcl.colors(4))

# Creating a vector of cluster assignments
wards_clusters <- cutree(tree = wards, 
                         k = 4)



## Describing Cluster Solutions

# Obtaining average variable values for each cluster
# for (original) numeric variables
aggregate(x = mkt[ ,nums], 
          by = list(wards_clusters),
          FUN = mean)
#Cluster 1 - Highest CLV, Highest income, lower premium,
#lowest claim amount
#Cluster 2 - Lowest income and clv, highest claim amount
#Cluster 3- Higher clv and income, lower claim
#Cluster4 - All lower/ average

# Obtain frequency values for each class level
# for categorical variables
aggregate(x = mkt[ ,c(ords,facs)], 
          by = list(wards_clusters), 
          FUN = table)
#Looking for patterns
##Cluster 1 - Highest number below HS, most married,
#Highest in California, most employed, most female(-4), all no
##Cluster 2 - lowest employed, highest unemployed, very low
#in urban, most single, half half men women, all no
##cluster 3 - High employed, lowest unemployed,most men, high 
#married, almost all no
##cluster 4 - Lowest below HS/ low in all edu, lower employed
#most retired, almost all yes

#External Validation
cluster.stats(d = hdist, # distance matrix
              clustering = wards_clusters, # cluster assignments
              alt.clustering = as.numeric(mkt$Response))$corrected.rand 

##Our rand index shows poor recovery. While this analysis,
#may be a good basis for further analysis, we should not
#base our decisions on this cluster analysis alone.




#------------------------------------------
######### Naive Bayes #########
#------------------------------------------
#install.packages("neuralnet")
library(NeuralNetTools)
#Setting up target variable
mkt$Response <- factor(mkt$Response)

# Standardization for Naive Bayes

cen_yjsc <- preProcess(x = mkt,
                       method = c("YeoJohnson", "center", "scale"))



mkt_bcs <- predict(object = cen_yjsc,
                   newdata = mkt)

#------------------------------------------

## Training and Testing

# Splitting the data into training and 
# testing sets using an 85/15 split rule

# Initialize random seed
set.seed(392552227)

# Create list of training indices
sub_nb <- createDataPartition(y = mkt_bcs$Response, # target variable
                              p = 0.85, # % in training
                              list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train_nb <- mkt_bcs[sub_nb, ] # create train dataframe
test_nb <- mkt_bcs[-sub_nb, ] # create test dataframe


#Sampling

train_nbs <- upSample(x = train_nb[ ,vars], # predictors
                      y = train_nb$Response, # target
                      yname = "Response") # name of y variable

par(mfrow = c(1,2))
plot(train$Response, main = "Original")
plot(train_nbs$Response, main = "RUS")
# par(mfrow = c(1,1))

#------------------------------------------

## Analysis

# We use the naiveBayes() function from the
# e1071 package to perform NB classification

# If we have NA values that we did not
# handle during preprocessing, we can
# use the na.action argument, which defaults
# to na.pass, which means NAs will not
# be included when calculating probabilities.
# Alternatively, na.action = na.omit can
# be specified and NAs will not be included
# in modeling.

# The default laplace value is 0, meaning
# laplace smoothing is not applied. To 
# determine if we need to use Laplace
# smoothing, we need to look for zero
# probability categories
aggregate(train_nbs[ ,c(facs,ords)],
          by = list(train_nbs$Response),
          FUN = table)

# Since we do not have a zero probability, 
# no smoothing is applied
nb_mod <- naiveBayes(x = train_nbs[ ,vars],
                     y = train_nbs$Response)
# laplace = 1) no zero probability
nb_mod

#------------------------------------------

### Model Performance & Fit

## Training Performance

# To assess the goodness of fit of the
# model, we compare the training and
# testing performance.

# First, we use the predict() function
# to obtain the class predictions
# (type = "class") for the training
# data based on our NB model. 

nb.train <- predict(object = nb_mod, # NB model
                    newdata = train_nbs[ ,vars], # predictors
                    type = "class")


# This creates a vector of class predictions
# for each of our training observations.
head(nb.train)

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train). We can set 
# mode = "everything" to obtain all available
# performance measures. We identify
# the "Responseed" class as positive, since
# that is the class we are more interested
# in being able to predict. We will save it 
# so that we can make comparisons
tr_nb_conf <- confusionMatrix(data = nb.train, # predictions
                              reference = train_nbs$Response, # actual
                              positive = "Yes",
                              mode = "everything")
tr_nb_conf


## Testing Performance

# To assess model performance, we focus on
# the performance of the model applied to 
# the testing set.

# We use the predict() function to obtain
# the class predictions (type = "class") 
# for the testing data based on our NB model. 
nb.test <- predict(object = nb_mod, # NB model
                   newdata = test_nb[ ,vars], # predictors
                   type = "class")

# Again, we use the confusionMatrix() 
# function from the caret package
te_nb_conf <- confusionMatrix(data = nb.test, # test predictions
                              reference = test_nb$Response, # actual
                              positive = "Yes",
                              mode = "everything")
te_nb_conf

# Overall Performance
# We can describe the overall performance 
# based on our accuracy and kappa values.
te_nb_conf$overall[c("Accuracy", "Kappa")]

# Class-Level Performance
# We can describe class-level performance
# for the class level of interest, Response = "Yes"
te_nb_conf$byClass


## Goodness of Fit
# We assess if the model is balanced,
# underfitting or overfitting

# Overall
cbind(Training = tr_nb_conf$overall,
      Testing = te_nb_conf$overall)

# Class-Level
cbind(Training = tr_nb_conf$byClass,
      Testing = te_nb_conf$byClass)


#------------------------------------------
####### Artificial Neural Networks ########
#------------------------------------------

#------------------------------------------

## Training & Testing

# We use the createDataPartition() function
# from the caret package to create our
# training and testing sets using an
# 85/15 split.

set.seed(392552227) # initialize the random seed

# Generate the list of observations for the
# train dataframe
sub <- createDataPartition(y = ms_dum$Response, 
                           p = 0.85,
                           list = FALSE)

# Create our train and test sets
train<- ms_dum[sub, ] 
test <- ms_dum[-sub, ]

#------------------------------------------

## Analysis

# Since there is no true default model, we 
# go straight to hyperparameter tuning to
# find the optimal number of hidden nodes
# and weight decay.


### Hyperparameter Tuning Model 
### train() in caret package

# We can use the train() function from the 
# caret package to tune our hyperparameters. 
# Here, we will use the nnet package 
# (method = "nnet"). We can tune the size 
# and decay hyperparameters.

## Size: number of nodes in the hidden layer. 
#  (Note: There can only be one hidden layer 
#  using nnet)
## Decay: weight decay. to avoid overfitting, 
#  adds a penalty for complexity (error + wd * SSW).
#  Values typically range between 0.01 - 0.1.
# Note: nnet() does not use gradient descent,
# so there is no learning rate.

# We will use a grid search and 5-fold cross
# validation.

# First, we set up the grid using the 
# expand.grid() function for the size and
# decay hyperparameters


grids <-  expand.grid(size = seq(from = 3, # min node value
                                 to = 19, # max node value
                                 by = 4), # counting by
                      decay = seq(from = 0, # min wd value
                                  to = 0.1, # max wd value
                                  by = 0.01)) # counting by

grids

# Next, we set up our control object for
# input in the train() function for the
# trControl argument
ctrl <- trainControl(method = "cv",
                     number = 5, # 5 folds
                     search = "grid") # grid search


# Downsampling due to class imbalance

ctrl$sampling <- "down"


ctrl

# Then, we use the train() function to
# train the ANN model using 5-Fold Cross 
# Validation to search over the 
# hyperparameter grid (grids).
# We use the preProcess argument for
# range (min-max) normalization.
set.seed(392552227)

annMod <- train(form = Response ~., # use all other variables to predict Response
                data = train[ , c(vars, "Response")], # training data
                preProcess = c("center", "scale"), # standardize
                method = "nnet", # use nnet()
                trControl = ctrl, 
                maxit = 200, # increase # of iterations from default (100)
                tuneGrid = grids, # search over the created grid
                trace = FALSE) # suppress output

# We can view the Accuracy and Kappa
# across our hyperparameter grid and
# obtain the optimal values of size
# and decay
annMod

# We can visualize the tuned ANN model
# using the plotnet() function from the
# NeuralNetTools package. 
plotnet(mod_in = annMod$finalModel, # nnet object
        pos_col = "darkgreen", # positive weights are shown in green
        neg_col = "darkred", # negative weights are shown in red
        circle_cex = 4, # reduce circle size (default is 5)
        cex_val = .4) # reduce text label size (default is 1)


## Training Performance
# We use the predict() function to obtain
# class predictions for the Purchase
# variable using the ANN model.
tune.tr.preds <- predict(object = annMod, # tuned model
                         newdata = train) # training data

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train).
tune_tr_conf <- confusionMatrix(data = tune.tr.preds, # predictions
                                reference = train$Response, # actual
                                positive = "Yes",
                                mode = "everything")
tune_tr_conf
## Testing Performance
# We use the predict() function to generate 
# class predictions for our testing data set
# and evaluate model performance.
tune.te.preds <- predict(object = annMod, # tuned model
                         newdata = test) # testing data

# Next, we get performance measures using
# the confusionMatrix() function
tune_te_conf <- confusionMatrix(data = tune.te.preds, # predictions
                                reference = test$Response, # actual
                                positive = "Yes",
                                mode = "everything")
tune_te_conf

## Goodness of Fit
# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = tune_tr_conf$overall,
      Testing = tune_te_conf$overall)

# Class-Level
cbind(Training = tune_tr_conf$byClass,
      Testing = tune_te_conf$byClass)

#-----------------------------------------------------------
#######     Decision Tree       ##############
#-----------------------------------------------------------

library(rpart)
library(rpart.plot)

#-----------------------------------------------------------------
## Prepare Target (Y) Variable
mkt$Response <- factor(mkt$Response)

# We can use the plot() function
# to create a barplot
plot(mkt$Response,
     main = "Response")

#Combining the variables into 1 vector
vars2 <- c(facs, ords, nums)

# We can obtain summary informaton for
# our prepared data
summary(mkt[,c(vars2, "Response")])


## Preprocessing & Transformation

## Training and Testing

# Splitting the data into training and 
# testing sets using an 85/15 split rule


# Initialize random seed
set.seed(392552227) 

# Create list of training indices
sub <- createDataPartition(y = mkt$Response, # target variable
                           p = 0.85, # % in training
                           list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train <- mkt[sub, ] # create train dataframe
test <- mkt[-sub, ] # create test dataframe



## Class Imbalance 

# First, we can evaluate if we have class
# imbalance in our target variable
summary(train$Response)

# We can use the plot() function
# to create a bar plot to view the
# distribution
plot(train$Response,
     main = "Response")

### Default, Base Model (with class imbalance)
# We will train a Decision Tree model
ctrl_DT <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 3)

set.seed(392552227) # initialize random seed

DTFit_base <- train(x = train[,vars2],
                    y = train$Response,
                    method = "rpart", # use the rpart package
                    trControl = ctrl_DT, # control object
                    tuneLength = 5) # try 5 cp values
# We can view the results of our tuned
# model with class imbalance
DTFit_base

### Resampling Methods to correct class imbalance

## Oversampling
#(We are oversampling for decision tree as we do not want
# potentially compromise on the number of nodes in our analysis.
#As our data set has a low number of the target variable, we 
#will oversample to treat class imbalance.)

set.seed(392552227) 
train_os <- upSample(x = train[ ,vars2], # predictors
                     y = train$Response, # target
                     yname = "response")



DTFit_ROS <- train(x = train_os[ ,vars2],
                   y = train_os$response,
                   method = "rpart", # use the rpart package
                   trControl = ctrl_DT, # control object
                   tuneLength = 5) # try 5 cp values

#Viewing the results of our tuned
# model using random oversampling
DTFit_ROS

## Analysis

# 1. Basic Model 
# We used the rpart() function in the rpart 
# package to perform basic DT classification 
# on our training dataset.


MR.rpart <- rpart(formula = response ~ ., # Y ~ all other variables in dataframe
                  data = train_os[ ,c(vars2, "response")], # include only relevant variables
                  method = "class")

# We can see the basic output of our
# Decision Tree model
MR.rpart

# We can use the variable.importance
# component of the rpart object to 
# obtain variable importance
MR.rpart$variable.importance

## Tree Plots
# We can use either the prp() function 
# or the rpart.plot() function in the 
# rpart.plot package to plot our 
# rpart object (FD.rpart).

prp(x = MR.rpart, # rpart object
    extra = 2) # include proportion of correct predictions

## Training Performance
# We use the predict() function to generate 
# class predictions for our training set
base.trpreds <- predict(object = MR.rpart, # DT model
                        newdata = train_os, # training data
                        type = "class") # class predictions

# We use the confusionMatrix() function
#  to obtain a confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train).
DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = train_os$response, # actual
                                 positive = "Yes",
                                 mode = "everything")
DT_train_conf


## Testing Performance
# We use the predict() function to generate 
# class predictions for our testing set
base.tepreds <- predict(object = MR.rpart, # DT model
                        newdata = test, # testing data
                        type = "class")

# Confusion matrix for test data set

DT_test_conf <- confusionMatrix(data = base.tepreds, # predictions
                                reference = test$Response, # actual
                                positive = "Yes",
                                mode = "everything")
DT_test_conf


## Goodness of Fit

# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)

# Class-Level
cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

#------------------------------------------

### 2. Hyperparameter Tuning Model

# We will perform a grid search for the 
# optimal cp value.

# We will search over a grid of values
# from 0 to 0.05. We use the expand.grid()
# function to define the search space
grids <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grids

# First, we set up a trainControl object
# (named ctrl) using the trainControl() 
# function in the caret package. We specify 
# that we want to perform 10-fold cross 
# validation, repeated 3 times and specify
# search = "grid" for a grid search. We use 
# this object as input to the trControl 
# argument in the train() function below.
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     search = "grid")
ctrl

# Next, we initialize a random seed for 
# our cross validation
set.seed(392552227)

# Then, we use the train() function to
# train the DT model using 5-Fold Cross 
# Validation (repeated 3 times). We set
# tuneGrid equal to our grid search
# objects, grids.
DTFit <- train(form = response ~ ., # use all variables in data to predict delay
               data = train_os[ ,c(vars2, "response")], # include only relevant variables
               method = "rpart", # use the rpart package
               trControl = ctrl, # control object
               tuneGrid = grids) # custom grid object for search

# We can view the results of our
# cross validation across cp values
# for Accuracy and Kappa. The output
# will also identify the optimal cp.
DTFit

DTFit$results[DTFit$results$cp %in% DTFit$bestTune,] #best result


# We can plot the cp value vs. Accuracy
plot(DTFit)

# We can view the confusion matrix showing
# the average performance of the model
# across resamples
confusionMatrix(DTFit)

# Decision Trees give us information 
# about Variable Importance. We can use
# the best fit object from the caret
# package to obtain variable importance
# information 
DTFit$finalModel$variable.importance


## Tuned Model Performance

### Training Performance
# We use the predict() function to generate 
# class predictions for our training data set
tune.trpreds <- predict(object = DTFit,
                        newdata = train_os,
                        type = "raw")

# We use the confusionMatrix() function
# from the caret package
DT_trtune_conf <- confusionMatrix(data = tune.trpreds, # predictions
                                  reference = train_os$response, # actual
                                  positive = "Yes",
                                  mode = "everything")

DT_trtune_conf


## Testing Performance
# We use the predict() function to generate class 
# predictions for our testing data set
tune.tepreds <- predict(object = DTFit,
                        newdata = test)

# We use the confusionMatrix() function
# from the caret package
DT_tetune_conf <- confusionMatrix(data = tune.tepreds, # predictions
                                  reference = test$Response, # actual
                                  positive = "Yes",
                                  mode = "everything")
DT_tetune_conf


## Goodness of Fit
# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = DT_trtune_conf$overall,
      Testing = DT_tetune_conf$overall)

# Class-Level
cbind(Training = DT_trtune_conf$byClass,
      Testing = DT_tetune_conf$byClass)
