Coursera Practical Machine Learning Project: write-up
========================================================

# Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

# Getting and cleanning data


```r
library(corrplot)
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
##Load and clean data
training <- read.csv("pml-training.csv", header = TRUE, sep = ",", na.strings = c("NA", ""))
training <- training[, which(as.numeric(colSums(is.na(training)))==0)]
```

# Feature Selection
First, remove variables that are highly correlated.

```r
## Remove variables that are highly correlated
corClass <- cor(x = training[sapply(training, is.numeric)])
highlyCor <- findCorrelation(corClass, 0.7)
#Apply correlation filter at 0.70,
#then we remove all the variable correlated with more 0.7.
training <- training[,-highlyCor]
```

Second, select the variable that has constant value.

```r
## remove nearZeroVars
removeColumns <- nearZeroVar(training)
training <- training[, -removeColumns]
## remove some information for users
training <- training[,c(5:36)]
```

Third, select the variable that is important using **varImp**. In this step, I build a prediction model using tree.

```r
##Split the data set
inTrain <- createDataPartition(y=training$classe, p=0.7, list = FALSE)
trainTR <- training[inTrain,]
testTR <- training[-inTrain,]


##First model using tree
modelFit1 <- train(classe ~., data = trainTR, method = "rpart")
```

After building the first model using tree, the variables with importance larger than 0 are selected.

```r
## select the important var
impVar <- varImp(modelFit1, surrogates = FALSE, competes = TRUE)

##choose the var the has importance larger than 0
impVar
```

```
## rpart variable importance
## 
##   only 20 most important variables shown (out of 31)
## 
##                   Overall
## accel_belt_z        100.0
## magnet_belt_y        93.8
## total_accel_belt     83.0
## yaw_belt             74.9
## magnet_dumbbell_y    71.9
## pitch_forearm        61.0
## magnet_dumbbell_z    57.0
## magnet_belt_z        26.8
## roll_forearm         24.3
## roll_dumbbell        22.0
## magnet_dumbbell_x    21.1
## roll_arm             15.8
## pitch_belt           15.6
## accel_forearm_x      13.5
## magnet_arm_y         12.4
## magnet_arm_x         11.9
## gyros_forearm_z       0.0
## accel_arm_x           0.0
## gyros_forearm_x       0.0
## magnet_forearm_y      0.0
```

```r
training <- training[, c("accel_belt_z", "magnet_belt_y", "total_accel_belt", "magnet_dumbbell_y", "pitch_forearm", "magnet_dumbbell_z", "roll_forearm", "yaw_belt","magnet_arm_x", "roll_dumbbell","magnet_belt_z", "roll_arm","accel_forearm_x","magnet_arm_y","classe")]
```

# Build the Model Using Random Forest
## Split data set for random forest
Use 75% for training set, 25% for testing set.

```r
inTrain <- createDataPartition(y=training$classe, p=0.75, list = FALSE)
trainRF <- training[inTrain,]
testRF <- training[-inTrain,]
```

## Training use random forest

```r
##Second model using random forest
modelFit2 <- train(classe ~., data = trainRF, method = "rf")
```

## Cross validation result on testing (out of sample) set

```r
confusionMatrix(testRF$classe,predict(modelFit2,testRF))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1393    2    0    0    0
##          B    1  944    4    0    0
##          C    0    5  846    4    0
##          D    3    1    6  791    3
##          E    0    4    3    4  890
## 
## Overall Statistics
##                                         
##                Accuracy : 0.992         
##                  95% CI : (0.989, 0.994)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.99          
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.997    0.987    0.985    0.990    0.997
## Specificity             0.999    0.999    0.998    0.997    0.997
## Pos Pred Value          0.999    0.995    0.989    0.984    0.988
## Neg Pred Value          0.999    0.997    0.997    0.998    0.999
## Prevalence              0.285    0.195    0.175    0.163    0.182
## Detection Rate          0.284    0.192    0.173    0.161    0.181
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.993    0.991    0.993    0.997
```

In conclussion, random forest is applied to predict the manner in which they did the exercise. The data are split as 75% for data set, and 25% for training set. The model has **98.78%** accuracy on testing set. In addtion, the model has **100%** accuracy on the submission testing set of the project.

