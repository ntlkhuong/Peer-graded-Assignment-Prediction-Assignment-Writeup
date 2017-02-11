# Assignment: Prediction Assignment Writeup
khuongtln  
2/5/2017  

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. 
These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## Data loading

Loading the data for explore

```r
training <- read.csv("pml-training.csv",na.strings = c("NA", "#DIV/0!"))
testing <-read.csv("pml-testing.csv",na.strings = c("NA", "#DIV/0!"))
```
## Some necessary libraries


```r
library(Hmisc)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.3.2
```

```r
library(randomForest)
library(foreach)
library(doParallel)
library(parallel)
```

## Data explore

Exam the loaded data

```r
dim(training)
```

```
## [1] 19622   160
```

```r
table(training$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r
dim(testing)
```

```
## [1]  20 160
```

For trainning data, Need to clean some NA values and remove some variable not related to training$classe

```r
NA_count = sapply(1:dim(training)[2],function(x)sum(is.na(training[,x])))
NA_list = which(NA_count>0)
colnames(training[,c(1:7)])
```

```
## [1] "X"                    "user_name"            "raw_timestamp_part_1"
## [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
## [7] "num_window"
```


```r
training = training[,-NA_list]
training = training[,-c(1:7)]
training$classe = factor(training$classe)
```

The same for testing data, need to clean NA values

```r
testing = testing[,-NA_list]
testing = testing[,-c(1:7)]
dim(testing)
```

```
## [1] 20 53
```

## Cross Validation
will try to use different classification methods in caret package, classification tree algorithm and random force. 3-fold validation using trainControl function also will be used.


```r
set.seed(1024)
cv3 = trainControl(method="cv",number=3,verboseIter = TRUE,allowParallel = TRUE)
modrf=train(classe ~.,data = training,method ="rf", trControl=cv3)
```

```
## + Fold1: mtry= 2 
## - Fold1: mtry= 2 
## + Fold1: mtry=27 
## - Fold1: mtry=27 
## + Fold1: mtry=52 
## - Fold1: mtry=52 
## + Fold2: mtry= 2 
## - Fold2: mtry= 2 
## + Fold2: mtry=27 
## - Fold2: mtry=27 
## + Fold2: mtry=52 
## - Fold2: mtry=52 
## + Fold3: mtry= 2 
## - Fold3: mtry= 2 
## + Fold3: mtry=27 
## - Fold3: mtry=27 
## + Fold3: mtry=52 
## - Fold3: mtry=52 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 27 on full training set
```

```r
modtree = train(classe~.,data=training,method="rpart",trControl=cv3)
```

```
## Loading required package: rpart
```

```
## + Fold1: cp=0.03568 
## - Fold1: cp=0.03568 
## + Fold2: cp=0.03568 
## - Fold2: cp=0.03568 
## + Fold3: cp=0.03568 
## - Fold3: cp=0.03568 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0357 on full training set
```

The performance of these two model on the testing data

```r
prf=predict(modrf,training)
ptree=predict(modtree,training)
table(prf,training$classe)
```

```
##    
## prf    A    B    C    D    E
##   A 5580    0    0    0    0
##   B    0 3797    0    0    0
##   C    0    0 3422    0    0
##   D    0    0    0 3216    0
##   E    0    0    0    0 3607
```

```r
table(ptree,training$classe)
```

```
##      
## ptree    A    B    C    D    E
##     A 5080 1581 1587 1449  524
##     B   81 1286  108  568  486
##     C  405  930 1727 1199  966
##     D    0    0    0    0    0
##     E   14    0    0    0 1631
```

For the testing data

```r
prf=predict(modrf,testing)
ptree=predict(modtree,testing)
table(prf,ptree)
```

```
##    ptree
## prf A B C D E
##   A 7 0 0 0 0
##   B 3 0 5 0 0
##   C 0 0 1 0 0
##   D 0 0 1 0 0
##   E 1 0 2 0 0
```

Comparing the result, it seems that random forest model has better accuracy for the testing dataset.

## Conclusion
Random forest model was chosen for the testing dataset.

```r
answers=predict(modrf,testing)
pml_write_files = function(x){
n = length(x)
for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
answers
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pml_write_files(answers)
```

