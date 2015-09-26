##Set up the workspace
rm(list=ls())
packages <- c("data.table","reshape2","dplyr")
sapply(packages,require,character.only=TRUE,quietly=TRUE)
#setwd("~/Dropbox/Coursera/GettingAndCleaningData/Project")
setwd("~/GitHub/GettingAndCleaningData/Project")
##Download and extract the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(),"20Dataset.zip")
download.file(url,f)
unzip(f)

dataFolder <- "./UCI HAR Dataset"
list.files(dataFolder,recursive=TRUE)
##Read in subject files
DT_subject_train <- data.table(fread(file.path(dataFolder,"train","subject_train.txt")))
DT_subject_test <- data.table(fread(file.path(dataFolder,"test","subject_test.txt")))
dim(DT_subject_train)
dim(DT_subject_test)
##Read the activity (X-movement) files
DT_X_train <- data.table(read.table(file.path(dataFolder,"train","X_train.txt")))
DT_X_test <- data.table(read.table(file.path(dataFolder,"test","X_test.txt")))
dim(DT_X_train)
dim(DT_X_test)
##Read the activity (Y-movement) files
DT_Y_train <- data.table(fread(file.path(dataFolder,"train","Y_train.txt")))
DT_Y_test <- data.table(fread(file.path(dataFolder,"test","Y_test.txt")))
dim(DT_Y_train)
dim(DT_Y_test)

##Concatenate the data tables for subject
DT_subject <- rbind(DT_subject_train,DT_subject_test)
setnames(DT_subject,"V1","subject")
rm(DT_subject_train,DT_subject_test)
##Concatenate the data tables for X-movement activity
DT_X <- rbind(DT_X_train,DT_X_test)
rm(DT_X_train,DT_X_test)
##Concatenate the data tables for Y-movement activity
DT_Y <- rbind(DT_Y_train,DT_Y_test)
setnames(DT_Y,"V1","activityNum")
rm(DT_Y_train,DT_Y_test)
tables()
##Merge columns to form single data table
DT_subject <- cbind(DT_subject,DT_Y)
rm(DT_Y);tables()
DT_X <- cbind(DT_subject,DT_X)
rm(DT_subject)
tables()
##Sort data table by subject and activityNum
setkey(DT_X,subject,activityNum)
tables()


##Read the features file
DT_features <- data.table(fread(file.path(dataFolder,"features.txt")))
setnames(DT_features,names(DT_features),c("featureNum","featureName"))
tables()

##Subset only measurements for the mean and standard deviation
DT_features <- DT_features[grepl("mean\\(\\)|std\\(\\)",featureName)]
tables()

##Convert the column numbers to a vector of variable names
DT_features$featureCode <- DT_features[,paste0("V",featureNum)]
tables()
head(DT_features)


##Subset the variables using variablenames
select <- c(key(DT_X),DT_features$featureCode)
DT_X <- DT_X[,select,with=FALSE]
tables()


##Read activity_labels.txt file
DT_activity_labels <- data.table(fread(file.path(dataFolder,"activity_labels.txt")))
##Use activity lables to add descriptive variable names to the activities
setnames(DT_activity_labels,names(DT_activity_labels),c("activityNum","activityName"))
tables()
##Merge activity labels by activity number
DT_X <- merge(DT_X,DT_activity_labels,by="activityNum",all.x=TRUE)
rm(DT_activity_labels)
##Add activityName as a key
setkey(DT_X,subject,activityNum,activityName)
tables()
##Reshape data table from a short and wide format to a tall and narrow
DT_X <- data.table(melt(DT_X,key(DT_X),variable.name="featureCode"))
tables()

##Merge activity names
setkey(DT_X,featureCode);setkey(DT_features,featureCode)
DT_X <- merge(DT_X,
DT_features[,list(featureNum,featureCode,featureName)],
all.x=TRUE)
rm(DT_features)
tables()
head(DT_X,n=3)
tail(DT_X,n=3)
##Create factor variable, activity that is equivalent to activityName
DT_X$activity <- factor(DT_X$activityName)
##Create factor variable, feature that is equivalent to featureName
DT_X$feature <- factor(DT_X$featureName)

##From the dataset in step 4, create independent tidy dataset with the
##average of each variable for each activity and each subject

##SeperatefeaturesfromfeatureName
grepthis <- function(regex)grepl(regex,DT_X$feature)
##Featureswith1category
DT_X$featJerk <- factor(grepthis("Jerk"),labels=c(NA,"Jerk"))
DT_X$featMagnitude <- factor(grepthis("Mag"),labels=c(NA,"Magnitude"))
##Featureswith2categories
n <- 2
y <- matrix(seq(1,n),nrow=n)
x <- matrix(c(grepthis("^t"),grepthis("^f")),ncol=nrow(y))
DT_X$featDomain <- factor(x%*%y,labels=c("Time","Freq"))
x <- matrix(c(grepthis("Acc"),grepthis("Gyro")),ncol=nrow(y))
DT_X$featInstrument <- factor(x%*%y,labels=c("Accelerometer","Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"),grepthis("GravityAcc")),ncol=nrow(y))
DT_X$featAcceleration <- factor(x%*%y,labels=c(NA,"Body","Gravity"))
x <- matrix(c(grepthis("mean()"),grepthis("std()")),ncol=nrow(y))
DT_X$featVariable <- factor(x%*%y,labels=c("Mean","SD"))
##Featureswith3categories
n <- 3
y <- matrix(seq(1,n),nrow=n)
x <- matrix(c(grepthis("-X"),grepthis("-Y"),grepthis("-Z")),ncol=nrow(y))
DT_X$featAxis <- factor(x%*%y,labels=c(NA,"X","Y","Z"))
setkey(DT_X,subject,activity,featDomain,featAcceleration,featInstrument,featJerk,featMagnitude,featVariable,featAxis)
DT <- DT_X[,list(count=.N,average=mean(value)),by=key(DT_X)]
rm(x,y,DT_X)
## Write tidy dataset to csv file
f <- file.path(getwd(),"tidyDataset.txt")
write.table(DT,f,row.name=FALSE)


