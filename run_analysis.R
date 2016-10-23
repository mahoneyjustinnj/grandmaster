## Getting and Cleaning Data: Final Project  161021
## Final Project
## Getting and Cleaning Data
## Make sure to change directory in 'R' to where the downloaded files
## are located.

## Part 1
## 1st - loading the packages 'data.table' and 'dplyr'

library(data.table)
library(dplyr)

## read supporting metadata (name of features and name of activities)

NamingFeatures <- read.table("UCI HAR Dataset/features.txt")
NamingActivities <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

## read 'training data'

TrainDataSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
TrainDataActivity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
TrainDataFeatures <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## read 'test data'

TestDataSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
TestDataActivity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
TestDataFeatures <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

## Merge 'training' and 'test' data (Question 1)

subject <- rbind(TrainDataSubject, TestDataSubject)
activity <- rbind(TrainDataActivity, TestDataActivity)
features <- rbind(TrainDataFeatures, TestDataFeatures)

## naming the columns in the 'features' data set 

names(NamingFeatures)

colnames(features) <- t(NamingFeatures[2])

## merge the data in 'features', 'activity' and 'subject' in the data file 
## 'completeDataSet' (Question 4)

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeDataSet <- cbind(features,activity,subject)


## Part 2
## Extract only measurements of mean and standard deviation
## for each measurement...

## 1st - extract column indices that have either mean or 
## standard deviation in them 

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeDataSet), ignore.case=TRUE)


## add "Activity" and "Subject" columns to the list 
## & look at Dimensions

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeDataSet)

## create 'extractedData' with the selected columns in 
## the 'requiredColumns' then look at dimensions of 'requiredColumns'

extractedData <- completeDataSet[,requiredColumns]
dim(extractedData)

## Part 3: Use descriptive activity names to name the 
## activities in the data set... (Question 3)
## the activity field in extractedData is originally 'numeric'
## it needs to be changed to 'character' type in order to 
## accept activity names. The activity names are taken from 
## the metadata 'NamingActivities'.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(NamingActivities[i,2])
}

## factor the 'activity' variable, once 'activity' names updated.

extractedData$Activity <- as.factor(extractedData$Activity)

## Part 4 - label the data with descriptive variable names (Question 4)

## By examining 'extractedData' we will rename variables as follows:
## Acc can be replaced with Accelerometer
## Gyro can be replaced with Gyroscope
## Mag can be replaced with Magnitude
## Character f can be replaced with Frequency
## Character t can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

## Show the names of the variable after editing

names(extractedData)

## Part 5 - this step creates a 2nd, independant tidy data set with the 
## average of  each variable for each activity in each subject; using the 
## data from step 4

## 1st - set 'Subject' as a factor variable

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

## create tidyData as a data set with average for each activity and 
## subject. Then, order the entries in tidyData and write it into the 
## file 'Tidy.txt' that contains the processed data

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)




