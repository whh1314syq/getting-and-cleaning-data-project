library(data.table)

library(dplyr)

#1,Merges the training and the test sets to create one data set
featureNames <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
activityLabels <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", header = FALSE)

subjectTrain <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", header = FALSE)

subjectTest <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", header = FALSE)

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

names(features) <- t(featureNames[2])
names(activity) <- "activity"
names(subject) <- "subject"
mergeData <- cbind(features,activity,subject)

#2,Extracts only the measurements on the mean and standard deviation for each measurement
subFeatureNames<-featureNames$V2[grep("mean\\(\\)|std\\(\\)", featureNames$V2)]
selectedNames<-c(as.character(subFeatureNames), "subject", "activity" )
extractedData<-subset(mergeData,select=selectedNames)

#3,Uses descriptive activity names to name the activities in the data set
extractedData$activity <- as.character(extractedData$activity)
for (i in 1:6){
extractedData$activity[extractedData$activity == i] <- as.character(activityLabels[i,2])
}
extractedData$activity <- as.factor(extractedData$activity)


#4,Appropriately labels the data set with descriptive variable names
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

#5,creates a second, independent tidy data set with the average of each variable for each activity and each subject
extractedData$subject <- as.factor(extractedData$subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~subject + activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

help("names")
