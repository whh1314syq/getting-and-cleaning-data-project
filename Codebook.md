#Code book

##1,Istalling the packages of data.table,dplyr and reshape2 to gain tidy data
##2,reading data set in R  as following:
 featureNames <- read.table("UCI HAR Dataset/features.txt")<br>
 activityLabels <- read.table("/UCI HAR Dataset/activity_labels.txt", header = FALSE)<br>
 subjectTrain <- read.table("/UCI HAR    Dataset/train/subject_train.txt", header = FALSE)<br>
 activityTrain <- read.table(" /UCI HAR Dataset/train/y_train.txt", header = FALSE)<br>
 featuresTrain <- read.table(" /UCI HAR Dataset/train/X_train.txt", header = FALSE)<br>
 subjectTest <- read.table(" /UCI HAR Dataset/test/subject_test.txt", header = FALSE)<br>
 activityTest <- read.table(" t/UCI HAR Dataset/test/y_test.txt", header = FALSE)<br>
 featuresTest <- read.table(" /UCI HAR Dataset/test/X_test.txt", header = FALSE)<br>
##3,adding rows
 subject <- rbind(subjectTrain, subjectTest)<br>
 activity <- rbind(activityTrain, activityTest)<br>
 features <- rbind(featuresTrain, featuresTest)<br>
##4,renaming 
 names(features) <- t(featureNames[2])<br>
 names(activity) <- "activity"<br>
 names(subject) <- "subject"<br>
 mergeData <- cbind(features,activity,subject)<br>
##5,Extracts only the measurements on the mean and standard deviation for each measurement
  subFeatureNames<-featureNames$V2[grep("mean\\(\\)|std\\(\\)", featureNames$V2)]<br>
  selectedNames<-c(as.character(subFeatureNames), "subject", "activity" )<br>
  extractedData<-subset(mergeData,select=selectedNames)<br>
##6,Uses descriptive activity names to name the activities in the data set
  extractedData$activity <- as.character(extractedData$activity)<br>
  for (i in 1:6){<br>
  extractedData$activity[extractedData$activity == i] <- as.character(activityLabels[i,2])<br>
  }<br>
  extractedData$activity <- as.factor(extractedData$activity)<br>
##7,Appropriately labels the data set with descriptive variable names
  names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))<br>
 names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))<br>
 names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))<br>
 names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))<br>
 names(extractedData)<-gsub("^t", "Time", names(extractedData))<br>
 names(extractedData)<-gsub("^f", "Frequency", names(extractedData))<br>
 names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))<br>
 names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)<br>
 names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)<br>
 names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)<br>
  names(extractedData)<-gsub("angle", "Angle", names(extractedData))<br>
 names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))<br>
##8,creates a second, independent tidy data set with the average of each variable for each activity   and each subject
 extractedData$subject <- as.factor(extractedData$subject)<br>
 extractedData <- data.table(extractedData)
 tidyData <- aggregate(. ~subject + activity, extractedData, mean)<br>
 tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]<br>
 write.table(tidyData, file = "Tidy.txt", row.names = FALSE)<br>
