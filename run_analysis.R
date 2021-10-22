
setwd("/Users/hyeaw/Desktop/Coursera/Getting abd Cleaning Data/Week 4/train/Inertial Signals")
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### process to time body acceleration
  library(reshape2) 

# Reading train files 
xtrain <- read.table("X_train.txt") 
ytrain <- read.table("Y_train.txt") 
subjecttrain <- read.table("subject_train.txt")

# Reading test files
xtest <- read.table("X_test.txt")
ytest <- read.table("Y_test.txt")
subjecttest <- read.table("subject_test.txt")

# activity labels, features, and features info files
activitylabels <- read.table("activity_labels.txt")

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### I will use the following data only
subjecttrain<-read.table("subject_train.txt")
xtrain<-read.table("X_train.txt")
ytrain<-read.table("Y_train.txt")  # train labels

subjecttest<-read.table("subject_test.txt")
xtest<-read.table("X_test.txt")
ytest<-read.table("Y_test.txt") # test labels

features<-read.table("features.txt")
activitylabels<-read.table("activity_labels.txt")
features <- read.table("features.txt")

#Merging the subject, activity, and features
# subject, activity, and features row binding
df_subject <- rbind(subjecttrain, subjecttest)
df_data <- rbind(xtrain, xtest)
df_activity <- rbind(ytrain, ytest)

# add subjectID and activityID in the subjectandactivity table
subjectandactivity<- c("subjectID","activityID")

# combine subject, activity, and features column binding
data <- cbind(df_data, df_subject,df_activity)   
data_colnames<- c(features$V2,subjectandactivity)  # combined the entire columns
names(data)<-data_colnames

#Extracts only the measurements on the mean and standard deviation for each measurement.
colnames(data)<-gsub("mean", "Mean", colnames(data))
meanstdcols <-data[grep("Mean\\(\\)|std\\(\\)",names(data))]  

# Use descriptive activity names in the data set

meanstdcols$activityID[meanstdcols$activityID == 1] = "WALKING" 
meanstdcols$activityID[meanstdcols$activityID == 2] = "WALKING_UPSTAIRS"
meanstdcols$activityID[meanstdcols$activityID == 3] = "WALKING_DOWNSTAIRS"
meanstdcols$activityID[meanstdcols$activityID == 4] = "SITTING"
meanstdcols$activityID[meanstdcols$activityID == 5] = "STANDING"
meanstdcols$activityID[meanstdcols$activityID == 6] = "LAYING"

#Use descriptive variable names in the data set

colnames(meanstdcols)<-gsub("^t", "time", colnames(meanstdcols))
colnames(meanstdcols)<-gsub("^f", "frequency", colnames(meanstdcols))
colnames(meanstdcols)<-gsub("Acc", "Accelerometer", colnames(meanstdcols))
colnames(meanstdcols)<-gsub("Gyro", "Gyroscope", colnames(meanstdcols))
colnames(dmeanstdcols)<-gsub("Mag", "Magnitude", colnames(meanstdcols))
colnames(meanstdcols)<-gsub("BodyBody", "Body", colnames(meanstdcols))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata <- aggregate(. ~subjectID + activityID, meanstdcols, mean)
write.table(data.frame(tidydata), file = "tidydata.txt", row.names = FALSE)