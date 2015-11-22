library(reshape2)

	# STEP 1: Merges the training and the test sets to create 	one data set

	# Read subject_test data set	OK

		subjectTest<-read.table("subject_test.txt", 		 	header=FALSE)

	# Read x_test data set	OK

		xTest<-read.table("x_test.txt", header=FALSE)
 	
	# Read y_test data set OK

 		yTest<-read.table("y_test.txt", header=FALSE)

	# Read x_train data set OK

		xTest<-read.table("x_ttrain.txt", header=FALSE)
 	
	# Read y_train data set 	OK

 		yTest<-read.table("y_train.txt", header=FALSE)

	# Read subject_train data set	OK

		subjectTrain<-read.table("subject_train.txt", 	header=FALSE)
	
	# Combine all the Test data set	OK
  
                completeTestSet<-data.frame(subjectTest, yTest, 	xTest)
	
	# Combine all train data set
		
		 completeTrainSet<-data.frame(subjectTrain, yTrain, 		xTrain)
	
	# Combine completeDataSet by completeTrainSet and 	 	completeTestSet data set
		
		completeDataSet <- rbind(completeTrainSet, 	completeTestSet)

	# Read features data set
		
		features <- read.table("features.txt", header=FALSE)

	# Read activity_labels data set
		
		activityLabels <- read.table("activity_labels.txt", 	header=FALSE)

	# create features measurement type data set

		columnNames<-as.vector(features[,2])
	
	# Add Column Names in the Train data set

		colnames(completeDataSet)<-c("subject", "ActivityID", 	columnNames)
	# grep names with mean or standard
		
		MeanandStDataSet <- grep("*mean*| *std*",columnNames) 	#var name

	# Add Column Names in the Train data set

		colnames(columnNames)<-c("featureNames")

	# Use descriptive names for abriviation and Active names for 	ActiveID in the completeDataSet 

        
        
	# sTEP 2. Extracts only the measurements on the mean and  	standard deviation for each  measurement.        	
	        

	 	meanstdcols <- grepl("mean\\(\\)", names		(completeDataSet)) | 
         	grepl("std\\(\\)", names(completeDataSet)) 
 
 
 	# ensure that we also keep the subjectID and activity 	columns 
	 	
		meanstdcols[1:2] <- TRUE  
 	
	# remove unnecessary columns 
	 
		completeDataSet <- completeDataSet[, meanstdcols] 

	# STEP 3: Uses descriptive activity names to name the  	 	activities 
 	# in the data set. 
	# STEP 4: Appropriately labels the data set with descriptive 
	# activity names.  
		
		names(completeDataSet)<-gsub("^t", "time", names			(completeDataSet))
        	names(completeDataSet)<-gsub("^f", "frequency", names	(completeDataSet))
		names(completeDataSet)<-gsub("*Gyro*", "Gyroscope", 	names(completeDataSet))
		names(completeDataSet)<-gsub("*Mag*", "Magnitute", 	names(completeDataSet))
		names(completeDataSet)<-gsub("Acc", "Accelerometer", 	names(dataTable))
		completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 1] = "WALKING" 
     		completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 2] = "WALKING_UPSTAIRS" 
      	completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 3] = "WALKING_DOWNSTAIRS" 
     		completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 4] = "SITTING" 
    		completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 5] = "STANDING" 
     		completeDataSet$ActivityID[ completeDataSet$ActivityID 	== 6] = "LAYING" 

# Step 5  Create a second, independent tidy data set with the average of each variable # for each activity and each subject
melted <- melt(completeDataSet, id=c("subjectID","activity")) tidy <- dcast(melted, subjectID+activity ~ variable, mean)

write.table(completeDataSet,file="TidyDataSet.txt",row.names=FALSE)
  		
