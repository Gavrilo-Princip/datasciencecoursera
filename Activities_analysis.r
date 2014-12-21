##     Joe Curcio 
##      12/21/14
##      Getting and Cleaning Data / Class Project 

##    Step 1 - Aquire Data from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##     Read Data Train in and Test Data 
TempURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(TempURL,"./ProjectData.zip")     ##  Downloads file to local directory
Unlink(TempURL)	    ##  Close TempURL connection
testData <- read.table(" ~ \\X_test.txt", header=FALSE)
trainData <- read.table(" ~ \\X_train.txt", header=FALSE)

##     Step 2 - Create subject data frame from the test data set.  We will Use subjectID, ActivityID, and Activity to build the data set.
##     read in the data 
##     Using subject_test.txt, y_test.txt, and activity_labels.txt data from the original data set
subjectTest <- read.table(" ~ \\subject_test.txt", header=FALSE)
activityTest <- read.table(" ~ \\y_test.txt", header=FALSE)
activityLabels <- read.table(" ~ \\activity_labels.txt", header=FALSE)
##     Change first column name in subjectTest from V1 to SubjectID
v1 <- "subjectID" ##     Assign the column name to variable v1
names(subjectTest) <- v1 ##     Assign the name variable string to the name in the data set.
##     Add columns from activityTest, label activity IDs with the appropriate corresponding activity.
##     Add activityID to subjectTest data frame
subjectTest$activityID <- (activityTest$V1)
##     Clean up Activity data convert uppercase and underscore to lowercase and space between word
a <- sapply(activityLabels, is.factor) #assign all factor columns to a
activityLabels[a] <- lapply(activityLabels[a],as.character) #change all factor columns to string
activityLabels$V2 <- tolower(activityLabels$V2) #make all labels lowercase
activityLabels$V2 <- sub("\\_"," ",activityLabels$V2) #remove the underscore for a space
activityLabels[a] <- lapply(activityLabels[a],as.factor)#change the labels back to factors
##     Create an activity column in trainLabel data frame matching the activityID to the right activity
subjectTest$activity <- factor(subjectTest$activityID,labels=activityLabels$V2)

##     Step 3 - Create subject data from subjects in the training set data. Build data with subjectID, ActivityID, and Activity
##    Read in data from subject_test.txt and y_test.txt. 
subjectTrain <- read.table(" ~ \\subject_test.txt", header=FALSE)
activityTrain <- read.table(" ~ \\y_test.txt", header=FALSE)
##     Change first column name in subjectTrain from V1 to SubjectID
v2 <- "subjectID" 	##     assign the column name to a variable
names(subjectTrain) <- v2 	##     assign the value of the names variable to the column name in our data set.
##      SubjectTrain data set is prepped and ready.  
##     Add the columns from activityTrain and label each activity ID to its corresponding activity.
##     Add the activityID to our subjectTrain data frame
subjectTrain$activityID <- (activityTrain$V1)
##     create an activity column in trainLabel data frame matching the activityID to the corresponding activity
subjectTrain$activity <- factor(subjectTrain$activityID,labels=activityLabels$V2)

##     Step 4 - Merge Subject Train and Test data
subjectInfo <- rbind(subjectTest,subjectTrain)

##     Step 5 - Clean up features variables to Camel style to comply with R naming convention.
##     Read in the features.txt data for processing.
features <- read.table(" ~ \\features.txt", header=FALSE)
##     Change features from a factor into strings to change variable names
i <- sapply(features, is.factor) #assigns factor columns to a variable
features[i] <- lapply(features[i],as.character) #changes all factor columns to string
##     clean up features list before adding header
features$V2 <- sub("mean","Mean",features$V2)
features$V2 <- sub("std","Std",features$V2)
features$V2 <- sub("mad","Mad",features$V2)
features$V2 <- sub("sma","Sma",features$V2)
features$V2 <- sub("energy","Energy",features$V2)
features$V2 <- sub("iqr","Iqr",features$V2)
features$V2 <- sub("max","Max",features$V2)
features$V2 <- sub("min","Min",features$V2)
features$V2 <- sub("entropy","Entropy",features$V2)
features$V2 <- sub("arCoeff","ArCoeff",features$V2)
features$V2 <- sub("correlation","Correlation",features$V2)
features$V2 <- sub("skewness","Skewness",features$V2)
features$V2 <- sub("kurtosis","Kurtosis",features$V2)
features$V2 <- sub("bands","Bands",features$V2)
features$V2 <- sub("gravity","Gravity",features$V2)
features$V2 <- sub("\\()","",features$V2)
features$V2 <- sub("-","",features$V2)
features$V2 <- sub(",","And",features$V2)
features$V2 <- sub("\\(","Of",features$V2)
features$V2 <- gsub("\\)","",features$V2)
##     delete first column in features - only need column 2 with variable names
features2 <- (features$V2)

##     Step 6 - Merge test and train data.
phoneData <- rbind(testData,trainData)

##     Step 7 -  Add feature labels to phoneData. 
names(phoneData) <- features2 #assigns the variable name to phoneData using features2.

##     Step 8 - Extract columns containing data for standard deviation (Std) and Mean. 
##     Exclude data which contains calculations using two or more variables and non-sense data
phoneData2 <- phoneData[,grepl("Subject|Activity|Mean|Std",names(phoneData))]
phoneData2 <- phoneData2[,-grep("angle|Freq",names(phoneData2))]

##     Step 9 - Merge subject data with phone data
phoneData2 <- cbind(subjectInfo,phoneData2)

##     Step 10 - First Tidy Data set
write.table(phoneData2,"phoneDataTidy1",row.names=FALSE)

##     Step 11- Create second tidy data set with subjectID, Activity, and average of each activity and each subject.
aggPhoneData <- aggregate(phoneData2,by=list(phoneData2$subjectID,phoneData2$activity),FUN=mean, na.rm=TRUE)
aggPhoneData <- aggPhoneData[,-grep("subject|activity",names(aggPhoneData))]
colnames(aggPhoneData)[1] <- "SubjectID"
colnames(aggPhoneData)[2] <- "Activity"

##     Step 12 - Save a file copy of your final tidy data set in csv and txt
write.csv(aggPhoneData,"phoneTidyData2.csv",row.names=FALSE)
write.table(aggPhoneData,"phoneTidyData2.txt",row.names=FALSE)
