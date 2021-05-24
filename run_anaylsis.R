library(dplyr)
filename <- "Coursera Getting and Clearning Data Course Project.zip"
# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

1#Merges the training and the test sets to create one data set.

xdatacomb <- rbind(xtrain, xtest)
ydatacomb <- rbind(ytrain,ytest)
subjectcomb <- rbind(subjecttest,subjecttrain)
wholemerge <- cbind(subjectcomb, xdatacomb, ydatacomb)

2#Extracts only the measurements on the mean and standard deviation for each measurement. 

tidydata <- wholemerge %>% select(subject,code,contains("mean"), contains("std"))

3#Uses descriptive activity names to name the activities in the data set
tidydata$code <- activities[tidydata$code,2]
4#Appropriately labels the data set with descriptive variable names. 
names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Accel", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Magn", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angl", "Angle", names(tidydata))
names(tidydata)<-gsub("grav", "Gravity", names(tidydata))
5#:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(final, "FinalData.txt", row.name=FALSE)

