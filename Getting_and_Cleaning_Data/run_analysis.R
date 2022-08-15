library(dplyr)

# Downloading the dataset and unzipping in current working directory

 
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = 'course_project.zip')
unzip('course_project.zip')


# Constructing data frames corresponding to all the relevant data sets and giving appropriate column names


features <- read.table("UCI HAR Dataset/features.txt", col.names = c("i","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity_code", "activity"))
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_number")
test_features <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
test_activity <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_code")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_number")
train_features <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
train_activity <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_code")
 

# Merging the test and training datasets 


Test_train_features <- rbind(train_features, test_features)
Test_train_activity <- rbind(train_activity, test_activity)
Subject_Number <- rbind(train_subject, test_subject)
Merged_Set <- cbind(Subject_Number, Test_train_activity , Test_train_features)


# Extracting only the mean and standard deviation for each of the different types of measurements 


Merged_Set <- Merged_Set %>% select(subject_number, activity_code, contains("mean"), contains("std"))


# Replacing activity code numbers with actual activity name


Merged_Set$activity_code <- activities[Merged_Set$activity_code, 2]


# Adding descriptive variable names as labels in the dataset 


names(Merged_Set)[2] = "activity"
names(Merged_Set)<-gsub("Acc", "Accelerometer", names(Merged_Set))
names(Merged_Set)<-gsub("Gyro", "Gyroscope", names(Merged_Set))
names(Merged_Set)<-gsub("BodyBody", "Body", names(Merged_Set))
names(Merged_Set)<-gsub("Mag", "Magnitude", names(Merged_Set))
names(Merged_Set)<-gsub("^t", "Time", names(Merged_Set))
names(Merged_Set)<-gsub("^f", "Frequency", names(Merged_Set))
names(Merged_Set)<-gsub("tBody", "TimeBody", names(Merged_Set))
names(Merged_Set)<-gsub("-mean()", "Mean", names(Merged_Set), ignore.case = TRUE)
names(Merged_Set)<-gsub("-std()", "STD", names(Merged_Set), ignore.case = TRUE)
names(Merged_Set)<-gsub("-freq()", "Frequency", names(Merged_Set), ignore.case = TRUE)
names(Merged_Set)<-gsub("angle", "Angle", names(Merged_Set))
names(Merged_Set)<-gsub("gravity", "Gravity", names(Merged_Set))


# Creating the final data frame with average value of each functions by subject and activity


Final_Data_Frame <- Merged_Set %>%
  group_by(subject_number, activity) %>%
  summarise_all(funs(mean))
write.table(Final_Data_Frame, "FinalDataFrame.txt", row.name=FALSE)


Final_Data_Frame
