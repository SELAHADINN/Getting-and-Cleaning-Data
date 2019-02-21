#first download the zip file from the given assignment in to your working directory
#and unzip the file
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# then read each .txt files according to train and test data plus name (subject_train)

# Reading trainings tables of .txt data type:
y_train <- read.table("y_train.txt")
x_train <- read.table("X_train.txt")
subject_train <- read.table("subject_train.txt")

#  Reading testing tables of .txt data type:
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

#  Reading feature vector of the form .txt data type:
features <- read.table('features.txt')

# Reading activity labels with data type .txt:
activityLabels = read.table('activity_labels.txt')

# Assigning column names:

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# Merging all data in one set:

mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

#Extracts only the measurements on the mean and standard deviation for the measurement.

# Reading column names:

colNames <- colnames(setAllInOne)

# Create vector for defining ID, mean and standard deviation:

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

# Making nessesary subset from setAllInOne:

setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

#in this step Uses descriptive activity names to name the activities in the data set


setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)


# Appropriately labels the data set with descriptive variable names.

#Done in previous steps 

# From the data set in above, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#Making a second tidy data set

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

# Writing second tidy data set in txt file

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
