# Load libraries needed
# install.packages("dplyr")
library(dplyr)

# Download the data
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file_url, "dataset.zip")

# Unzip the data into a folder
unzip("dataset.zip")

# File paths and file names
root <- "UCI HAR Dataset"
testfolder <- file.path(root, "test")
trainfolder <- file.path(root, "train")

# Read test data
testdata <- read.table(file.path(testfolder, "X_test.txt"))
testlabels <- read.table(file.path(testfolder, "y_test.txt"))

# Read train data
traindata <- read.table(file.path(trainfolder, "X_train.txt"))
trainlabels <- read.table(file.path(trainfolder, "y_train.txt"))

# Read subjects
testsubjects <- read.table(file.path(testfolder, "subject_test.txt"))
trainsubjects <- read.table(file.path(trainfolder, "subject_train.txt"))

# extract mean and standard deviation variables
testdata <- tbl_df(testdata)
test <- testdata %>% select(V1,V2,V3,V4,V5,V6,V41,V42,V43,V44,V45,V46,V81,V82,V83,V84,V85,V86,V121,V122,V123,V124,V125,V126,V161,V162,V163,V164,V165,V166,V201,V202,V214,V215,V227,V228,V240,V241,V253,V254,V266,V267,V268,V269,V270,V271,V294,V295,V296,V345,V346,V347,V348,V349,V350,V373,V374,V375,V424,V425,V426,V427,V428,V429,V452,V453,V454,V503,V504,V516,V517,V526,V529,V530,V539,V542,V543,V552)

traindata <- tbl_df(traindata)
train <- traindata %>% select(V1,V2,V3,V4,V5,V6,V41,V42,V43,V44,V45,V46,V81,V82,V83,V84,V85,V86,V121,V122,V123,V124,V125,V126,V161,V162,V163,V164,V165,V166,V201,V202,V214,V215,V227,V228,V240,V241,V253,V254,V266,V267,V268,V269,V270,V271,V294,V295,V296,V345,V346,V347,V348,V349,V350,V373,V374,V375,V424,V425,V426,V427,V428,V429,V452,V453,V454,V503,V504,V516,V517,V526,V529,V530,V539,V542,V543,V552)

# Remove unneeded data frames
rm(testdata)
rm(traindata)

# Read activity labels
labels <- read.table(file.path(root, "activity_labels.txt"))
colnames(labels)[2] <- "activity"

# Prepare test labels
colnames(testlabels)[1] <- "activitycode"
testlabels <- testlabels %>% merge(labels, by.x = "activitycode", by.y = "V1") %>% select(activity)

#Prepare train labels
colnames(trainlabels)[1] <- "activitycode"
trainlabels <- trainlabels %>% merge(labels, by.x = "activitycode", by.y = "V1") %>% select(activity)

# Prepare subjects
colnames(testsubjects)[1] <- "subject"
colnames(trainsubjects)[1] <- "subject"

# Add labels to data
test <- cbind(test, testlabels, testsubjects)
train <- cbind(train, trainlabels, trainsubjects)

# Merge test and train data sets
data <- rbind(test, train)

# Assign meaningful variable names
colnames(data) <- c("tBodyAcc-mean-X", "tBodyAcc-mean-Y", "tBodyAcc-mean-Z", "tBodyAcc-std-X", "tBodyAcc-std-Y", "tBodyAcc-std-Z", "tGravityAcc-mean-X", "tGravityAcc-mean-Y", "tGravityAcc-mean-Z", "tGravityAcc-std-X", "tGravityAcc-std-Y", "tGravityAcc-std-Z", "tBodyAccJerk-mean-X", "tBodyAccJerk-mean-Y", "tBodyAccJerk-mean-Z", "tBodyAccJerk-std-X", "tBodyAccJerk-std-Y", "tBodyAccJerk-std-Z", "tBodyGyro-mean-X", "tBodyGyro-mean-Y", "tBodyGyro-mean-Z", "tBodyGyro-std-X", "tBodyGyro-std-Y", "tBodyGyro-std-Z", "tBodyGyroJerk-mean-X", "tBodyGyroJerk-mean-Y", "tBodyGyroJerk-mean-Z", "tBodyGyroJerk-std-X", "tBodyGyroJerk-std-Y", "tBodyGyroJerk-std-Z", "tBodyAccMag-mean", "tBodyAccMag-std", "tGravityAccMag-mean", "tGravityAccMag-std", "tBodyAccJerkMag-mean", "tBodyAccJerkMag-std", "tBodyGyroMag-mean", "tBodyGyroMag-std", "tBodyGyroJerkMag-mean", "tBodyGyroJerkMag-std", "fBodyAcc-mean-X", "fBodyAcc-mean-Y", "fBodyAcc-mean-Z", "fBodyAcc-std-X", "fBodyAcc-std-Y", "fBodyAcc-std-z", "fBodyAcc-meanFreq-X", "fBodyAcc-meanFreq-Y", "fBodyAcc-meanFreq-Z", "fBodyAccJerk-mean-X", "fBodyAccJerk-mean-Y", "fBodyAccJerk-mean-Z", "fBodyAccJerk-std-X", "fBodyAccJerk-std-Y", "fBodyAccJerk-std-Z", "fBodyAccJerk-meanFreq-X", "fBodyAccJerk-meanFreq-Y", "fBodyAccJerk-meanFreq-Z", "fBodyGyro-mean-X", "fBodyGyro-mean-Y", "fBodyGyro-mean-Z", "fBodyGyro-std-X", "fBodyGyro-std-Y", "fBodyGyro-std-Z", "fBodyGyro-meanFreq-X", "fBodyGyro-meanFreq-Y", "fBodyGyro-meanFreq-Z", "fBodyAccMag-mean", "fBodyAccMag-std", "fBodyBodyAccJerkMag-mean", "fBodyBodyAccJerkMag-std", "fBodyBodyAccJerkMag-meanFreq", "fBodyBodyGyroMag-mean", "fBodyBodyGyroMag-std", "fBodyBodyGyroMag-meanFreq", "fBodyBodyGyroJerkMag-mean", "fBodyBodyGyroJerkMag-std", "fBodyBodyGyroJerkMag-meanFreq", "activity", "subject")

# group and calculate averages
result <- data %>% group_by(activity, subject) %>% summarize(mean=mean(mean), standarddeviation=mean(standarddeviation))

# save the results for submission
write.table(result, file ="result.txt", row.name=FALSE)
