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

# extract mean and standard deviation
test <- data.frame(mean=apply(testdata, 1, mean, na.rm=TRUE), standarddeviation=apply(testdata, 1, sd, na.rm=TRUE))
train <- data.frame(mean=apply(traindata, 1, mean, na.rm=TRUE), standarddeviation=apply(traindata, 1, sd, na.rm=TRUE))

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

# group and calculate averages
result <- data %>% group_by(activity, subject) %>% summarize(mean=mean(mean), standarddeviation=mean(standarddeviation))

# save the results for submission
write.table(result, file ="result.txt", row.name=FALSE)
