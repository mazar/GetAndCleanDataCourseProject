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
colnames(data) <- c("tBodyAccmeanX", "tBodyAccmeanY", "tBodyAccmeanZ", "tBodyAccstdX", "tBodyAccstdY", "tBodyAccstdZ", "tGravityAccmeanX", "tGravityAccmeanY", "tGravityAccmeanZ", "tGravityAccstdX", "tGravityAccstdY", "tGravityAccstdZ", "tBodyAccJerkmeanX", "tBodyAccJerkmeanY", "tBodyAccJerkmeanZ", "tBodyAccJerkstdX", "tBodyAccJerkstdY", "tBodyAccJerkstdZ", "tBodyGyromeanX", "tBodyGyromeanY", "tBodyGyromeanZ", "tBodyGyrostdX", "tBodyGyrostdY", "tBodyGyrostdZ", "tBodyGyroJerkmeanX", "tBodyGyroJerkmeanY", "tBodyGyroJerkmeanZ", "tBodyGyroJerkstdX", "tBodyGyroJerkstdY", "tBodyGyroJerkstdZ", "tBodyAccMagmean", "tBodyAccMagstd", "tGravityAccMagmean", "tGravityAccMagstd", "tBodyAccJerkMagmean", "tBodyAccJerkMagstd", "tBodyGyroMagmean", "tBodyGyroMagstd", "tBodyGyroJerkMagmean", "tBodyGyroJerkMagstd", "fBodyAccmeanX", "fBodyAccmeanY", "fBodyAccmeanZ", "fBodyAccstdX", "fBodyAccstdY", "fBodyAccstdz", "fBodyAccmeanFreqX", "fBodyAccmeanFreqY", "fBodyAccmeanFreqZ", "fBodyAccJerkmeanX", "fBodyAccJerkmeanY", "fBodyAccJerkmeanZ", "fBodyAccJerkstdX", "fBodyAccJerkstdY", "fBodyAccJerkstdZ", "fBodyAccJerkmeanFreqX", "fBodyAccJerkmeanFreqY", "fBodyAccJerkmeanFreqZ", "fBodyGyromeanX", "fBodyGyromeanY", "fBodyGyromeanZ", "fBodyGyrostdX", "fBodyGyrostdY", "fBodyGyrostdZ", "fBodyGyromeanFreqX", "fBodyGyromeanFreqY", "fBodyGyromeanFreqZ", "fBodyAccMagmean", "fBodyAccMagstd", "fBodyBodyAccJerkMagmean", "fBodyBodyAccJerkMagstd", "fBodyBodyAccJerkMagmeanFreq", "fBodyBodyGyroMagmean", "fBodyBodyGyroMagstd", "fBodyBodyGyroMagmeanFreq", "fBodyBodyGyroJerkMagmean", "fBodyBodyGyroJerkMagstd", "fBodyBodyGyroJerkMagmeanFreq", "activity", "subject")

# group and calculate averages
result <- data %>% group_by(activity, subject) %>% summarize(tBodyAccmeanX=mean(tBodyAccmeanX), tBodyAccmeanY=mean(tBodyAccmeanY), tBodyAccmeanZ=mean(tBodyAccmeanZ), tBodyAccstdX=mean(tBodyAccstdX), tBodyAccstdY=mean(tBodyAccstdY), tBodyAccstdZ=mean(tBodyAccstdZ), tGravityAccmeanX=mean(tGravityAccmeanX), tGravityAccmeanY=mean(tGravityAccmeanY), tGravityAccmeanZ=mean(tGravityAccmeanZ), tGravityAccstdX=mean(tGravityAccstdX), tGravityAccstdY=mean(tGravityAccstdY), tGravityAccstdZ=mean(tGravityAccstdZ), tBodyAccJerkmeanX=mean(tBodyAccJerkmeanX), tBodyAccJerkmeanY=mean(tBodyAccJerkmeanY), tBodyAccJerkmeanZ=mean(tBodyAccJerkmeanZ), tBodyAccJerkstdX=mean(tBodyAccJerkstdX), tBodyAccJerkstdY=mean(tBodyAccJerkstdY), tBodyAccJerkstdZ=mean(tBodyAccJerkstdZ), tBodyGyromeanX=mean(tBodyGyromeanX), tBodyGyromeanY=mean(tBodyGyromeanY), tBodyGyromeanZ=mean(tBodyGyromeanZ), tBodyGyrostdX=mean(tBodyGyrostdX), tBodyGyrostdY=mean(tBodyGyrostdY), tBodyGyrostdZ=mean(tBodyGyrostdZ), tBodyGyroJerkmeanX=mean(tBodyGyroJerkmeanX), tBodyGyroJerkmeanY=mean(tBodyGyroJerkmeanY), tBodyGyroJerkmeanZ=mean(tBodyGyroJerkmeanZ), tBodyGyroJerkstdX=mean(tBodyGyroJerkstdX), tBodyGyroJerkstdY=mean(tBodyGyroJerkstdY), tBodyGyroJerkstdZ=mean(tBodyGyroJerkstdZ), tBodyAccMagmean=mean(tBodyAccMagmean), tBodyAccMagstd=mean(tBodyAccMagstd), tGravityAccMagmean=mean(tGravityAccMagmean), tGravityAccMagstd=mean(tGravityAccMagstd), tBodyAccJerkMagmean=mean(tBodyAccJerkMagmean), tBodyAccJerkMagstd=mean(tBodyAccJerkMagstd), tBodyGyroMagmean=mean(tBodyGyroMagmean), tBodyGyroMagstd=mean(tBodyGyroMagstd), tBodyGyroJerkMagmean=mean(tBodyGyroJerkMagmean), tBodyGyroJerkMagstd=mean(tBodyGyroJerkMagstd), fBodyAccmeanX=mean(fBodyAccmeanX), fBodyAccmeanY=mean(fBodyAccmeanY), fBodyAccmeanZ=mean(fBodyAccmeanZ), fBodyAccstdX=mean(fBodyAccstdX), fBodyAccstdY=mean(fBodyAccstdY), fBodyAccstdz=mean(fBodyAccstdz), fBodyAccmeanFreqX=mean(fBodyAccmeanFreqX), fBodyAccmeanFreqY=mean(fBodyAccmeanFreqY), fBodyAccmeanFreqZ=mean(fBodyAccmeanFreqZ), fBodyAccJerkmeanX=mean(fBodyAccJerkmeanX), fBodyAccJerkmeanY=mean(fBodyAccJerkmeanY), fBodyAccJerkmeanZ=mean(fBodyAccJerkmeanZ), fBodyAccJerkstdX=mean(fBodyAccJerkstdX), fBodyAccJerkstdY=mean(fBodyAccJerkstdY), fBodyAccJerkstdZ=mean(fBodyAccJerkstdZ), fBodyAccJerkmeanFreqX=mean(fBodyAccJerkmeanFreqX), fBodyAccJerkmeanFreqY=mean(fBodyAccJerkmeanFreqY), fBodyAccJerkmeanFreqZ=mean(fBodyAccJerkmeanFreqZ), fBodyGyromeanX=mean(fBodyGyromeanX), fBodyGyromeanY=mean(fBodyGyromeanY), fBodyGyromeanZ=mean(fBodyGyromeanZ), fBodyGyrostdX=mean(fBodyGyrostdX), fBodyGyrostdY=mean(fBodyGyrostdY), fBodyGyrostdZ=mean(fBodyGyrostdZ), fBodyGyromeanFreqX=mean(fBodyGyromeanFreqX), fBodyGyromeanFreqY=mean(fBodyGyromeanFreqY), fBodyGyromeanFreqZ=mean(fBodyGyromeanFreqZ), fBodyAccMagmean=mean(fBodyAccMagmean), fBodyAccMagstd=mean(fBodyAccMagstd), fBodyBodyAccJerkMagmean=mean(fBodyBodyAccJerkMagmean), fBodyBodyAccJerkMagstd=mean(fBodyBodyAccJerkMagstd), fBodyBodyAccJerkMagmeanFreq=mean(fBodyBodyAccJerkMagmeanFreq), fBodyBodyGyroMagmean=mean(fBodyBodyGyroMagmean), fBodyBodyGyroMagstd=mean(fBodyBodyGyroMagstd), fBodyBodyGyroMagmeanFreq=mean(fBodyBodyGyroMagmeanFreq), fBodyBodyGyroJerkMagmean=mean(fBodyBodyGyroJerkMagmean), fBodyBodyGyroJerkMagstd=mean(fBodyBodyGyroJerkMagstd), fBodyBodyGyroJerkMagmeanFreq=mean(fBodyBodyGyroJerkMagmeanFreq))

# save the results for submission
write.table(result, file ="result.txt", row.name=FALSE)
