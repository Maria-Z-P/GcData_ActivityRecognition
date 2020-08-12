### Getting and Cleaning Data Course Project

## Downloading raw data

if (!file.exists("./UCI HAR Dataset")) {
    Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(Url, "project_data.zip", method = "curl")
    unzip("project_data.zip")
    
    if(file.exists("project_data.zip")) { file.remove("project_data.zip") }
}

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")  # here read.csv doesn't work
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

## Merging the training and the test sets to create one data set

mtest  <- cbind(xtest, ytest, subject_test)
mtrain <- cbind(xtrain, ytrain, subject_train)
m_train_test <- rbind(mtrain, mtest)

# setting features names

features <- read.table("./UCI HAR Dataset/features.txt")
features <- features[, 2]
colnames(m_train_test) <- c(features, 'activity', 'subject')

## Extracting only the measurements on the mean and standard deviation for each measurement

meanstd <- grep('mean\\(|std\\(', names(m_train_test), value = TRUE)
meanstdactsub <- c(meanstd, 'activity', 'subject')
mdata <- m_train_test[, meanstdactsub]

## Using descriptive activity names to name the activities in the data set

activities <- read.table('./UCI HAR Dataset/activity_labels.txt')
mdata$activity <- factor(mdata$activity, labels = tolower(activities[, 2]))

## Appropriately labeling the data set with descriptive variable names

vnames <- names(mdata)

vnames <- sapply(vnames, tolower)
vnames <- sub(pattern = "\\(\\)-", replacement = "", x = vnames)
vnames <- sub(pattern = "\\(\\)", replacement = "", x = vnames)
vnames <- sub(pattern = "-mean", replacement = ".mean", x = vnames)
vnames <- sub(pattern = "-std", replacement = ".std", x = vnames)

names(mdata) <- vnames
#names(mdata)

## Creating a second, independent tidy data set with the average of each variable
## for each activity and each subject

library(dplyr)

tidy_data <- mdata %>% group_by(subject, activity)

sum_mean <- tidy_data %>% summarise_all(.funs = mean)
#View(sum_mean)

write.table(x = sum_mean, file = "UCI HAR_data.txt", row.names = FALSE)