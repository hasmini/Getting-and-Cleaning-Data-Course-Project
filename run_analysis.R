

## 1. Merges the training and the test sets to create one data set

train_data <- read.table ("UCI HAR Dataset/train/X_train.txt")
train_label <- read.table ("UCI HAR Dataset/train/Y_train.txt")
train_subject <- read.table ("UCI HAR Dataset/train/subject_train.txt")
table(train_label)

test_data <- read.table ("UCI HAR Dataset/test/X_test.txt")
test_label <- read.table ("UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table ("UCI HAR Dataset/test/subject_test.txt")
table(test_label)

combine_data <- rbind(train_data, test_data)
combine_label <- rbind(train_label, test_label)
combine_subject <- rbind(train_subject, test_subject)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement

features <- read.table("UCI HAR Dataset/features.txt")

## use grep to extract
mean_std <- grep("mean\\(\\)|std\\(\\)", features[, 2])

length(mean_std) ##checking content

## to extract only mean and std from merged/combined data
combine_data <- combine_data[, mean_std]

## to properly rename Mean and Std
names(combine_data) <- gsub("\\(\\)", "", features[mean_std, 2]) ## to remove "()"
names(combine_data) <- gsub("mean", "Mean", names(combine_data)) ## to capitalize M
names(combine_data) <- gsub("std", "Std", names(combine_data)) ## to capitalize S
names(combine_data) <- gsub("-", "", names(combine_data)) ## to remove "-" in column names

## 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2])) ## tolower for smaller case
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8)) ## substr for replace substrings and convert to upper case "upper case"
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))

activity_labels <- activity_name[combine_label[,1],2]
combine_label[, 1] <- activity_labels
names(combine_label) <- "activity"


## 4. Appropriately labels the data set with descriptive variable names. 

names(combine_subject) <- "subject"
clean_data <- cbind(combine_subject, combine_label, combine_data)
write.table(clean_data,"merged_data.txt") ## write out merged dataset

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(data.table)
dataDT <- data.table(clean_data)
averagedata <- dataDT[, lapply(.SD, mean), by=c("subject", "activity")]
head(averagedata) ##checking

write.table(averagedata,"tidydata_means.txt", row.name=FALSE)
