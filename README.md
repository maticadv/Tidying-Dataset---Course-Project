# Tidying-Dataset---Course-Project
## Course Project for Getting and Cleaning Data
## INSTRUCTIONS:
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## EXPLANATION OF SCRIPT:

## Calling for libraries and choosefile function:
library(data.table)
library(dplyr)
choose.files()

# 1. Merges the training and the test sets to create one data set.

## Reading the dataset, both train and test:
test_data <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\test\\X_test.txt")
train_data <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\train\\X_train.txt")
# Merging both:
data_binded <- rbind(test_data, train_data)

## Reading the labels, both train and test: 
test_labels <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\test\\y_test.txt")
train_labels <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\train\\y_train.txt")
# Merging both:
labels_binded <- rbind(test_labels, train_labels)

## Reading the subjects, both train and test:
test_subjects <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\test\\subject_test.txt")
train_subjects <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\train\\subject_train.txt")
# Merging both:
subjects_binded <- rbind(test_subjects, train_subjects)

## Removing useless data tables:
rm(test_data, train_data, test_labels, train_labels, test_subjects, train_subjects)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## Reading features data:
features <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\features.txt")$V2
## Obtaining mean and deviation from features:
columns_to_keep <- grepl("(std|mean)", features)
## Binding in data_binded
data_binded <- data_binded[ , columns_to_keep]
## Renaming the column of the data set 
names(data_binded) <- features[columns_to_keep]
## Removing the lists
rm(features, columns_to_keep)


# 3. Uses descriptive activity names to name the activities in the data set.

## The first column of activities is numbers:
activity <- read.table("C:\\Users\\ADMIN\\Desktop\\Data Scientist Specialization\\Course 3 - Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\activity_labels.txt")
activity[,2] <- gsub("_", "", tolower(as.character(activity[,2])))
## Associating to any observation the correponding activity
## Renaming activities with descriptive names
labels_binded[,1] <- activity[labels_binded[,1], 2]
rm(activity)
## Modifying the column label
names(labels_binded) <- "activity"
## Adding the column to the data set and we remove the data
data_binded$activity <- labels_binded
rm(labels_binded)
## Repeating with subjects
names(subjects_binded) <- "subject"
data_binded$subject <- subjects_binded
rm(subjects_binded)


# 4. Appropriately labels the data set with descriptive variable names:

names(data_binded) <- gsub("^t", "time", as.character(names(data_binded)))
names(data_binded) <- gsub("^f", "frequency", as.character(names(data_binded)))
names(data_binded) <- gsub("Acc", "accelerometer", as.character(names(data_binded)))
names(data_binded) <- gsub("Gyro", "gyroscope", as.character(names(data_binded)))
names(data_binded) <- gsub("Jerk", "jerksignal", as.character(names(data_binded)))
names(data_binded) <- gsub("Mag", "magnitude", as.character(names(data_binded)))
names(data_binded) <- gsub("meanFreq", "meanfrequency", as.character(names(data_binded)))
names(data_binded) <- gsub("X", "directionx", as.character(names(data_binded)))
names(data_binded) <- gsub("Y", "directiony", as.character(names(data_binded)))
names(data_binded) <- gsub("Z", "directionz", as.character(names(data_binded)))

## Everything in lower case and removing non alphanumeric characters
names(data_binded) <- gsub("[\\(\\)-]", "", tolower(as.character(names(data_binded))))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

## Creating an empty dataset with the right number of columns and no rows
New_dataset <- data.frame(matrix(ncol = ncol(data_binded), nrow = 0))
## Extrapolating names to it, in order to use rbind later
names(New_dataset) <- names(data_binded)
## Creating a list of subjects and a list of activities by extracting from column
subjects <- unique(data_binded$subject)$subject
activities <- unique(data_binded$activity)$activity
for (subject in subjects) {
for (activity in activities) {
## Creating temporary row with the means, so we exclude the activity and subject number from it.
## colMeans returns a vector, that I need to transpose (t()) in order to get a data frame with
## The correct dimensions (1 row, many columns).
temp_row <- data.frame(t(colMeans(data_binded[data_binded$subject == subject & data_binded$activity == activity, 1:(ncol(data_binded)-2)])))
## Then we need to add the last two data to it
temp_row$activity <- activity
temp_row$subject <- subject
## Appending this row to the previously created new tidy data set
New_dataset <- rbind(New_dataset, temp_row)
## Print(dim(new_data_set))
## Destroying the temporary row
rm(temp_row)
}}
## Removing useless variables
rm(activity, activities, subject, subjects)
## Writing to disk the new data set in a table
write.table(New_dataset, file = "tidydataset.txt", row.names = FALSE)
