##############################################
## Getting and Cleaning Data Course Project ##
##############################################

#author: Michiel Meijer (michielmeijer1979@yahoo.co.uk)
#juli 2017

###########
## Setup ##
########### 
#Clean environment
rm(list = ls())

#Libraries
library(dplyr)

#working directory - which already has the dataset for the project downloaded into it
#setwd("C:/Users/MichielM/rcoursera/03_GettingAndCleaningData/Week4/UCI HAR Dataset")

##################
## Getting data ##
##################
# in this part the given datasets for the project are made tidy

#features.txt
features <-  read.table("features.txt", sep = '\n', header = FALSE)
colnames(features) <- "feature_name"
features$feature_name <- gsub("\\d+","",features$feature_name)
features$feature_name <- gsub(" ", "", features$feature_name)

#activity_labels.txt
activity_labels <-  read.table("activity_labels.txt")
colnames(activity_labels) <- c("activity_number", "activtity_name")

#test files
X_test <- read.table("test/X_test.txt")
colnames(X_test) <- features$feature_name
Y_test <- read.table("test/Y_test.txt")
colnames(Y_test) <- "activity_number"
subject_test <- read.table("test/subject_test.txt")
colnames(subject_test) <- "subject_number"

#train files
X_train <- read.table("train/X_train.txt")
colnames(X_train) <- features$feature_name
Y_train <- read.table("train/Y_train.txt")
colnames(Y_train) <- "activity_number"
subject_train <- read.table("train/subject_train.txt")
colnames(subject_train) <- "subject_number"


#####################################################################
# 1. Merges the training and the test sets to create one data set. ##
#####################################################################
# in this part X, Y and subject from both the test and train set are
# merged into one dataframe

# rbind X_test and X_train into dataframe X 
X <- rbind(X_test, X_train)

# rbind X_test and X_train into dataframe X 
Y <- rbind(Y_test, Y_train)

# cbind X and Y into Z
Z <- cbind(X, Y)

# rbind subject_test and subjects_train
subject <- rbind(subject_test, subject_train)

# cbind subject and Z into Z
Z <- cbind(subject, Z)

################################################################
## 2. Extracts only the measurements                          ##
## on the mean and standard deviation for each measurement.   ##
################################################################
# in this parts all but the mean and std variables from X are removed.

#variables are to math names mean() or std() to make sure 
# ("Mean"is deliberatly excluded) )

#all mean variables
mean_vars <- colnames(Z[grepl("mean()", colnames(Z))])

#all sd variables
std_vars <- colnames(Z[grepl("std()", colnames(Z))])

#all mean an sd variables
mean_and_std_vars <- c(mean_vars, std_vars)

#extract only mean and std measurements
Z <- Z[,c("subject_number", "activity_number", mean_and_std_vars)]

################################################################
## 3. Uses descriptive activity names to name the activities  ##
## in the data set                                            ##
################################################################
# in this part the activity_numbers are transformed into activity_labels

#merge dfs activity_labels and Z
Z <- merge.data.frame(activity_labels, Z)

#remove columns activity_number 
Z <- Z[,!names(Z) == "activity_number"]

###########################################################
## 4. Appropriately labels the data set with descriptive ##
## variable names.                                       ##
###########################################################

# This is not really an extra step in this script, since the column 
# names are made appropriate in earlier steps (for instance 
# activity_name and subject_number).

###################################################################
## 5. From the data set in step 4, creates a second, independent ##
## tidy data set with the average of each variable for each      ##
## activity and each subject.                                    ##
###################################################################
# the mean of ech variable is calculated fro every activity as well 
# as for every subject. The resulting dataframes are then bound into 
# 1 tidy dataframe.

#means per activity
activity_mean <- aggregate(Z[, 3:ncol(Z)], list(Z$activtity_name), mean)
activity_mean$activity_or_subject <- "Activity"

#means per subject
subject_mean <- aggregate(Z[, 3:ncol(Z)], list(Z$subject_number), mean)
subject_mean$activity_or_subject <- "Subject"
subject_mean$Group.1 <- as.factor(subject_mean$Group.1)

#fit means per activity and per subject into one dataframe
mean_per_variable <- rbind(activity_mean, subject_mean)

#reorder so that column "activity_or_subject" is the first column
mean_per_variable <- mean_per_variable[c("activity_or_subject", names(mean_per_variable[,1:80]))]

#rename column Group.1 into Name 
mean_per_variable <- rename(mean_per_variable, Name = Group.1)

################################################
## Export dataframe mean_per_variable as .txt ##
################################################
# in this step the resulting dataframe form the previos step is exported 
# as "mean_per_variable.txt" into the working directory.

write.table(mean_per_variable, file = "mean_per_variable.txt", sep = ";", row.name=FALSE)
dir()
source(run_analysis.R)
getwd()
names(mean_per_variable)