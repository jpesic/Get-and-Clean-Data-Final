library(readr)
library(tidyr)
library(dplyr)

setwd("~/R_Scripts/Programming_Examples/Clean_Data")
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', 'project_data.zip')
unzip('project_data.zip')

setwd('UCI HAR Dataset')
features <- read.csv(textConnection(gsub("  ", " ", readLines("features.txt"))), header=FALSE,sep=" ", stringsAsFactors = FALSE)

setwd("~/R_Scripts/Programming_Examples/Clean_Data/UCI HAR Dataset/train")

#read and merge train files
subject_train <- read.csv(textConnection(gsub("  ", " ", readLines("subject_train.txt"))), header=FALSE,sep=" ")
x_train <- read.csv(textConnection(gsub("  ", " ", readLines("X_train.txt"))), header=FALSE,sep=" ")
y_train <- read.csv(textConnection(gsub("  ", " ", readLines("y_train.txt"))), header=FALSE,sep=" ")
x_train<-x_train[,2:ncol(x_train)]
train_set<-cbind(subject_train, y_train, x_train)
colnames(train_set)<-c("subject","activity.labels",t(features[,2]))
train_set$'train/test'<-'train'

setwd("~/R_Scripts/Programming_Examples/Clean_Data/UCI HAR Dataset/test")

#read and merge test files
subject_test <- read.csv(textConnection(gsub("  ", " ", readLines("subject_test.txt"))), header=FALSE,sep=" ")
x_test <- read.csv(textConnection(gsub("  ", " ", readLines("X_test.txt"))), header=FALSE,sep=" ")
y_test <- read.csv(textConnection(gsub("  ", " ", readLines("y_test.txt"))), header=FALSE,sep=" ")
x_test<-x_test[,2:ncol(x_test)]
test_set<-cbind(subject_test, y_test, x_test)
colnames(test_set)<-c("subject","activity.labels",t(features[,2]))
test_set$'train/test'<-'test'

#1. merge test/train data set
data_set<-rbind(train_set, test_set)

#2. extract cols with Mean/std in the names
data_set_mean_std<-data_set[,grepl("subject|labels|train|mean()|std()", names(data_set) )]
data_set_mean_std<-data_set_mean_std[,!grepl("meanFreq()", names(data_set_mean_std) )]

#3. mutate activity labels to names
data_set_mean_std$activity.labels<-factor(data_set_mean_std$activity.labels, labels=c("walking", "walking upstairs", "walking downstaris", "sitting", "standing", "laying"))

#4.labels the data set with descriptive variable names
column_names<-c("subject",
                           "activity.labels",
                           "body.acceleration.mean.x(time)",
                           "body.acceleration.mean.y(time)",
                           "body.acceleration.mean.z(time)",
                           "body.acceleration.std.x(time)",
                           "body.acceleration.std.y(time)",
                           "body.acceleration.std.z(time)",
                           
                           "gravity.acceleration.mean.x(time)",
                           "gravity.acceleration.mean.y(time)",
                           "gravity.acceleration.mean.z(time)",
                           "gravity.acceleration.std.x(time)",
                           "gravity.acceleration.std.y(time)",
                           "gravity.acceleration.std.z(time)",
                           
                           "body.acceleration.jerk.mean.x(time)",
                           "body.acceleration.jerk.mean.y(time)",
                           "body.acceleration.jerk.mean.z(time)",
                           "body.acceleration.jerk.std.x(time)",
                           "body.acceleration.jerk.std.y(time)",
                           "body.acceleration.jerk.std.z(time)",
                           
                           "body.angular.velocity.mean.x(time)",
                           "body.angular.velocity.mean.y(time)",
                           "body.angular.velocity.mean.z(time)",
                           "body.angular.velocity.std.x(time)",
                           "body.angular.velocity.std.y(time)",
                           "body.angular.velocity.std.z(time)",
                           
                           "body.angular.jerk.mean.x(time)",
                           "body.angular.jerk.mean.y(time)",
                           "body.angular.jerk.mean.z(time)",
                           "body.angular.jerk.std.x(time)",
                           "body.angular.jerk.std.y(time)",
                           "body.angular.jerk.std.z(time)",

                           "body.acceleration.magnitude.mean(time)",
                           "body.acceleration.magnitude.std(time)",
                           
                           "gravity.acceleration.magnitude.mean(time)",
                           "gravity.acceleration.magnitude.std(time)",
                           
                           "body.acceleration.jerk.magnitude.mean(time)",
                           "body.acceleration.jerk.magnitude.std(time)",
                           
                           "body.angular.velocity.magnitude.mean(time)",
                           "body.angular.velocity.magnitude.std(time)",
                           
                           "body.angular.jerk.magnitude.mean(time)",
                           "body.angular.jerk.magnitude.std(time)",
                           
                           "body.acceleration.mean.x(FFT)",
                           "body.acceleration.mean.y(FFT)",
                           "body.acceleration.mean.z(FFT)",
                           "body.acceleration.std.x(FFT)",
                           "body.acceleration.std.y(FFT)",
                           "body.acceleration.std.z(FFT)",
                           "body.acceleration.jerk.mean.x(FFT)",
                           "body.acceleration.jerk.mean.y(FFT)",
                           "body.acceleration.jerk.mean.z(FFT)",
                           "body.acceleration.jerk.std.x(FFT)",
                           "body.acceleration.jerk.std.y(FFT)",
                           "body.acceleration.jerk.std.z(FFT)",
                           
                           "body.angular.velocity.mean.x(FFT)",
                           "body.angular.velocity.mean.y(FFT)",
                           "body.angular.velocity.mean.z(FFT)",
                           "body.angular.velocity.std.x(FFT)",
                           "body.angular.velocity.std.y(FFT)",
                           "body.angular.velocity.std.z(FFT)",
                           
                           "body.acceleration.magnitude.mean(FFT)",
                           "body.acceleration.magnitude.std(FFT)",
                           
                           "body.acceleration.jerk.magnitude.mean(FFT)",
                           "body.acceleration.jerk.magnitude.std(FFT)",
                           
                           "body.angular.velocity.magnitude.mean(FFT)",
                           "body.angular.velocity.magnitude.std(FFT)",
                           
                           "body.angular.jerk.magnitude.mean(FFT)",
                           "body.angular.jerk.magnitude.std(FFT)",
                           "train/test"
                           )
colnames(data_set_mean_std)<-column_names

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data_set_by_group<-aggregate(data_set_mean_std, by=list(data_set_mean_std$subject,data_set_mean_std$activity.labels), mean)

# clean up extra columns

data_set_by_group_final<-data_set_by_group[,c(1:2,5:70)]
column_names<-colnames(data_set_by_group_final)
column_names[1]<-"subject"
column_names[2]<-"activity"
colnames(data_set_by_group_final)<-column_names

write.table(data_set_by_group_final, file="data_set_by_group_final.txt", row.names=FALSE)





