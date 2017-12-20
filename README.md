# Get-and-Clean-Data-Final

This repo contains run_analysis.R file that forms and saves a tidy data set named data_set_by_group_final by performing the following operatios:

1) Downloads and unzips the original data files in the 'UCI HAR Dataset' subfolder of the original working directory
2) Merges the training and the test sets to create one data set.
3) Extracts only the measurements on the mean and standard deviation for each measurement.
4) Uses descriptive activity names to name the activities in the data set
5) Appropriately labels the data set with descriptive variable names.
6) From the data set in step 5, creates a second, independent tidy data set with the average of each variable for each activity and each subject. This file is saved in the current working foder as data_set_by_group_final.txt
