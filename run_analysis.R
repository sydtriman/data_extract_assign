## Peer Assignment 
## ===========================================================================

## ---------------------------------------------------------------------------
## Purpose
## =======
## The purpose of this function is firstly to:
## a) to merger two sets of data (training and test)
## b) extract only the mean and standard deviation measurements
## c) provide description names and variables
## The function then uses the data set created above and calculates the
## average for each activity and subject
##
## This function makes sure of several additional packages which are loaded
## as a part of the function
## ---------------------------------------------------------------------------

## load package for join
require(plyr)

## set the various file names with locations
## f_ denotes that it is a filename
dir <- "UCI\ HAR\ Dataset"
f_activity_labels <- paste(dir, "/activity_labels.txt", sep="")
f_features <- paste(dir, "/features.txt", sep="")
f_subject_test <- paste(dir, "/test/subject_test.txt", sep="")
f_x_test <- paste(dir, "/test/x_test.txt", sep="")
f_y_test <- paste(dir, "/test/y_test.txt", sep="")
f_subject_train <- paste(dir, "/train/subject_train.txt", sep="")
f_x_train <- paste(dir, "/train/x_train.txt", sep="")
f_y_train <- paste(dir, "/train/y_train.txt", sep="")

## load the files into R
## d_ denotes data
d_activity_labels <- read.table(f_activity_labels, sep=" ", colClasses = c("character"), col.names = c("ActivityID","Activity"))
d_features <- read.table(f_features, sep=" ", colClasses=c("character"), col.names=c("FeatureID", "Feature")) 
d_subject_test <- read.table(f_subject_test, col.names=c("SubjectID"))
d_x_test <- read.table(f_x_test)
d_y_test <- read.table(f_y_test, col.names = c("ActivityID"))
d_subject_train <- read.table(f_subject_train, col.names=c("SubjectID"))
d_x_train <- read.table(f_x_train) 
d_y_train <- read.table(f_y_train, col.names = c("ActivityID"))

## join the test and training data together with cbind to form 2 dataframes
train_data <- cbind(d_subject_train, d_x_train)
train_data <- cbind(train_data, d_y_train)
test_data <- cbind(d_subject_test, d_x_test)
test_data <- cbind(test_data, d_y_test)

## join the two dataframes created above using rbind
sensor_data <- rbind(train_data, test_data)

# get df of just features (not IDs)
feature_list <- d_features[,2]
col_headings <- c("SubjectID",feature_list,"ActivityID")

## add names to variables
names(sensor_data) <- col_headings

## determien which variables have mean/std dv
## note: must include ActivityID and SubjectID otherwise they will be stripped out
search_pattern <- "mean|std|ActivityID|SubjectID"
mean_sd_cols <- grepl(search_pattern, col_headings) ## testing only

## extract only those variables with mean or std into new set
sensor_data_mean_std_only <- sensor_data[,grepl(search_pattern, col_headings)]

## add activity labels to data subset using join
sensor_data_mean_std_only_labelled <- join(sensor_data_mean_std_only, d_activity_labels, by="ActivityID")

## create new data set with average of for activity and subject
sensor_data_mean_grouped = ddply(sensor_data_mean_std_only_labelled, c("SubjectID","Activity"), numcolwise(mean))
write.table(sensor_data_mean_grouped, file="sensor_data_mean_grouped.txt", row.name=FALSE)


