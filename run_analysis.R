run_analysis <- function (){
        ## Input files:
        ##   X_test.txt, Y_test.txt, subject_test.txt, X_train.txt, Y_train.txt, subject_train.txt
        ##     "X_" files = 561-feature vector with time and frequency domain variables
        ##     "Y_" files =  activity type (values 1 ~ 6)
        ##     "subject_" files = subject id (values 1  30)
        ##   Features.txt = features whose measurements are captured in the "X_" files
        
        ## Step 1: Merge the "training" and "test" data sets into one data set
        
        ## Read the features.txt to get column names for the measurements
        
        features_tbl <- tbl_df(read.delim("features.txt", header = FALSE, sep=" "))
        
        ## Merge the "test" data sets (bind by column)
        
        X_test_tbl <- tbl_df(read.table("test/X_test.txt", header = FALSE, check.names=TRUE))
        colnames(X_test_tbl) <- features_tbl$V2
        
        subj_test_tbl <- tbl_df(read.delim("test/subject_test.txt", header = FALSE, sep=" "))
        colnames(subj_test_tbl) <- c("subject")
        
        Y_test_tbl <- tbl_df(read.delim("test/y_test.txt", header = FALSE, sep=" "))
        colnames(Y_test_tbl) <- c("activity")
        
        merged_test_set <- cbind(Y_test_tbl, subj_test_tbl, X_test_tbl)
        
        ## Merge the "train" data sets (bind by column)
        
        X_train_tbl <- tbl_df(read.table("train/X_train.txt", header = FALSE, check.names=TRUE))
        colnames(X_train_tbl) <- features_tbl$V2
        
        Y_train_tbl <- tbl_df(read.delim("train/y_train.txt", header = FALSE, sep=" "))
        colnames(Y_train_tbl) <- c("activity")
        
        subj_train_tbl <- tbl_df(read.delim("train/subject_train.txt", header = FALSE, sep=" "))
        colnames(subj_train_tbl) <- c("subject")
        
        merged_train_set <- cbind(Y_train_tbl, subj_train_tbl, X_train_tbl)
        
        ## Combine (merge) the "test" and "train" sets (bind by record)
        
        merged_set <- tbl_df(rbind(merged_test_set, merged_train_set))
        
        ## (End of Step 1)
        
        ## Step 4: Appropriately label the data set with descriptive variable names
        
        ## "Clean" the column names and ensure uniqueness 
        
        colnames(merged_set) <- make.names(colnames(merged_set), unique=TRUE)
        
        ## Step 2: Extract only the measurements on the mean and standard deviation for each measurement
       
        ## "Means" can be found by any measurement variable containing "mean"
        ## "Standard Deviation" ... by any measurement variable containing "std"
        
        act_sub_set <- select(merged_set, subject, activity)
        
        mean_set <- select(merged_set, contains("mean"))
        
        std_set <- select(merged_set, contains("std"))
        
        merged_set <- tbl_df(cbind(act_sub_set, mean_set, std_set))
        
        ## Step 3: Use descriptive activity names to name the activities in the data set
        
        ## Per the "activity_labels.txt"
        ##   1 WALKING
        ##   2 WALKING_UPSTAIRS
        ##   3 WALKING_DOWNSTAIRS
        ##   4 SITTING
        ##   5 STANDING
        ##   6 LAYING
        
        merged_set <- merged_set %>%
                mutate(activity = ifelse(activity == 1, "WALKING",
                                  ifelse(activity == 2, "WALKING_UPSTAIRS",
                                  ifelse(activity == 3, "WALKING_DOWNSTAIRS",
                                  ifelse(activity == 4, "SITTING",
                                  ifelse(activity == 5, "STANDING",
                                  ifelse(activity == 6, "LAYING", 0)))))))
                
        
        merged_set
        
        ## Step 5: From the data set in step 4, creates a second, independent tidy data set 
        ##         with the average of each variable for each activity and each subject.
        
        grouped_set <- group_by(merged_set, activity, subject)
        
        tidy_set <- summarize(grouped_set,                              
                              avg_tBodyAcc.mean...X = mean( tBodyAcc.mean...X ),
                              avg_tBodyAcc.mean...Y = mean( tBodyAcc.mean...Y ),
                              avg_tBodyAcc.mean...Z = mean( tBodyAcc.mean...Z ),
                              avg_tGravityAcc.mean...X = mean( tGravityAcc.mean...X ),
                              avg_tGravityAcc.mean...Y = mean( tGravityAcc.mean...Y ),
                              avg_tGravityAcc.mean...Z = mean( tGravityAcc.mean...Z ),
                              avg_tBodyAccJerk.mean...X = mean( tBodyAccJerk.mean...X ),
                              avg_tBodyAccJerk.mean...Y = mean( tBodyAccJerk.mean...Y ),
                              avg_tBodyAccJerk.mean...Z = mean( tBodyAccJerk.mean...Z ),
                              avg_tBodyGyro.mean...X = mean( tBodyGyro.mean...X ),
                              avg_tBodyGyro.mean...Y = mean( tBodyGyro.mean...Y ),
                              avg_tBodyGyro.mean...Z = mean( tBodyGyro.mean...Z ),
                              avg_tBodyGyroJerk.mean...X = mean( tBodyGyroJerk.mean...X ),
                              avg_tBodyGyroJerk.mean...Y = mean( tBodyGyroJerk.mean...Y ),
                              avg_tBodyGyroJerk.mean...Z = mean( tBodyGyroJerk.mean...Z ),
                              avg_tBodyAccMag.mean.. = mean( tBodyAccMag.mean.. ),
                              avg_tGravityAccMag.mean.. = mean( tGravityAccMag.mean.. ),
                              avg_tBodyAccJerkMag.mean.. = mean( tBodyAccJerkMag.mean.. ),
                              avg_tBodyGyroMag.mean.. = mean( tBodyGyroMag.mean.. ),
                              avg_tBodyGyroJerkMag.mean.. = mean( tBodyGyroJerkMag.mean.. ),
                              avg_fBodyAcc.mean...X = mean( fBodyAcc.mean...X ),
                              avg_fBodyAcc.mean...Y = mean( fBodyAcc.mean...Y ),
                              avg_fBodyAcc.mean...Z = mean( fBodyAcc.mean...Z ),
                              avg_fBodyAcc.meanFreq...X = mean( fBodyAcc.meanFreq...X ),
                              avg_fBodyAcc.meanFreq...Y = mean( fBodyAcc.meanFreq...Y ),
                              avg_fBodyAcc.meanFreq...Z = mean( fBodyAcc.meanFreq...Z ),
                              avg_fBodyAccJerk.mean...X = mean( fBodyAccJerk.mean...X ),
                              avg_fBodyAccJerk.mean...Y = mean( fBodyAccJerk.mean...Y ),
                              avg_fBodyAccJerk.mean...Z = mean( fBodyAccJerk.mean...Z ),
                              avg_fBodyAccJerk.meanFreq...X = mean( fBodyAccJerk.meanFreq...X ),
                              avg_fBodyAccJerk.meanFreq...Y = mean( fBodyAccJerk.meanFreq...Y ),
                              avg_fBodyAccJerk.meanFreq...Z = mean( fBodyAccJerk.meanFreq...Z ),
                              avg_fBodyGyro.mean...X = mean( fBodyGyro.mean...X ),
                              avg_fBodyGyro.mean...Y = mean( fBodyGyro.mean...Y ),
                              avg_fBodyGyro.mean...Z = mean( fBodyGyro.mean...Z ),
                              avg_fBodyGyro.meanFreq...X = mean( fBodyGyro.meanFreq...X ),
                              avg_fBodyGyro.meanFreq...Y = mean( fBodyGyro.meanFreq...Y ),
                              avg_fBodyGyro.meanFreq...Z = mean( fBodyGyro.meanFreq...Z ),
                              avg_fBodyAccMag.mean.. = mean( fBodyAccMag.mean.. ),
                              avg_fBodyAccMag.meanFreq.. = mean( fBodyAccMag.meanFreq.. ),
                              avg_fBodyBodyAccJerkMag.mean.. = mean( fBodyBodyAccJerkMag.mean.. ),
                              avg_fBodyBodyAccJerkMag.meanFreq.. = mean( fBodyBodyAccJerkMag.meanFreq.. ),
                              avg_fBodyBodyGyroMag.mean.. = mean( fBodyBodyGyroMag.mean.. ),
                              avg_fBodyBodyGyroMag.meanFreq.. = mean( fBodyBodyGyroMag.meanFreq.. ),
                              avg_fBodyBodyGyroJerkMag.mean.. = mean( fBodyBodyGyroJerkMag.mean.. ),
                              avg_fBodyBodyGyroJerkMag.meanFreq.. = mean( fBodyBodyGyroJerkMag.meanFreq.. ),
                              avg_angle.tBodyAccMean.gravity. = mean( angle.tBodyAccMean.gravity. ),
                              avg_angle.tBodyAccJerkMean..gravityMean. = mean( angle.tBodyAccJerkMean..gravityMean. ),
                              avg_angle.tBodyGyroMean.gravityMean. = mean( angle.tBodyGyroMean.gravityMean. ),
                              avg_angle.tBodyGyroJerkMean.gravityMean. = mean( angle.tBodyGyroJerkMean.gravityMean. ),
                              avg_angle.X.gravityMean. = mean( angle.X.gravityMean. ),
                              avg_angle.Y.gravityMean. = mean( angle.Y.gravityMean. ),
                              avg_angle.Z.gravityMean. = mean( angle.Z.gravityMean. ),
                              avg_tBodyAcc.std...X = mean( tBodyAcc.std...X ),
                              avg_tBodyAcc.std...Y = mean( tBodyAcc.std...Y ),
                              avg_tBodyAcc.std...Z = mean( tBodyAcc.std...Z ),
                              avg_tGravityAcc.std...X = mean( tGravityAcc.std...X ),
                              avg_tGravityAcc.std...Y = mean( tGravityAcc.std...Y ),
                              avg_tGravityAcc.std...Z = mean( tGravityAcc.std...Z ),
                              avg_tBodyAccJerk.std...X = mean( tBodyAccJerk.std...X ),
                              avg_tBodyAccJerk.std...Y = mean( tBodyAccJerk.std...Y ),
                              avg_tBodyAccJerk.std...Z = mean( tBodyAccJerk.std...Z ),
                              avg_tBodyGyro.std...X = mean( tBodyGyro.std...X ),
                              avg_tBodyGyro.std...Y = mean( tBodyGyro.std...Y ),
                              avg_tBodyGyro.std...Z = mean( tBodyGyro.std...Z ),
                              avg_tBodyGyroJerk.std...X = mean( tBodyGyroJerk.std...X ),
                              avg_tBodyGyroJerk.std...Y = mean( tBodyGyroJerk.std...Y ),
                              avg_tBodyGyroJerk.std...Z = mean( tBodyGyroJerk.std...Z ),
                              avg_tBodyAccMag.std.. = mean( tBodyAccMag.std.. ),
                              avg_tGravityAccMag.std.. = mean( tGravityAccMag.std.. ),
                              avg_tBodyAccJerkMag.std.. = mean( tBodyAccJerkMag.std.. ),
                              avg_tBodyGyroMag.std.. = mean( tBodyGyroMag.std.. ),
                              avg_tBodyGyroJerkMag.std.. = mean( tBodyGyroJerkMag.std.. ),
                              avg_fBodyAcc.std...X = mean( fBodyAcc.std...X ),
                              avg_fBodyAcc.std...Y = mean( fBodyAcc.std...Y ),
                              avg_fBodyAcc.std...Z = mean( fBodyAcc.std...Z ),
                              avg_fBodyAccJerk.std...X = mean( fBodyAccJerk.std...X ),
                              avg_fBodyAccJerk.std...Y = mean( fBodyAccJerk.std...Y ),
                              avg_fBodyAccJerk.std...Z = mean( fBodyAccJerk.std...Z ),
                              avg_fBodyGyro.std...X = mean( fBodyGyro.std...X ),
                              avg_fBodyGyro.std...Y = mean( fBodyGyro.std...Y ),
                              avg_fBodyGyro.std...Z = mean( fBodyGyro.std...Z ),
                              avg_fBodyAccMag.std.. = mean( fBodyAccMag.std.. ),
                              avg_fBodyBodyAccJerkMag.std.. = mean( fBodyBodyAccJerkMag.std.. ),
                              avg_fBodyBodyGyroMag.std.. = mean( fBodyBodyGyroMag.std.. ),
                              avg_fBodyBodyGyroJerkMag.std.. = mean( fBodyBodyGyroJerkMag.std.. ))
                              
        ## Return the tidy set
        tidy_set
        
}