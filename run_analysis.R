#Read raw data
test_data <- read.table("test/X_test.txt")
train_data <- read.table("train/X_train.txt")
user_test <- read.table("test/subject_test.txt")
user_train <- read.table("train/subject_train.txt")
activity_train <- read.table("train/y_train.txt")
activity_test <- read.table("test/y_test.txt")
names <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

#Set column names
colnames(test_data) <- names[ ,2]
colnames(train_data) <- names[ ,2]
colnames(activity_test) <- "Activity_ID"
colnames(activity_train) <- "Activity_ID"
colnames(user_train) <- "User_ID"
colnames(user_test) <- "User_ID"
colnames(activities) <- c("id", "Activity")

#Combine and merge data into the single data set
test_full <- cbind(user_test, activity_test, test_data)
train_full <- cbind(user_train, activity_train, train_data)
full_data <- rbind(test_full, train_full)

#Extract the measurements on the mean and standard deviation
cols_MeanStd <- grep(".*Mean.*|.*Std.*|.*User.*|.*Activity.*", names(full_data), ignore.case=TRUE)
new_data <- full_data[, cols_MeanStd]

#Apply descriptive activity names to name the activities
new_data <- merge(new_data, activities, by.x = "Activity_ID", by.y = "id", all.x = TRUE)
new_data <- new_data[order(new_data$User_ID), ]
new_data <- new_data[c(2, 89, 3:88)]

#Appropriately label the data with descriptive variable names
names(new_data)<-gsub("_ID", "", names(new_data))
names(new_data)<-gsub("Acc", "Accelerometer", names(new_data))
names(new_data)<-gsub("Gyro", "Gyroscope", names(new_data))
names(new_data)<-gsub("BodyBody", "Body", names(new_data))
names(new_data)<-gsub("Mag", "Magnitude", names(new_data))
names(new_data)<-gsub("^t", "Time", names(new_data))
names(new_data)<-gsub("^f", "Frequency", names(new_data))
names(new_data)<-gsub("tBody", "TimeBody", names(new_data))
names(new_data)<-gsub("-mean()", "Mean", names(new_data))
names(new_data)<-gsub("-std()", "STD", names(new_data))
names(new_data)<-gsub("-freq()", "Frequency", names(new_data))
names(new_data)<-gsub("angle", "Angle", names(new_data))
names(new_data)<-gsub("gravity", "Gravity", names(new_data))
names(new_data)<-gsub("\\()", "", names(new_data))

#Create a data set with the average of each variable for each activity and each subject
library(plyr)
tidy_data <- ddply(new_data, .(User, Activity), numcolwise(mean))

#Write data set into a file
write.table(tidy_data, "tidy_data.txt", sep="\t", row.names = FALSE)
