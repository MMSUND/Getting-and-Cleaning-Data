#1.Merges the training and the test sets to create one data set.

x_train <- read.table("UCI HAR Dataset/train/X_train.txt",  header = FALSE)
x_test<-read.table("UCI HAR Dataset/test/X_test.txt",  header = FALSE)
head(x_train)
head(x_test)
x_merge <- rbind(x_train, x_test) 

y_train<-read.table("UCI HAR Dataset/train/y_train.txt",  header = FALSE)
y_test<-read.table("UCI HAR Dataset/test/y_test.txt",  header = FALSE)
head(y_train)
head(y_test)
y_merge <- rbind(y_train, y_test)


subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt",  header = FALSE)
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",  header = FALSE)
subject_merge <- rbind(subject_train, subject_test)

names(x_merge)
names(y_merge)

colnames(x_merge)
colnames(y_merge)
colnames(subject_merge)

features <- read.table("UCI HAR Dataset\\features.txt", header = FALSE)
names(x_merge)<-features[,2]

names(y_merge)<-"Activity"
names(subject_merge)<-"Subject"

completedata_merge<-cbind(x_merge,y_merge,subject_merge)
completedata_merge       

##2.Extracts only the measurements on the mean and standard deviation for each measurement

q<-grep(".*Mean.*|.*Std.*", colnames(completedata_merge), ignore.case=TRUE) 
q
t<-completedata_merge[,c(q,562,563)]
t
names(t) 
head(t)


###3.Uses descriptive activity names to name the activities in the data set

Activity <- read.table("UCI HAR Dataset/activity_labels.txt",  header = FALSE)
Activity

t$Activity<-as.character(t$Activity)
for (i in 1:6){
        t$Activity[t$Activity==i]<-as.character(Activity[i,2])
} 

head(t)


####4. Appropriately labels the data set with descriptive variable names. 

names(t)<-gsub("Acc", "Accelerometer", names(t))
names(t)<-gsub("Gyro", "Gyrometer", names(t))
names(t)<-gsub("-mean()", "Mean", names(t))
names(t)<-gsub("-std()", "STD", names(t))
names(t)<-gsub("-freq()", "Frequency", names(t))



t$Activity<-as.factor(t$Activity)
t$Subject<-as.factor(t$Subject)



colnames(t)
names(t)<-gsub("-mean()", "Mean", names(t), ignore.case = TRUE)
names(t)<-gsub("-std()", "STD", names(t), ignore.case = TRUE)
names(t)<-gsub("()", "", names(t)) 


#####5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

t$Activity <- as.factor(t$Activity)
t$Subject <- as.factor(t$Subject)
new_tidy_data <- aggregate(. ~ t$Activity + t$Subject, data = t, mean)

#Write into file
write.table(new_tidy_data, file = "tidy_data_set_1.txt", sep = ",", row.name=FALSE, col.names = colnames(new_tidy_data), qmethod = "double")

new_tidy_data



