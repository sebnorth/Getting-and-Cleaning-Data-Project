# setting working directory
setwd("~/R/Getting and Cleaning Data/pa1")

# using shell commands:
# grep 'mean()' features.txt > features_mean_and_std
# grep 'std()' features.txt >> features_mean_and_std
# I created 'features_mean_and_std' file with names of variables of the measurement's mean and standard deviation.
# There are 561 variables and in 'features_mean_and_std' file there are two columns:
# 1st column: number of the variable, 2nd column: the name of the variable

features = read.csv("./UCI HAR Dataset/features_mean_and_std", sep = " ", header = FALSE)
nrow(features) # there are 66 variables
features_indices <- features[,1]
features_names <- features[,2]

# using shell commands: (to find number of rows, of course, the read.csv knows this number but i manipulated 
# this number to perform my analysis on small datasets)
# sebnorth@seba:~/R/Getting and Cleaning Data/pa1/UCI HAR Dataset/train$ wc *
# wc: Inertial Signals: Jest katalogiem
#       0        0        0 Inertial Signals
#    7352     7352    20152 subject_train.txt
#    7352  4124472 66006256 X_train.txt
#    7352     7352    14704 y_train.txt
#   22056  4139176 66041112 razem
#
#sebnorth@seba:~/R/Getting and Cleaning Data/pa1/UCI HAR Dataset/test$ wc *
#wc: Inertial Signals: Jest katalogiem
#       0        0        0 Inertial Signals
#    2947     2947     7934 subject_test.txt
#    2947  1653267 26458166 X_test.txt
#    2947     2947     5894 y_test.txt
#    8841  1659161 26471994 razem
#

N = 7352

# X_train is a data frame collecting measurements from X_train.txt
X_train<- read.csv("./UCI HAR Dataset/train/X_train.txt", sep = " ", header = FALSE, nrows = N)
# X_train_sub is a data frame collecting measurements features only to features_names variables
X_train_sub <- subset(X_train, select = features_indices)
# I use the the features_names to create a header
names(X_train_sub) <- features_names
# I need two columns, one with subjects and one with activity, these are numbers
x_train_subject_number_df = read.csv("./UCI HAR Dataset/train/subject_train.txt", sep = " ", header = FALSE, nrows = N)
y_train_activity_number_df = read.csv("./UCI HAR Dataset/train/y_train.txt", sep = " ", header = FALSE, nrows = N)
# head(x_train_subject_number_df)
# head(y_train_activity_number_df)
# I use "subject" and "activity_number" names to create a header
names(x_train_subject_number_df) <- "subject"
names(y_train_activity_number_df) <- "activity_number"
# I merge these two new variables with X_train_sub
X_train_sub = cbind(x_train_subject_number_df, y_train_activity_number_df,  X_train_sub)
# I create 'activity_code' variable to revalue 'activity_number' variable 
activity_labels<- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE, nrows = 6)
activity_labels
oldvalues <- activity_labels[,1]
newvalues <- activity_labels[,2]
X_train_sub$activity_code <- newvalues[ match(X_train_sub$activity_number, oldvalues) ]
# I create 'subject_code' variable to revalue 'subject' variable
oldvalues <- 1:30
newvalues <- paste("object ",1:30, sep="")
X_train_sub$subject_code <- newvalues[ match(X_train_sub$subject, oldvalues) ]
# I rearrange data frame's columns
X_train_sub <- X_train_sub[,c(70,69,3:68,1,2)]
head(X_train_sub)

## The same for 'test' data set
N = 2947
X_test<- read.csv("./UCI HAR Dataset/test/X_test.txt", sep = " ", header = FALSE, nrows = N)
X_test_sub <- subset(X_test, select = features_indices)
names(X_test_sub) <- features_names
x_test_subject_number_df = read.csv("./UCI HAR Dataset/test/subject_test.txt", sep = " ", header = FALSE, nrows = N)
y_test_activity_number_df = read.csv("./UCI HAR Dataset/test/y_test.txt", sep = " ", header = FALSE, nrows = N)
names(x_test_subject_number_df) <- "subject"
names(y_test_activity_number_df) <- "activity_number"
X_test_sub = cbind(x_test_subject_number_df, y_test_activity_number_df,  X_test_sub)
activity_labels<- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE, nrows = 6)
activity_labels
oldvalues <- activity_labels[,1]
newvalues <- activity_labels[,2]
X_test_sub$activity_code <- newvalues[ match(X_test_sub$activity_number, oldvalues) ]
oldvalues <- 1:30
newvalues <- paste("object ",1:30, sep="")
X_test_sub$subject_code <- newvalues[ match(X_test_sub$subject, oldvalues) ]
X_test_sub <- X_test_sub[,c(70,69,3:68,1,2)]
head(X_test_sub)

# Now I merge both sets
X_merge <- rbind(X_train_sub, X_test_sub)
# and save the result
write.csv(X_merge, "data.csv", row.names=FALSE)

# The last part is to "Create a second, independent tidy data set with the average
# of each variable for each activity and each subject. "

data_split <- split(X_merge[,3:68], list(X_merge$activity_code, X_merge$subject_code))
newdata <- sapply(data_split, colMeans, na.rm = TRUE)

# save the result to 'data2.csv' file
write.csv(as.data.frame(newdata, header = TRUE, row.names = names(X_merge[,3:68])), "data2.csv", row.names=TRUE)
