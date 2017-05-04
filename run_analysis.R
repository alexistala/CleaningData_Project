library(readr)
library(data.table)
library(dplyr)


activity_labels <- read_delim("./activity_labels.txt", 
                              " ", escape_double = FALSE, col_names = FALSE, 
                              trim_ws = TRUE)
features <- read_delim("./Users/features.txt", 
                       " ", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
subject_train <- read_delim("./train/subject_train.txt", 
                            " ", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
y_train <- read_delim("./train/y_train.txt", 
                     " ", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
X_train <- read_delim("./train/X_train.txt", 
                      ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)
subject_test <- read_delim("./test/subject_test.txt", 
                            " ", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
y_test <- read_delim("./test/y_test.txt", 
                      " ", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)
X_test <- read_delim("./test/X_test.txt", 
                      ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)




names(activity_labels)<-c("Level","Activity")
names(features)<-c("FeatureID","Feature name")
names(y_train)<-"Activity"
y_train$Activity<-factor(y_train$Activity, levels = activity_labels$Level ,labels=activity_labels$Activity)
names(y_test)<-"Activity"
y_test$Activity<-factor(y_test$Activity, levels = activity_labels$Level ,labels=activity_labels$Activity)
names(subject_test)<-"Subject"
names(subject_train)<-"Subject"
Train.Data<-cbind(subject_train,y_train,X_train)
Test.Data<-cbind(subject_test,y_test,X_test)



names(X_train)<-features$`Feature name`
names(X_test)<-features$`Feature name`
X_train$set<-"Train"
X_test$set<-"Test"
X<-rbind(X_train,X_test)
X<-X[c(562,1:561)]

y<-rbind(y_train,y_test)

subject<-rbind(subject_train,subject_test)

Data <- cbind(subject,y,X)
Data <- Data[,c(3,1,2,4:564)]

means<-NULL; stddev<-NULL
for (i in 1:562) means[i]=grepl("mean",names(Data)[i])
for (i in 1:562) stddev[i]=grepl("std",names(Data)[i])
meansid<-which(means)
stddevid<-which(stddev)
Data.Means<-Data[,c(1:3,meansid)]
Data.StdDev<-Data[,c(1:3,stddevid)]
Data.MeanSd<-Data[,c(1:3,meansid,stddevid)]

Data.Aggr<-aggregate(x=Data.MeanSd, 
                     by = list(Data.MeanSd$Subject,Data.MeanSd$Activity), 
                     FUN="mean")
Data.Aggr <- Data.Aggr[,c(4,2,6:84)]
names(Data.Aggr)[2]<-"Activity"

write.table(Data.Aggr,file="Aggregated_Data.txt",sep=",", row.names=FALSE)