run_analysis<-function()
  #reads data from all the files
  #Reshapes the feature list
  features<-read.csv("UCI HAR Dataset/features.txt",header=FALSE,sep=" ")
   features<-as.list(as.character(features$V2))
  testLabels<-read.table("UCI HAR Dataset/test/subject_test.txt")
  names(testLabels)<-c("users")
testLabels$users<-as.numeric(as.character(testLabels$users))
trainLabels<-read.table("UCI HAR Dataset/train/subject_train.txt")
names(trainLabels)<-c("users")
trainLabels$users<-as.numeric(as.character(trainLabels$users))

testSet<-read.table("UCI HAR Dataset/test/X_test.txt")
#testSet$ID<-testLabels$users
traintSet<-read.table("UCI HAR Dataset/train/X_train.txt")
#traintSet$ID<-trainLabels$users

#renames the colums of the data
names(comp)<-c(features,"ID")
actTest<-read.table("UCI HAR Dataset/test/y_test.txt")
actTrain<-read.table("UCI HAR Dataset/train/y_train.txt")
names(actTrain)<-c("activity")
names(actTest)<-c("activity")

#Merges the tables together 2 by two
activity<-rbind(actTrain,actTest)
data<-rbind(testSet,traintSet)
users<-rbind(trainLabels,testLabels)

#Selects only relevant features
names(data)<-features
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[])
data<-data[,meanStdIndices]

#Rename activities
listAct<-read.csv("UCI HAR Dataset/activity_labels.txt",header=FALSE,sep=" ")
activity$activity<-as.numeric(activity$activity)
activity$activity<-listAct$V2[activity$activity]

#Join all tables
complete<-cbind(activity,data,users)
write.table(complete, "merged_data.txt")

nrSub <- length(table(users)) 
nrAct <- dim(listAct)[1] 
columnLen <- dim(complete)[2]

result <- matrix(NA, nrow=nrSub*nrAct, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(complete)

#create the new dataset with the means
nrSub <- length(table(users)) 
nrAct <- dim(listAct)[1] 
columnLen <- dim(complete)[2]

row <- 1
for(i in 1:nrSub) {
  for(j in 1:nrAct) {
    result[row, 1] <- sort(unique(users)[, 1])[i]
    result[row, 2] <- activity$activity[j]
    bool1 <- i == complete$users
    bool2 <- activity$activity[j] == complete$activity
    result[row, 3:columnLen] <- colMeans(complete[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
write.table(result, "final_data.txt")