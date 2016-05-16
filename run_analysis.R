setwd("UCI HAR Dataset")
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
features<-read.table("features.txt")
train_X<-read.table("train/X_train.txt",sep="",col.names=features[,2])
train_Y<-read.table("train/y_train.txt",sep=" ",col.names="labels")
train_subject<-read.table("train/subject_train.txt",sep=" ",col.names="subject")
test_X<-read.table("test/X_test.txt",sep="",col.names=features[,2])
test_Y<-read.table("test/y_test.txt",sep=" ",col.names="labels")
test_subject<-read.table("test/subject_test.txt",sep=" ",col.names="subject")

#Merges the training and the test sets to create one data set.
dat1<-data.frame(train_subject,train_Y,train_X)
dat2<-data.frame(test_subject,test_Y,test_X)
dat<-rbind(dat1,dat2)

#Extracts only the measurements on the mean and standard deviation for each measurement.
dat<-dat[grep("subject|labels|mean|std",names(dat))]

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#second data is "t_mean"
subject<-c()
labels<-c()
for (i in unique(dat["subject"][,1])){
	for (k in unique(dat["labels"][,1])){	
		subject<-c(subject,i)
		labels<-c(labels,k)
	}
}
t_mean<-data.frame(subject,labels)
for (j in names(dat)[3:length(dat)]){
	t_mean_tmp<-c()
	for (i in unique(dat["subject"][,1])){
		for (k in unique(dat["labels"][,1])){	
			t_mean_tmp<-c(t_mean_tmp,mean(filter(dat,subject==i & labels==k)[,j]))
		}
	}
	t_mean<-data.frame(t_mean,t_mean_tmp)
}
names(t_mean)<-names(dat)
write.csv(t_mean,"out.csv")
