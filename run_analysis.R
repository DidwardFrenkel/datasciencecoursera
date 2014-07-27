run_analysis <- function() {
	library(reshape2)
	#sets the working directory for the labels
	setwd("/Users/danielzhou/Documents/Mass\ Prog/R\ Prog/Getting\ &\ Cleaning\ Data/UCI\ HAR\ Dataset")
	#read the features and labels
	rawFeatures <- read.table("features.txt",sep=" ")
	features <- as.character(rawFeatures[,2])
	labels <- read.table("activity_labels.txt",sep=" ")
	
	
	#sets the working directory for the training set and reads
	#the data
	setwd("/Users/danielzhou/Documents/Mass\ Prog/R\ Prog/Getting\ &\ Cleaning\ Data/UCI\ HAR\ Dataset/train")
	xTrainOrig <- read.table("X_train.txt",sep=" ",fill=T)
	xTrain <- NULL
	xTrain <- newTable(newlist=xTrain,oldlist=xTrainOrig)
	yTrain <- unlist(read.table("y_train.txt",sep=" "))
	subjectTrain <- unlist(read.table("subject_train.txt",sep=" "))
	trainTotal <- cbind(xTrain,yTrain,subjectTrain)

	#sets the working directory for the test set and reads
	#the data
	setwd("/Users/danielzhou/Documents/Mass\ Prog/R\ Prog/Getting\ &\ Cleaning\ Data/UCI\ HAR\ Dataset/test")
	xTestOrig <- read.table("X_test.txt",sep=" ",fill=T)
	xTest <- NULL
	xTest <- newTable(newlist=xTest,oldlist=xTestOrig)
	yTest <- unlist(read.table("y_test.txt",sep=" "))
	subjectTest <- unlist(read.table("subject_test.txt",sep=" "))
	testTotal <- cbind(xTest,yTest,subjectTest)

	fullData <- rbind(trainTotal,testTotal)
	names(fullData) <- c(features,"Activity Number","Subject")
	fullData$Activity <- as.character(labels[fullData[,562],2])
	modData <- fullData[,grepl("mean()",names(fullData),fixed=T) | grepl("std()",names(fullData),fixed=T) | grepl("Activity",names(fullData),fixed=T) | grepl("Subject",names(fullData),fixed=T) ]
	modFinal <- tapply(modData[,grepl("mean()",names(fullData),fixed=T) | grepl("std()",names(fullData),fixed=T)],modData[,grepl("Activity",names(fullData),fixed=T) | grepl("Subject",names(fullData),fixed=T)],mean)
	jpg(modFinal)
}

newTable <- function(newlist,oldlist,featureNum=561) {
	#filters the NA's created by extra spaces in the data
	tempRow <- 0
	for (rowNum in 1:nrow(oldlist)) {
		#accounts for data that might have been split in the middle by newlines, merging them
		#and skipping the line after. Assumes only one newline split, which is observed in this
		#particular data set.
		
		if(length(as.numeric(oldlist[rowNum,!is.na(oldlist[rowNum,])]))<featureNum && tempRow != rowNum) {
			#case number of rows falls short
			tempRow <- rowNum +1
			newVector <- c(as.numeric(oldlist[rowNum,!is.na(oldlist[rowNum,])]),as.numeric(oldlist[tempRow,!is.na(oldlist[tempRow,])]))
			newlist <- rbind(newlist,newVector)
		} else if (rowNum == tempRow) {
			#skip the row
			tempRow <- 0
		} else {
			#normal case
			newlist <- rbind(newlist,as.numeric(oldlist[rowNum,!is.na(oldlist[rowNum,])]))
		}
	}
	newlist <- data.frame(newlist)
}
