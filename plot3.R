plot3 <- function() {
	#reads the text file in the directory and limits it to
	#2007-02-01 and 2007-02-02
	data1 <- read.table("household_power_consumption.txt",sep=";");
	names(data1) <- c("Date","Time","Global Active Power","Global Reactive Power","Voltage","Global Intensity","Sub Metering 1","Sub Metering 2","Sub Metering 3");
	data <- rbind(data1[data1$Date == "1/2/2007",],data1[data1$Date == "2/2/2007",]);
	options(warn=-1) #turn off warnings to suppress statements

	#produces the plot3 png file.
	#Could not find how to change time axis labels into weekdays.
	sub1 <- as.numeric(data$"Sub Metering 1");
	sub2 <- as.numeric(data$"Sub Metering 2");
	sub3 <- as.numeric(data$"Sub Metering 3");
	sub1.ts <- ts(sub1,start=c(0,1));
	sub2.ts <- ts(sub2,start=c(0,1));
	sub3.ts <- ts(sub3,start=c(0,1));
	
	png(filename="plot3.png",width = 480, height=480,units="px");
	plot.ts(sub1.ts,xlab="",ylab="Energy sub metering",col="black");
	lines(sub2.ts,col="red");
	lines(sub3.ts,col="blue");
	legend("topright",col=c("black","red","blue"),legend=c("Sub Metering 1","Sub Metering 2","Sub Metering 3"),lty=1);
	dev.off();
	
	#reproduces the graph
	quartz();	
	plot.ts(sub1.ts,xlab="",ylab="Energy sub metering",col="black");
	lines(sub2.ts,col="red");
	lines(sub3.ts,col="blue");
	legend("topright",col=c("black","red","blue"),legend=c("Sub Metering 1","Sub Metering 2","Sub Metering 3"),lty=1);
}