plot4 <- function() {
	#reads the text file in the directory and limits it to
	#2007-02-01 and 2007-02-02
	data1 <- read.table("household_power_consumption.txt",sep=";");
	names(data1) <- c("Date","Time","Global Active Power","Global Reactive Power","Voltage","Global Intensity","Sub Metering 1","Sub Metering 2","Sub Metering 3");
	data <- rbind(data1[data1$Date == "1/2/2007",],data1[data1$Date == "2/2/2007",]);
	options(warn=-1) #turn off warnings to suppress statements

	#produces the plot4 png file.
	#Could not find how to change time axis labels into weekdays.
	png(filename="plot4.png",width = 480, height=480,units="px");
	par(mfrow=c(2,2));
	
	globalpower <- as.numeric(data$"Global Active Power")/500;
	global.ts <- ts(globalpower,start=c(0,1))
	plot.ts(global.ts,xlab="",ylab="Global Active Power (kilowatts)")
	
	volts <- as.numeric(data$"Voltage");
	volts.ts <- ts(volts,start=c(0,1));
	plot.ts(volts.ts,xlab="datetime",ylab="Voltage");
		
	sub1 <- as.numeric(data$"Sub Metering 1");
	sub2 <- as.numeric(data$"Sub Metering 2");
	sub3 <- as.numeric(data$"Sub Metering 3");
	sub1.ts <- ts(sub1,start=c(0,1));
	sub2.ts <- ts(sub2,start=c(0,1));
	sub3.ts <- ts(sub3,start=c(0,1));
	
	plot.ts(sub1.ts,xlab="",ylab="Global Active Power (kilowatts)",col="black")
	lines(sub2.ts,col="red")
	lines(sub3.ts,col="blue")
	legend("topright",col=c("black","red","blue"),legend=c("Sub Metering 1","Sub Metering 2","Sub Metering 3"),lty=1)
	
	reactivepower <- as.numeric(data$"Global Reactive Power");
	reactive.ts <- ts(reactivepower,start=c(0,1))
	plot.ts(reactive.ts,xlab="datetime",ylab="Global Reactive Power")
	
	dev.off();

	#reproduces the graph
	quartz();
	par(mfrow=c(2,2));
	
	globalpower <- as.numeric(data$"Global Active Power")/500;
	global.ts <- ts(globalpower,start=c(0,1))
	plot.ts(global.ts,xlab="",ylab="Global Active Power (kilowatts)")
	
	volts <- as.numeric(data$"Voltage");
	volts.ts <- ts(volts,start=c(0,1));
	plot.ts(volts.ts,xlab="datetime",ylab="Voltage");
		
	sub1 <- as.numeric(data$"Sub Metering 1");
	sub2 <- as.numeric(data$"Sub Metering 2");
	sub3 <- as.numeric(data$"Sub Metering 3");
	sub1.ts <- ts(sub1,start=c(0,1));
	sub2.ts <- ts(sub2,start=c(0,1));
	sub3.ts <- ts(sub3,start=c(0,1));
	
	plot.ts(sub1.ts,xlab="",ylab="Global Active Power (kilowatts)",col="black")
	lines(sub2.ts,col="red")
	lines(sub3.ts,col="blue")
	legend("topright",col=c("black","red","blue"),legend=c("Sub Metering 1","Sub Metering 2","Sub Metering 3"),lty=1)
	
	reactivepower <- as.numeric(data$"Global Reactive Power");
	reactive.ts <- ts(reactivepower,start=c(0,1))
	plot.ts(reactive.ts,xlab="datetime",ylab="Global Reactive Power")
	
}