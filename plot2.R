plot2 <- function() {
	#reads the text file in the directory and limits it to
	#2007-02-01 and 2007-02-02
	data1 <- read.table("household_power_consumption.txt",sep=";");
	names(data1) <- c("Date","Time","Global Active Power","Global Reactive Power","Voltage","Global Intensity","Sub Metering 1","Sub Metering 2","Sub Metering 3");
	data <- rbind(data1[data1$Date == "1/2/2007",],data1[data1$Date == "2/2/2007",]);
	options(warn=-1) #turn off warnings to suppress statements

	#produces the plot2 png file.
	#Could not find how to change time axis labels into weekdays.
	png(filename="plot2.png",width = 480, height=480,units="px");
	globalpower <- as.numeric(data$"Global Active Power")/500;
	global.ts <- ts(globalpower,start=c(0,1))
	plot.ts(global.ts,xlab="",ylab="Global Active Power (kilowatts)")
	
	dev.off();
	
	#reproduces the graph
	quartz();
	plot.ts(global.ts,xlab="",ylab="Global Active Power (kilowatts)")
}