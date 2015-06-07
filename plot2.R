CreatePlot2 <- function()
{
	#Open the CSV file and read the whole thing into memory
	EnergyUsage <- read.csv('household_power_consumption.txt', sep=';', na.strings='?')

	#Convert Column1 and Column2 into a new Date column that we can do ranges on.
	EnergyUsage[2] <- mapply(
	      function(date, time) strptime(paste(date, time), format="%d/%m/%Y %H:%M:%S"), 
	      EnergyUsage[1], 
	      EnergyUsage[2])
	EnergyUsage <- EnergyUsage[2:9]

	#Specify starting and ending ranges
	StartTime <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
	EndTime  <- strptime("2007-02-03 00:00:00", format="%Y-%m-%d %H:%M:%S")

	#Scrape the data that we're interestd in
	PlotData <- EnergyUsage[EnergyUsage$Time >= StartTime & EnergyUsage$Time < EndTime,]

	#Begin our plot
	png(filename="plot2.png", width=480, height=480)

	with(PlotData, plot(Time, 
						Global_active_power, 
						type="l", 
						ylab="Global Active Power (kilowatts)",
						xlab=""))

	dev.off()
}

CreatePlot2()