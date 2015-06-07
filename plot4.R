CreatePlot4 <- function()
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
    png(filename="plot4.png", width=480, height=480)

    par(mfrow=c(2,2))

    #Add Plot1
    with(PlotData, plot(Time, 
                        Global_active_power, 
                        type="l", 
                        ylab="Global Active Power",
                        xlab=""))

    #Add Plot2
    with(PlotData, plot(Time, 
                        Voltage, 
                        type="l", 
                        xlab="datetime"
                        ))


    #Add Plot3
    with(PlotData, plot(Time, 
                        Sub_metering_1, 
                        type="l", 
                        ylab="Energy sub metering",
                        xlab=""))
    with(PlotData, points(Time, 
                        Sub_metering_2, 
                        type="l", 
                        col="red",
                        xlab=""))
    with(PlotData, points(Time, 
                        Sub_metering_3, 
                        type="l", 
                        col="blue",
                        xlab=""))
    legend("topright", 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           col = c("black", "red", "blue"), 
           lty=c(1,1,1),
           bty="n")


    #Add Plot4
    with(PlotData, plot(Time, 
                        Global_reactive_power, 
                        type="l",
                        xlab="datetime"))

    dev.off()
}

CreatePlot4()