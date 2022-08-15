# Downloading and unzipping the dataset
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = 'power_data.zip')
unzip('power_data.zip')

# Reading the dataset and assigning to variable 'pdata' 
pdata <- read.csv("household_power_consumption.txt", sep = ";")

# Converting Time column to POSIXct class and using date from Date Column
pdata$Time <- strptime(paste(pdata$Date, pdata$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")

# Converting Date column to Date class
pdata$Date <- as.Date(pdata$Date, format = "%d/%m/%Y")

# Converting all other columns to numeric class
pdata[,3:8] <- lapply(pdata[,3:8], as.numeric)

# Creating variables for the key date values
d1 = as.Date("2007-02-01", format= "%Y-%m-%d")
d2 = as.Date("2007-02-02", format= "%Y-%m-%d")

# Subsetting the dataset for the above dates and storing in variable 'pdata1'
pdata1 <- pdata[which(pdata$Date == d1 | pdata$Date == d2), ]

# Plotting the required plot
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(pdata1$Time, pdata1$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
plot(pdata1$Time, pdata1$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
plot(pdata1$Time,pdata1$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(pdata1$Time,pdata1$Sub_metering_2, type = "l", col= "red")
lines(pdata1$Time,pdata1$Sub_metering_3, type = "l", col= "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1, 1), col = c("black","red", "blue"), bty = "n")
plot(pdata1$Time, pdata1$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")

# Saving the plot as png
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
