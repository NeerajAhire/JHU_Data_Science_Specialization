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
hist(pdata1$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

# Saving the plot as png
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
