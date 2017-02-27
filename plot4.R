plot4 <- function() {
#read table: not to convert strings to factor use stringsAsFactor = False
df.house_power <- read.table("household_power_consumption.txt", header = FALSE, sep = ";", na.strings = "?", stringsAsFactors = F)
#read first row from the table to find column names
col_names <- readLines("household_power_consumption.txt", 1)
#split names
col_names <- strsplit(col_names, ";", fixed = T)
#use splitted names for our data.frame
names(df.house_power) <- col_names[[1]]
#merge columns Date and Time into one as POSIXct class
df.house_power$Date <- as.POSIXct(paste(df.house_power$Date, df.house_power$Time), format = "%d/%m/%Y %H:%M:%S")
#subset data.frame by date 2007-02-01 and 2007-02-02. Also, delete Time column, it duplicates Date columns
df.sub_data <- subset(df.house_power, Date >= "2007-02-01" & Date < "2007-02-03", select = -Time)
#change column character class to numeric
df.sub_data[, 2:8] <- sapply(df.sub_data[, 2:8], as.numeric)
#plot
png(file = "plot4.png", width = 480, height = 480)
#set parameters of plot and margins
par(mfrow = c(2,2), mar = c(4,4,2,1))
#plot1
with(df.sub_data, plot(Global_active_power ~ Date ,type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
#plot2
with(df.sub_data, plot(Voltage ~ Date, type = "l", xlab = "datetime"))
#plot3
plot(df.sub_data$Sub_metering_1 ~ df.sub_data$Date, type = "l", xlab = "", ylab = "Energy sub metering")
points(df.sub_data$Sub_metering_2 ~ df.sub_data$Date, type = "l", col = "red")
points(df.sub_data$Sub_metering_3 ~ df.sub_data$Date, type = "l", col = "blue")
legend("topright", seg.len = 2, bty = "n", cex = 0.7, lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#plot4
with(df.sub_data, plot(Global_reactive_power ~ Date, type = "l", xlab = "datetime"))
#save plot to file
dev.off()
}
