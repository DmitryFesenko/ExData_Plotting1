
plot1 <- function() {
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
png(file = "plot1.png", width = 480, height = 480)
hist(df.sub_data$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
dev.off()
}