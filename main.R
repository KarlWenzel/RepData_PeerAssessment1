library(ggplot2)
library(plyr)

# Close any currently open images; reset par() values for graphics device
if (dev.cur() > 1) dev.off()

# Obtain data from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
# After downloading, unzip and copy extracted file (activity.csv) to local directory.

if( !exists("df.RAW") ) {
  df.RAW <- read.csv("activity.csv") # Give us a 3-tuple: steps, date, interval
}

# Determine steps per day
df.SPD <- ddply(df.RAW, .(date), summarize, steps=sum(steps, na.rm=TRUE))

# Display steps as a histogram 
p <- ggplot(df.SPD) + geom_histogram(aes(steps), bins=nrow(df.SPD)) + ggtitle("Histogram for Steps per Day") 
print(p)

# Display mean and median, plus additional summary data of steps taken per day
print( summary(df.SPD$steps) )

# Determine average steps taken every day by 5-minute time intervals
df.SPI <- ddply(df.RAW, .(interval), summarize, msteps=mean(steps, na.rm=TRUE))

# Display a time series plot
p <- ggplot(df.SPI, aes(interval, msteps)) + geom_line() + ggtitle("Time Series Steps per Interval")
print(p)

# Which interval has the maximum number of average steps?
i <- with(df.SPI, which(msteps == max(msteps)))
print(df.SPI[i, ])

# How many records in our data set contain NAs?
print( sum(is.na(df.RAW)) )

# Create new data set with imputed values replacing NA values
df.IMP <- join(df.RAW, df.SPI, by="interval")
naFilter <- is.na(df.IMP)
df.IMP[naFilter,]$steps <- df.IMP[naFilter,]$msteps

# Determine steps per day with imputed data
df.SPD2 <- ddply(df.IMP, .(date), summarize, steps=sum(steps))

# Display steps as a histogram 
p <- ggplot(data=df.SPD2) + geom_histogram(aes(steps), bins=nrow(df.SPD2))
p <- p + ggtitle("Histogram for Imputed Steps per Day") 
print(p)

# Display mean and median, plus additional summary data of steps taken per day
print( summary(df.SPD2$steps) )

# Create and assign factor for weekday vs. weekend
df.IMP$weekday <- weekdays(as.Date(df.IMP$date))
df.IMP$dayType <- ifelse( df.IMP$weekday %in% c("Saturday","Sunday"), "weekend", "weekday" )
df.IMP$dayType <- as.factor(df.IMP$dayType)

# Determine average steps taken per interval, grouped by day type
df.SPI2 <- ddply(df.IMP, .(interval, dayType), summarize, msteps=mean(steps))

# Display a panel time series plot, grouped by day type
p <- ggplot(df.SPI2, aes(interval, msteps)) + geom_line() + facet_grid(. ~ dayType)
p <- p + ggtitle("Time Series Steps per Interval")
print(p)





