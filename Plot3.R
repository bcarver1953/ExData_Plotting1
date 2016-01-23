
#
# Exploratory Data Analysis Assignment: Course Project 1
#

# Clean up extra factor levels after a subset action
myFactorCleanup <- function(x) {
  if (is.factor(x)) {
    return(as.factor(as.character(x)))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if(is.factor(x[,i])) { x[,i] <- as.factor(as.character(x[,i])) }
    }
    return(as.data.frame(x))
  }
}

setwd("C:/bnc/Coursera/Exploratory Data Analysis/exdata_data_household_power_consumption")


#count.fields("household_power_consumption.txt", sep=";")

# Read all the base data rows in - subset later...

hhpowerIN <- read.table("household_power_consumption.txt", skip=0, 
                          header=TRUE, sep=";", na.strings="?", dec=".", strip.white=TRUE, 
                          quote = "", fill=TRUE)

# Save off just in case
save("hhpowerIN", file="hhpowerIN.rda")
#load("hhpowerIN.rda")


head(hhpowerIN)
summary(hhpowerIN)


# Subset to the dates needed

hhpower <- subset(hhpowerIN, subset=Date == '1/2/2007' | Date == '2/2/2007')
hhpower <- myFactorCleanup(hhpower)
summary(hhpower)


# Reformat date fields into new columns

hhpower$myDate <- as.Date(hhpower$Date, format="%d/%m/%Y")
hhpower$myTime <- strptime(hhpower$Time, format="%H:%M:%S")
hhpower$myDT <- paste(hhpower$Date, hhpower$Time, sep=" ")
hhpower$myDateTime <- strptime(hhpower$myDT, format="%d/%m/%Y %H:%M:%S")
summary(hhpower)

save("hhpower", file="hhpower.rda")
#load("hhpower.rda")

par(mfrow = c(1,1)) # Reset 





# Plot 3  

# Legend was truncated when using the dev.copy route - this technique fixed that

png(filename = "plot3.png",
    width = 480, height = 480)

plot(hhpower$myDateTime, hhpower$Sub_metering_1
     #     , col="red"
     , type="n"
     , xlab=""
     , ylab="Energy sub metering"
     , main=""
    )

legend("topright",col=c("black","red","blue")
       ,lwd=c(2.5,2.5)
 #      ,cex=.8
       ,c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
)

lines(hhpower$myDateTime, hhpower$Sub_metering_1
      , col="black"
      ,type="l")
lines(hhpower$myDateTime, hhpower$Sub_metering_2
      , col="red"
      ,type="l")
lines(hhpower$myDateTime, hhpower$Sub_metering_3
      , col="blue"
      ,type="l")

dev.off()





