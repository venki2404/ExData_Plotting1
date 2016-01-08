#
# Exploratory Data Analysis (EDA) - week1 - Assigment 1 on Plotting
# plot4.R - Plot 4 plots in 4 quadrants
#

# ===========
# Build plot4
# ===========

# Get table with reduced data (only for Feb 1, 2 of 2007)
rdata <- getReducedTable() # separate function to get reduced table

# Launch graphics device png
png(filename = "plot4.png", width = 480, height = 480)

# Create Time2 column with exact time for rdata table
rdata$Time2 = strptime(paste(rdata$Date, rdata$Time), 
                       "%d/%m/%Y %H:%M:%S")

# ----------------------------------------
# PLOT four plots a, b, c, d such as below
# a   b
# c   d
# ----------------------------------------

# ----- Set up layout of PLOT -----
par(mfrow = c(2,2), mar = c(4, 4, 4, 2), oma = c(1, 2, 2, 1))

# ---- plot a: Global Active Power versus time ------
plot(rdata$Time2, rdata$Global_active_power, type = "l", xlab = "",
     ylab = "Global Active Power")

# ---- plot b: Voltage versus time ------
plot(rdata$Time2, rdata$Voltage, type = "l", xlab = "datetime",
     ylab = "Voltage")


# ---- plot c: Energy sub metering versus time
plot(rdata$Time2, rdata$Sub_metering_1, type = "n", xlab = "",
     ylab = "Energy sub metering")

points(rdata$Time2, rdata$Sub_metering_1, type = "l", col = "black")
points(rdata$Time2, rdata$Sub_metering_2, type = "l", col = "red")
points(rdata$Time2, rdata$Sub_metering_3, type = "l", col = "blue")

legend("topright", lty = 1, col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


# ---- plot d: Global Reactive Power versus time ------
plot(rdata$Time2, rdata$Global_reactive_power, type = "l", xlab = "datetime",
     ylab = "Global_reactive_power")


# close png device
dev.off()


#  -------------------------------------------
# Function to get reduced table 
# Builds table with data for Feb 1 & 2 of 2007
# --------------------------------------------

getReducedTable <- function() {
  
  # ---- Download and unzip the files ----
  fileURL <- 
    "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  filezip <- "./household_power_consumption.zip"
  filetxt = "./household_power_consumption.txt"
  
  download.file(fileURL, destfile = filezip, method = "curl")
  filetxt <- unzip(filezip)
  
  #  ----- Find the range of lines to read 
  #        to get data for Feb 1 & 2 of 2007 --------
  
  con <- file(filetxt, "")
  open(con, "r")
  
  lineCount <- 0
  match <- logical(0); match <- FALSE
  foundStart <- logical(0); foundStart <- FALSE
  
  while(length(line <- readLines(con, n = 1)) != 0) {
    lineCount <- lineCount + 1
    
    if (lineCount == 1) headerLine <- line # will need to make header
    
    match <- grepl("1/2/2007|2/2/2007",line)
    if (match & !foundStart) {
      foundStart <- TRUE
      start <- lineCount
    }
    
    if (foundStart & !match) {
      end <- lineCount - 1
      break
    }
  }
  
  close(con)
  
  # Set # of lines to skip and # lines to read for table creation
  skipl <- start - 1
  nrowl <- end - start + 1
  
  # Read reduced table using skip and nrow determined
  mdata <- read.table(filetxt, header = FALSE, sep = ";",
                      skip = skipl, nrow = nrowl,
                      stringsAsFactors = FALSE, na.strings = "?")
  
  # Set the header for the table
  headvec <- strsplit(headerLine,";")
  colnames(mdata) <- headvec[[1]]
  
  return(mdata)
  
}










