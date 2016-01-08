#
# Exploratory Data Analysis (EDA) - week1 - Assigment 1 on Plotting
# plot1.R - Create the histogram of Global Active Power
#

# ===========
# Build plot1
# ===========

# Get table with reduced data (only for Feb 1, 2 of 2007)
rdata <- getReducedTable() # separate function to get reduced table

# Launch graphics device png
png(filename = "plot1.png", width = 480, height = 480)

# plot the histogram
hist(rdata$Global_active_power, col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)")

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










