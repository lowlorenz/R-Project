# Load the required library
library(data.table)

# Read the input CSV file
data <- fread("www/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv",
              header = FALSE, sep = ";")

# Replace commas with periods in the 5th and 6th columns
data[, V5 := gsub(",", ".", V5)]
data[, V6 := gsub(",", ".", V6)]

# Convert the 5th and 6th columns to numeric
data[, c("V5", "V6") := lapply(.SD, as.numeric), .SDcols = c("V5", "V6")]

# Function to format the 5th and 6th columns as floating-point numbers.
# Sometimes the floatingpoint is missing, eg. 56778 instead of 56.778
format_as_float <- function(x) {
  if (is.numeric(x)) {
    return(format(x, nsmall = 6))
  } else {
    return(x)
  }
}

# Apply the format_as_float function to the 5th and 6th columns
data$V5 <- sapply(data$V5, format_as_float)
data$V6 <- sapply(data$V6, format_as_float)

# Write the modified data to a new CSV file
write.table(data, file = "output.csv", sep = ";", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
print("DONE")
