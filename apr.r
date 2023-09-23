# Load necessary libraries
library(httr)
library(jsonlite)
library(zoo)
library(TTR)
# Define the API request URL for historical price data of Bitcoin (BTC)
url <- "https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range"
# Ensure the date range is within the last 90 days
params <- list(vs_currency = "usd", from = as.numeric(as.POSIXct(Sys.Date() - 89)), to = as.numeric(Sys.time()))

# Make the API request
response <- GET(url, query = params)

# Check the status code and parse the JSON response to a data frame
if (status_code(response) == 200) {
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  df <- as.data.frame(data$prices)
  
  # Check if the data frame is empty
  if (nrow(df) == 0) {
    stop("No data returned from the API.")
  }
  
  # Filter the data to keep only every 4th data point (4-hour increments)
  df <- df[seq(1, nrow(df), by = 4), ]
  
  # Set column names and convert timestamp to date format
  colnames(df) <- c("timestamp", "price")
  df$timestamp <- as.POSIXct(df$timestamp / 1000, origin = "1970-01-01")
  
  # Continue with the rest of the script for calculating technical indicators and saving to CSV...
} else {
  print(paste("Error:", status_code(response)))
  print(content(response, "text"))
}


# Check if the data frame is empty
if (nrow(df) == 0) {
  stop("No data returned from the API.")
}

# Calculate Moving Averages
df$ma5 <- rollmean(df$price, k = 5, fill = NA, align = "right")
df$ma10 <- rollmean(df$price, k = 10, fill = NA, align = "right")

# Calculate MACD and Signal Line
shortEMA <- EMA(df$price, n = 12)
longEMA <- EMA(df$price, n = 26)
df$macd <- shortEMA - longEMA
df$signalLine <- EMA(df$macd, n = 9)

# Calculate RSI
df$rsi <- RSI(df$price, n = 14)

# Identify NA values in the data frame and replace them with 0
df[is.na(df)] <- 0

# Save the data frame to a CSV file
write.csv(df, "bitcoin_price_data.csv", row.names = FALSE)

