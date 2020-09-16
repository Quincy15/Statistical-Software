library(zoo)
aapl <- read.zoo("aapl.csv", sep=",", header = TRUE, format = "%Y-%m-%d") 
plot(aapl, main = "APPLE Closing Prices on NASDAQ", ylab = "Price (USD)", xlab = "Date") 
head(aapl)
tail(aapl)
aapl[which.max(aapl)] 
ret_simple <- diff(aapl) / lag(aapl, k = -1) * 100 
summary(coredata(ret_simple)) 
ret_simple[which.min(ret_simple)] 
hist(ret_simple, breaks = 100, main = "Histogram of Simple Returns", xlab="%")
aapl_2013 <- window(aapl, start = '2013-01-01', end = '2013- 12-31') 
aapl_2013[which.max(aapl_2013)] 