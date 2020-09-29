rm(list = ls())

library(tseries)
library(zoo)
library(xts)
library(Quandl)
library(TTR)
library(quantmod)


#-----------------------------1.数据下载和清洗----------------------------------
#从S&P500中下载20支个股数据
symbols <- c("A", "AA", "AAPL", "ABC", "ABT", "ACN", "ADBE", "ADI", "ADM", "ADP",
             "ADSK", "AEE", "AEP", "AES","AET", "AFL","AGN", "AIG", "AIV", "AIZ")
res <- lapply(symbols, function(symbol) 
  get.hist.quote(symbol, quote = "AdjClose", quiet = TRUE,
                 start = as.Date('2013-01-01'), end = as.Date('2016-11-30')))
#整个股票市场的数据/S&P500
getSymbols("^GSPC", src="yahoo", from = '2013-01-01', to = '2016-11-30')
G2 <- as.data.frame(GSPC)
G2$Date <- as.Date(row.names(G2))
G2 <- G2[order(G2[, 7], decreasing = TRUE), ]
row.names(G2) <- 1:nrow(G2)
data2 <- G2$GSPC.Adjusted
#无风险资产的数据/Overnight London Interbank Offered Rate
G3 <- Quandl("FRED/USDONTD156N", start_date = '2013-01-01',
             end_date = '2016-11-30')
data3 <- G3$VALUE
#识别共同日期,并将日度数据转换为月度数据
cdates <- intersect(G3$DATE, G2$Date)
#日期拆分成"日”、“年-月”
d <- data.frame(date = as.Date(cdates, origin = '1970-01-01'))
d$day <- format(d$date, format = '%d')
d$my <- format(d$date, format = '%Y-%m')
#每个月只取最小的一天
(fds <- with(d, tapply(day, my, min)))
#a <- tapply(d$day, d$my, min)
#将日期整合为“年-月-日”
(fds <- as.Date(paste(row.names(fds), fds, sep = '-')))
#对res过滤，得到一个基于月度的数据集, 将其储存为data.frame格式
res <- lapply(res, function(x) x[which(zoo::index(x) %in% fds)])
res <- do.call(merge, res)
str(res)
res <- as.data.frame(res)
names(res) <- symbols
#对S&P进行过滤，生成一个新的月度数据集
data2 <- G2[G2$Date %in% fds, 'GSPC.Adjusted']
#过滤无风险资产数据
data3 <- G3[G3$DATE %in% fds, 'VALUE']
#无风险资产的对数收益率
rft <- log(1 + head(data3, -1)/36000 * as.numeric(diff(fds)))
#日对数收益率函数
logreturn <- function(x) log(tail(x, -1) / head(x, -1))
#风险溢价函数
riskpremium <- function(x) logreturn(x) - rft
#计算20支个股的风险溢价值
res <- apply(res, 2, riskpremium)


#--------------------------2.对SCL建模---------------------------
#将20支个股的风险溢价值转换为data.frame格式
res <- as.data.frame(res)
r <- t(sapply(symbols, function(symbol)
  c(beta = lm(res[, symbol] ~
                riskpremium(data2))$coefficients[[2]],
    mean = mean(res[, symbol]))
  ))
r <- as.data.frame(r)
plot(r$beta, r$mean)
abline(lm(r$mean ~ r$beta), col = 'red')
summary(lm(r$mean ~ r$beta))

#----------------------3.体验个体方差的解释能力-------------------
#计算20支个股的beta、均值以及个体方差值
r <- t(sapply(symbols, function(symbol) {
  stock <- res[, symbol]
  beta <- lm(stock ~ riskpremium(data2))$coefficients[[2]]
  c(
    beta = beta,
    mean = mean(stock, na.rm = TRUE),
    risk = var(stock, na.rm = TRUE) - beta^2 * var(data2))
  }))
r <- as.data.frame(r)
summary(lm(r$mean ~ r$beta + r$risk))








































