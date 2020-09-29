rm(list = ls())
#install.packages('Quandl')
#install.packages('quantmod')

library(zoo)
library(xts)
library(Quandl)
library(TTR)
library(quantmod)

#---------------------1.下载和清洗数据-------------------------------
Quandl.api_key("QGc4jxwf72tdN3h9BzSs")
#单只股票的数据/Microsoft Corporation (MSFT) Stock Prices
G1 <- Quandl('EOD/MSFT', start_date = '2013-01-31',
             end_date = '2016-11-30')
data1 <- G1$Close
#整个股票市场的数据/S&P500
getSymbols("^GSPC", src="yahoo", from = '2013-01-31', to = '2016-11-30')
G2 <- as.data.frame(GSPC)
G2$Date <- as.Date(row.names(G2))
G2 <- G2[order(G2[, 7], decreasing = TRUE), ]
row.names(G2) <- 1:nrow(G2)
data2 <- G2$GSPC.Adjusted
#无风险资产的数据/Overnight London Interbank Offered Rate
#LIBOR: https://www.quandl.com/data/FRED/USDONTD156N-Overnight-
#London-Interbank-Offered-Rate-LIBOR-based-on-U-S-Dollar
G3 <- Quandl("FRED/USDONTD156N", start_date = '2013-01-31',
             end_date = '2016-11-30')
data3 <- G3$VALUE
#查看3个数据集中的样本量
sapply(list(data1, data2, data3), length)

#识别共同日期
date_inter12 <- intersect(G1$Date, G2$Date)
date_inter23 <- intersect(G1$Date, G3$DATE)
cdates1 <- intersect(date_inter12, date_inter23)

cdates <- Reduce(intersect, list(G1$Date, G2$Date, G3$DATE))
sum(cdates == cdates1)

data1 <- G1[G1$Date %in% cdates, 'Close']
data2 <- G2[G2$Date %in% cdates, 'GSPC.Adjusted']
data3 <- G3[G3$DATE %in% cdates, 'VALUE']


#--------------------------2.贝塔估计--------------------------
#简单估计
#日对数收益率函数
logreturn <- function(x) log(head(x, -1) / tail(x, -1))
#无风险资产的对数收益率
rft <- log(1 + tail(data3, -1)/36000 * diff(cdates))
#风险溢价函数
riskpremium <- function(x) logreturn(x) - rft
beta <- cov(riskpremium(data1), riskpremium(data2)) / var(riskpremium(data2))
#基于线性回归估计beta (截距项不为0)
(fit <- lm(riskpremium(data1) ~ riskpremium(data2)))
#可视化
plot(riskpremium(data2), riskpremium(data1))
abline(fit, col = 'red')

#截距项为0
fit2 <- lm(riskpremium(data1) ~ -1 + riskpremium(data2)) #-1表示没有截距项, +1表示有截距项
summary(fit2)
#残差检验
par(mfrow = c(2, 2))
#par(mfcol = c(2, 2))
plot(fit2)
#图1：检验残差与真实值之间是否无关
#图2：检验残差是否服从正态分布
#图3：检验等方差假设
#图4：检验极端点










