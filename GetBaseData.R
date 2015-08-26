library("dplyr", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")

channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")
data <- list()
data$ReturnDaily <- tbl(channel, "ReturnDaily") %>%
  filter(IfTradingDay == 1) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr, TradingDay, DailyReturn,
         MarketCap, FloatMarketCap, IndustryCodeNew, IndustryNameNew, IfSuspended) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))

data$SecuMainIndex <- tbl(channel, "QT_IndexQuote") %>%
  filter(InnerCode %in% c(3145, 4978, 4982, 4089)) %>%
  select(InnerCode, TradingDay, PrevClosePrice, ClosePrice) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))

data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
  select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
  filter(IndexInnerCode %in% c(3145, 4978, 4982)) %>%
  collect %>%
  mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) 
data$IndexComponent$OutDate[is.na(data$IndexComponent$OutDate)] <- as.Date("2999-12-31")

data$TradingDay <- tbl(channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83) %>%
  select(TradingDate, IfTradingDay, IfWeekEnd, IfMonthEnd, IfYearEnd) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate))