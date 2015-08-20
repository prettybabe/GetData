
library("dplyr", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")
library("RSQLServer")

nIndexCode <- 4982
startdate <- as.Date("2007-01-15")
enddate <- as.Date("2015-8-14")

channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")
data <- list()
data$ReturnDaily <- tbl(channel, "ReturnDaily") %>%
  filter(IfTradingDay == 1) %>%
  select(InnerCode, TradingDay, DailyReturn, FloatMarketCap, IndustryNameNew, IfWeekEnd, IfSuspended) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))

data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
  select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
  filter(IndexInnerCode == nIndexCode) %>%
  collect %>%
  mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) 
data$IndexComponent$OutDate[is.na(data$IndexComponent$OutDate)] <- as.Date("2999-12-31")

data$TradingDay <- tbl(channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83) %>%
  select(TradingDate, IfTradingDay, IfWeekEnd, IfMonthEnd, IfYearEnd) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate))


trading_date <- data$TradingDay %>%
  filter(IfWeekEnd == 1, TradingDate >= startdate & TradingDate <= enddate) %>%
  select(TradingDate) %>%
  mutate(Start = lag(TradingDate)) %>%
  rename(End =  TradingDate) %>%
  select(Start, End) %>%
  na.omit()

industry_weekly_return <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == start) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
              by = c("InnerCode" = "SecuInnerCode"))
  
  stock_return_temp <- temp %>%
    inner_join(data$ReturnDaily %>% 
                 filter(TradingDay > start,  TradingDay <= end) %>%
                 select(InnerCode, DailyReturn), by = "InnerCode") %>% 
    group_by(InnerCode, FloatMarketCap, IndustryNameNew) %>% 
    summarise(StockReturn = expm1(sum(log1p(DailyReturn.y)))) %>%
    ungroup()
  
  industry_return_temp <- stock_return_temp %>%
    group_by(IndustryNameNew) %>%
    summarise(UnSespendedFloatMarketCap = sum(FloatMarketCap)) %>% 
    inner_join(stock_return_temp %>% semi_join(temp %>% filter(IfSuspended == 0), by = "InnerCode"),
               by = "IndustryNameNew") %>%
    group_by(IndustryNameNew, UnSespendedFloatMarketCap) %>% 
    summarise(IndustryReturn = weighted.mean(StockReturn, FloatMarketCap),
              SespendedFloatMarketCap = sum(FloatMarketCap)) %>%
    ungroup() 
  
  industry_weekly_return <- rbind(industry_weekly_return, industry_return_temp)
}

save(industry_weekly_return, file = "IndustryWeeklyReturn.RData")  
  