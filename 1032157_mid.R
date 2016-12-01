getwd()
setwd("D:/sourcetree/ECODATA/midtest")
getwd()
library(data.table)
library(plyr)
library(curl)
#一. 請隨機產生 10000 組正整數儲存成 vector 格式，並輸出成 random10k.csv (5%)
number1 <- c(sample(10000))
number2 <- data.frame(number1)
is.vector(number2)
as.vector(number2)
write.table(number2,file = "random10k.csv",sep = ",")
#二. 請使用 for 迴圈列出 15 個費布納西(Fibonacci)數列 (10%)
len <- 15
fibvals <- numeric(len)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:len) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
} 
fibvals
#三. 請將 sample_data.txt 輸入進 R 內，#並完成以下計算 (55%)
#(a) 將 yyyymmddhh 轉成 POSIXct 時間戳記格式，並新增為一個欄(variable)，命名為 timestamp。
#並將此 sample data 輸出為 sample_data_parsed.csv (以逗號分隔，具有欄位名稱)
col_names = c('yyyymmddhh','PS01','TX01','RH01','WD01','WD02','PP01','SS01')
col_classes = c('integer', 'float', 'float', 'integer', 'float','integer', 'float','float')
sampledata <- fread(file.choose(),skip = 1,header=FALSE, colClasses=col_classes)
setnames(sampledata, col_names)
#處理時間
txt_timestamp <-  as.POSIXct(strptime(sampledata$yyyymmddhh, '%Y%m%D%h'))
#存成一個新欄位
DT = data.table(sampledata)
DT[,timestamp:=txt_timestamp]
DT

#sampledata[,timestamp:= as.POSIXct(sampledata["yyyymmddhh"], format="%Y%m%D%h")]

#(b) 請計算 2014 年至 2015 年這個測站的每月平均氣溫、#每月平均濕度、每月累積降水， 並用表格呈現。
#(c) 請計算 2014 年和 2015 年最冷月分別是在哪個月份？(提示：先計算月均溫)
#(d) 在 2015 年最冷的那個月份中，該月中每日的最低溫平均是幾度C？
#(e) 請計算 2014 年和 2015 年中，最熱的月分別是在哪個月份？
#(f) 請計算 2014 年最熱的月份中，該月的每日最高溫平均為幾度C?
#(g) 請算出 2014 至 2015 年中，最濕月份的平均溫度
#(h) 請計算每個月的月溫差(每月最高溫減去每月最高溫，取兩年平均)，平均月溫差最大的是哪個月？
#(i) 請計算這兩年的年溫差平均(每年最高溫減去最低溫)
#(j) 溫量指數(warmth index)是 Kira (1945) 提出的一個生態氣候指標，其計算方式為:
#  (1) 若該月均溫高於 5 ºC，則將該月份的月均溫減去 5 ºC。
#(2) 若該月均溫低於或等於 5 ºC，則令其為 0 ºC
#(3) 將所有減去 5 ºC 或當成 0 ºC 的月均溫相加起來得到的數據稱為「溫量指數」
#請根據 (b) 所計算出的數值，算出 2014 至 2015 年的溫量指數。
#(k) 請使用 climatol package 繪製 2014 至 2015 的生態氣候圖(Ecological climate diagrams)。 提示：你需要計算出每個月的累積降水平均、每日最高溫平均、每日最低溫平均、每月絕對最低溫。 可參考繪製生態氣候圖

#四. 請計算 Table 2 中的下列各子題 (30%)
#(a) 請計算各島環境因子(total_cover, C, EC, ..., etc.) 的平均、 第一四分位數、中位數、第三四分位數、最大值及最小值以及標準差，並整理成如下表格：
twodata = fread(file.choose(),header=T)
mean_omit_na = function(x){
  x = as.numeric(x)
  return(mean(x, na.rm=T))
}
twodatasss <- aggregate(.~ island, twodata, mean_omit_na)
cmax = function(x) {
  x = as.numeric(x)
  return(max(x, na.rm=T))
}
cmin = function(x) {
  x = as.numeric(x)
  return(min(x, na.rm=T))
}
twodatasss2 <- aggregate(.~ island, twodata, cmax)
twodatasss3 <- aggregate(.~ island, twodata, cmin)
#(b) 請分別列出 C, EC, K, Na, N 最高的五個樣區(plotid)