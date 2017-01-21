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
write.table(number2,file = "random10k.csv",sep = ",")#答案

#二. 請使用 for 迴圈列出 15 個費布納西(Fibonacci)數列 (10%)
len <- 15
fibvals <- numeric(len)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:len) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
} 
fibvals#答案

#三. 請將 sample_data.txt 輸入進 R 內，#並完成以下計算 (55%)
#(a) 將 yyyymmddhh 轉成 POSIXct 時間戳記格式，並新增為一個欄(variable)，命名為 timestamp。
#並將此 sample data 輸出為 sample_data_parsed.csv (以逗號分隔，具有欄位名稱)
col_names = c('yyyymmddhh','PS01','TX01','RH01','WD01','WD02','PP01','SS01')
col_classes = c('integer', 'float', 'integer', 'integer', 'float','integer', 'integer','float')
sampledata <- fread(file.choose(),skip = 1,header=FALSE, colClasses=col_classes,na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))
setnames(sampledata, col_names)
#處理時間
sampledata[,timestamp:=as.POSIXct(strptime(sampledata$yyyymmddhh, format='%Y%m%d%H'))]
View(sampledata)
write.table(sampledata,file = "sample_data_parsed.csv",sep = ",")#答案

#(b) 請計算 2014 年至 2015 年這個測站的每月平均氣溫、每月平均濕度、每月累積降水， 並用表格呈現。
sampledata [, year:= substr ((sampledata$yyyymmddhh),1 ,4)]
sampledata [, month:= substr ((sampledata$yyyymmddhh),1 ,6)]
sampledata [, day:= substr ((sampledata$yyyymmddhh),1 ,8)]
stno1 <- aggregate(sampledata$TX01 ~ sampledata$month , sampledata, mean ,na.rm = T)
stno2 <- aggregate(sampledata$RH01 ~ sampledata$month , sampledata, mean ,na.rm = T)
stno3 <- aggregate(sampledata$PP01 ~ sampledata$month , sampledata, sum ,na.rm = T)
stno <- dplyr::inner_join(stno1,stno2,by = "sampledata$month")
stnofinish <- dplyr::inner_join(stno,stno3,by = "sampledata$month")#答案

#(c) 請計算 2014 年和 2015 年最冷月分別是在哪個月份？(提示：先計算月均溫)
tx201415 <- aggregate(sampledata$TX01 ~ (sampledata$year+sampledata$month) , sampledata, mean ,na.rm = T)
txmin <- aggregate(tx201415$`sampledata$TX01` ~ tx201415$`sampledata$year`, tx201415, min ,na.rm = T)
txmin
colnames(txmin) <- c("sampledata$year","sampledata$TX01") 
txminmonth <- dplyr::left_join(txmin,tx201415,by = "sampledata$TX01")
txminmonth#答案

#(d) 在 2015 年最冷的那個月份中，該月中每日的最低溫平均是幾度C？
monthcold2015 <- dplyr::filter(sampledata,sampledata$month == 201501)
monthcold2015
monthcold2015ed <- aggregate(monthcold2015$TX01 ~ monthcold2015$day, monthcold2015, min ,na.rm = T)
monthcold2015ed
mean(monthcold2015ed$`monthcold2015$TX01`)#答案

#(e) 請計算 2014 年和 2015 年中，最熱的月分別是在哪個月份？
txmax <- aggregate(tx201415$`sampledata$TX01` ~ tx201415$`sampledata$year`, tx201415, max ,na.rm = T)
txmax
colnames(txmax) <- c("sampledata$year","sampledata$TX01") 
txmaxmonth <- dplyr::left_join(txmax,tx201415,by = "sampledata$TX01")
txmaxmonth#答案

#(f) 請計算 2014 年最熱的月份中，該月的每日最高溫平均為幾度C?
monthhot2014 <- dplyr::filter(sampledata,sampledata$month == 201407)
monthhot2014
monthhot2014ed <- aggregate(monthhot2014$TX01 ~ monthhot2014$day, monthhot2014, max ,na.rm = T)
monthhot2014ed
mean(monthhot2014ed$`monthhot2014$TX01`)#答案

#(g) 請算出 2014 至 2015 年中，最濕月份的平均溫度
wetmonth <- aggregate(sampledata$RH01 ~ (sampledata$year+sampledata$month), sampledata, mean ,na.rm = T)
wetmonth
wetmax <- aggregate( wetmonth$`sampledata$RH01` ~ wetmonth$`sampledata$year`,wetmonth, max ,na.rm = T)
wetmax
colnames(wetmax) <- c("year","RH01") 
colnames(wetmonth) <- c("year","month","RH01") 
wetmonth
wetmax
wetmaxyear <- dplyr::left_join(wetmax,wetmonth,by = "RH01")
wetmaxyear
#所以201409是最濕月份
TM201409 <- dplyr::filter(sampledata,sampledata$month == 201409)
TM201409
TMEAN201409 <- aggregate(TM201409$TX01 ~ TM201409$month, TM201409, mean ,na.rm = T)
TMEAN201409#答案

#(h) 請計算每個月的月溫差(每月最高溫減去每月最低溫，取兩年平均)，平均月溫差最大的是哪個月？
TMAX <- aggregate(sampledata$TX01 ~ sampledata$month, sampledata, max ,na.rm = T)
TMAX
TMIN <- aggregate(sampledata$TX01 ~ sampledata$month, sampledata, min ,na.rm = T)
TMIN
TMAXMIN <- dplyr::full_join(TMAX,TMIN,by = "sampledata$month")
colnames(TMAXMIN) <- c("month","Htx01","Ctx01") 
TMAXMIN
TMAXMIN2 <- dplyr::mutate(TMAXMIN,new = Htx01 - Ctx01)
TMAXMIN2
max(TMAXMIN2$'new')
TMAXMIN3 <- dplyr::mutate(TMAXMIN2,nothing = 1)
TMAXMIN3
TMAXMIN4 <- aggregate(TMAXMIN3$new ~ TMAXMIN3$nothing, TMAXMIN3, max ,na.rm = T)
TMAXMIN4
colnames(TMAXMIN4) <- c("nothing","new") 
TMAXMIN4
TMAXMIN5 <- dplyr::semi_join(TMAXMIN3,TMAXMIN4,by = "new")
TMAXMIN5#答案

#(i) 請計算這兩年的年溫差平均(每年最高溫減去最低溫)
YTMAX <- aggregate(sampledata$TX01 ~ sampledata$year, sampledata, max ,na.rm = T)
YTMAX
YTMIN <- aggregate(sampledata$TX01 ~ sampledata$year, sampledata, min ,na.rm = T)
YTMIN
YTMAXMIN <- dplyr::full_join(YTMAX,YTMIN,by = "sampledata$year")
YTMAXMIN
colnames(YTMAXMIN) <- c("year","Htx01","Ctx01")
YTMAXMIN2 <- dplyr::mutate(YTMAXMIN,new = Htx01 - Ctx01)
YTMAXMIN2
mean(YTMAXMIN2$'new')#答案

#(j) 溫量指數(warmth index)是 Kira (1945) 提出的一個生態氣候指標，其計算方式為:
##(1) 若該月均溫高於 5 ºC，則將該月份的月均溫減去 5 ºC。
##(2) 若該月均溫低於或等於 5 ºC，則令其為 0 ºC
##(3) 將所有減去 5 ºC 或當成 0 ºC 的月均溫相加起來得到的數據稱為「溫量指數」
##請根據 (b) 所計算出的數值，算出 2014 至 2015 年的溫量指數。

colnames(stnofinish) <- c("month","TX01","RH01","PP01")
stnofinish2 <- dplyr::mutate(stnofinish, new = ifelse(stnofinish$TX01 > 5 ,stnofinish$TX01-5,0 )) 
stnofinish2
j <- sum(stnofinish2$new)
j
#(k) 請使用 climatol package 繪製 2014 至 2015 的生態氣候圖(Ecological climate diagrams)。 
#提示：你需要計算出每個月的累積降水平均、每日最高溫平均、每日最低溫平均、每月絕對最低溫。 可參考繪製生態氣候圖
library(climatol)
library(Cairo)

#四. 請計算 Table 2 中的下列各子題 (30%)
#(a) 請計算各島環境因子(total_cover, C, EC, ..., etc.) 的平均、 第一四分位數、中位數、第三四分位數、
#最大值及最小值以及標準差，並整理成如下表格：
#EXCEL是BIG5編碼,所以要加上encoding = 'UTF-8'

twodata = fread(file.choose(),header=T,encoding = 'UTF-8')
tcmean <- aggregate(twodata$total_cover~twodata$island,twodata,mean,na.rm = T)
tcmean
colnames(tcmean) <- c("island","average")
rownames(tcmean) <- c("西吉嶼total cover","西嶼坪嶼total cover","東吉嶼total cover","東嶼坪嶼total cover","鋤頭嶼total cover")
tcmean

ccmean <- aggregate(twodata$C~twodata$island,twodata,mean,na.rm = T)
ccmean
colnames(ccmean) <- c("island","average")
rownames(ccmean) <- c("西吉嶼C","西嶼坪嶼C","東吉嶼C","東嶼坪嶼C","鋤頭嶼C")
ccmean

ecmean <- aggregate(twodata$EC~twodata$island,twodata,mean,na.rm = T)
ecmean
colnames(ecmean) <- c("island","average")
rownames(ecmean) <- c("西吉嶼EC","西嶼坪嶼EC","東吉嶼EC","東嶼坪嶼EC","鋤頭嶼EC")
ecmean

kmean <- aggregate(twodata$K~twodata$island,twodata,mean,na.rm = T)
kmean
colnames(kmean) <- c("island","average")
rownames(kmean) <- c("西吉嶼K","西嶼坪嶼K","東吉嶼K","東嶼坪嶼K","鋤頭嶼K")
kmean

Namean <- aggregate(twodata$Na~twodata$island,twodata,mean,na.rm = T)
Namean
colnames(Namean) <- c("island","average")
rownames(Namean) <- c("西吉嶼Na","西嶼坪嶼Na","東吉嶼Na","東嶼坪嶼Na","鋤頭嶼Na")
Namean

Nmean <- aggregate(twodata$N~twodata$island,twodata,mean,na.rm = T)
Nmean
colnames(Nmean) <- c("island","average")
rownames(Nmean) <- c("西吉嶼N","西嶼坪嶼N","東吉嶼N","東嶼坪嶼N","鋤頭嶼N")
Nmean

Rrmean <- aggregate(twodata$rock_ratio~twodata$island,twodata,mean,na.rm = T)
Rrmean
colnames(Rrmean) <- c("island","average")
rownames(Rrmean) <- c("西吉嶼rock_ratio","西嶼坪嶼rock_ratio","東吉嶼rock_ratio","東嶼坪嶼rock_ratio","鋤頭嶼rock_ratio")
Rrmean


tcquantile <- aggregate(twodata$total_cover~twodata$island,twodata,quantile, na.rm = T)
tcquantile

ccquantile <- aggregate(twodata$C~twodata$island,twodata,quantile,na.rm = T)
ccquantile

ecquantile <- aggregate(twodata$EC~twodata$island,twodata,quantile,na.rm = T)
ecquantile

kquantile <- aggregate(twodata$K~twodata$island,twodata,quantile,na.rm = T)
kquantile

Naquantile <- aggregate(twodata$Na~twodata$island,twodata,quantile,na.rm = T)
Naquantile

Nquantile <- aggregate(twodata$N~twodata$island,twodata,quantile,na.rm = T)
Nquantile

Rrquantile <- aggregate(twodata$rock_ratio~twodata$island,twodata,quantile,na.rm = T)
Rrquantile

tcsd <- aggregate(twodata$total_cover~twodata$island,twodata,sd, na.rm = T)
tcsd

ccsd <- aggregate(twodata$C~twodata$island,twodata,sd,na.rm = T)
ccsd

ecsd <- aggregate(twodata$EC~twodata$island,twodata,sd,na.rm = T)
ecsd

ksd <- aggregate(twodata$K~twodata$island,twodata,sd,na.rm = T)
ksd

Nasd <- aggregate(twodata$Na~twodata$island,twodata,sd,na.rm = T)
Nasd

Nsd <- aggregate(twodata$N~twodata$island,twodata,sd,na.rm = T)
Nsd

Rrsd <- aggregate(twodata$rock_ratio~twodata$island,twodata,sd,na.rm = T)
Rrsd

ing1 <- dplyr::bind_rows(tcmean,ccmean)
ing1
ing2 <- dplyr::left_join(ing1,ecmean,by = "twodata$island")
ing2
ing3 <- dplyr::left_join(ing2,kmean,by = "twodata$island")
ing4 <- dplyr::left_join(ing3,Namean,by = "twodata$island")
ing5 <- dplyr::left_join(ing4,Nmean,by = "twodata$island")
ing6 <- dplyr::left_join(ing5,Rrmean,by = "twodata$island")
ing6







#(b) 請分別列出 C, EC, K, Na, N 最高的五個樣區(plotid)
tryyyyyyy <- dplyr::mutate(twodata,nothing = 1)
twodata2 <- as.data.table(twodata)
View(tryyyyyyy)
max(twodata$C)
maxcc <- aggregate(tryyyyyyy$C ~ tryyyyyyy$nothing, tryyyyyyy, max ,na.rm = T)
maxcc
colnames(maxcc) <- c("nothing","C") 
maxcc
maxcc2 <- dplyr::semi_join(tryyyyyyy,maxcc,by = "C")
maxcc2#答案

max(twodata$EC)
maxec <- aggregate(tryyyyyyy$EC ~ tryyyyyyy$nothing, tryyyyyyy, max ,na.rm = T)
maxec
colnames(maxec) <- c("nothing","EC") 
maxec
maxec2 <- dplyr::semi_join(tryyyyyyy,maxec,by = "EC")
maxec2#答案

max(twodata$K)
maxK <- aggregate(tryyyyyyy$K ~ tryyyyyyy$nothing, tryyyyyyy, max ,na.rm = T)
maxK
colnames(maxK) <- c("nothing","K") 
maxK
maxK2 <- dplyr::semi_join(tryyyyyyy,maxK,by = "K")
maxK2#答案

max(twodata$Na)
maxNa <- aggregate(tryyyyyyy$Na ~ tryyyyyyy$nothing, tryyyyyyy, max ,na.rm = T)
maxNa
colnames(maxNa) <- c("nothing","Na") 
maxNa
maxNa2 <- dplyr::semi_join(tryyyyyyy,maxNa,by = "Na")
maxNa2#答案

max(twodata$N)
maxN <- aggregate(tryyyyyyy$N ~ tryyyyyyy$nothing, tryyyyyyy, max ,na.rm = T)
maxN
colnames(maxN) <- c("nothing","N") 
maxN
maxN2 <- dplyr::semi_join(tryyyyyyy,maxN,by = "N")
maxN2#答案

##不知道為什麼最後的dplyr::semi_join(tryyyyyyy,maxN,by = "N")都跟我說Error in data.table::setkeyv(y, by$x) : x is not a data.table
