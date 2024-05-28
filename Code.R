library(quantmod)
library(fredr)
library(lubridate)
library(gtrendsR)
library(ggplot2)


##Retail Investor interest
NVDA_searches <- read_excel("nvda_interp.xlsx", col_names =FALSE)
BYON_searches <- read_excel("byon_interp.xlsx", col_names =FALSE)
LLY_searches <- read_excel("lly_interp.xlsx", col_names =FALSE)


NVDA_searches = NVDA_searches$...2
BYON_searches = BYON_searches$...2
LLY_searches = LLY_searches$...2


NVDA_searches = NVDA_searches[1:4788]
BYON_searches = BYON_searches[1:4788]
LLY_searches = LLY_searches[1:4788]

start_date <- as.Date("2004-01-1")
end_date <- as.Date("2024-01-01")
all_dates <- seq(from = start_date, to = end_date, by = "day")
weekdays_only <- all_dates[weekdays(all_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]

weekdays_only = weekdays_only[1:4788]
dates_xts <- xts(NVDA_searches, order.by = weekdays_only)


options(scipen = 999)

plot(dates_xts,  main = "NVDA searches from 2004-2023", ylab = "Number of Searches", axes = FALSE)



famaFactors = read.csv("FamaDaily.csv")

marketFactor = famaFactors$Mkt.RF[1:4788]
SMB = famaFactors$SMB[1:4788]
HML = famaFactors$HML[1:4788]
RF = famaFactors$RF[1:4788]



#NVDA Stock returns
NVDA_data = getSymbols("NVDA", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
NVDA_data = NVDA_data$NVDA.Close
NVDA_data = coredata(NVDA_data)


NVDA_returns = diff(log(NVDA_data))[1:4788]
NVDA_Excess_returns = NVDA_returns - RF
NVDA_Excess_returns = NVDA_Excess_returns


#LLY Stock returns
LLY_data = getSymbols("LLY", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
LLY_data = LLY_data$LLY.Close
LLY_data = coredata(LLY_data)

LLY_returns = diff(log(LLY_data))[1:4788]
LLY_Excess_returns = LLY_returns - RF
LLY_Excess_returns = LLY_Excess_returns


#BYON Stock returns
BYON_data = getSymbols("BYON", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
BYON_data = BYON_data$BYON.Close
BYON_data = coredata(BYON_data)

BYON_returns = diff(log(BYON_data))[1:4788]
BYON_Excess_returns = BYON_returns - RF
BYON_Excess_returns = BYON_Excess_returns


#Semicondoctor ETF
SOXX = getSymbols("SOXX", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
SOXX = SOXX$SOXX.Close
SOXX = coredata(SOXX$SOXX.Close)

SOXX_returns = diff(log(SOXX))[1:4788]
SOXX_Excess_returns = SOXX_returns - RF

plot(SOXX_returns)

##Healthcare ETF
XLV = getSymbols("XLV", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
XLV = XLV$XLV.Close
XLV = coredata(XLV$XLV)

XLV_returns = diff(log(XLV))[1:4788]
XLV_Excess_returns = XLV_returns - RF

plot(XLV_returns)


#iShares Cohen & Steers REIT ETF (ICF)
##HomeBuilders ETF
ICF = getSymbols("ICF", auto.assign = FALSE, from='2004-01-01',to='2024-01-01', periodicity  = "daily")
ICF = ICF$ICF.Close
ICF = coredata(ICF$ICF)

ICF_returns = diff(log(ICF))[1:4788]
ICF_Excess_returns = ICF_returns - RF

plot(ICF_Excess_returns)
head(NVDA_Excess_returns)


modelNVDA = lm(NVDA_Excess_returns~ marketFactor+SMB+HML + SOXX_Excess_returns + NVDA_searches)#+NVDA_searches
summary(modelNVDA)


plot(modelNVDA$residuals)
pacf(modelNVDA$residuals)
Box.test(modelNVDA$residuals, type=c("Ljung-Box"))
Box.test(modelNVDA$residuals^2, type=c("Ljung-Box"))

AIC(modelNVDA)


modelBYON = lm(BYON_Excess_returns~ marketFactor+SMB+HML + XLV_Excess_returns+BYON_searches) 
summary(modelBYON)
plot(modelBYON$residuals)
pacf(modelBYON$residuals)
Box.test(modelBYON$residuals, type=c("Ljung-Box"))
Box.test(modelBYON$residuals^2, type=c("Ljung-Box"))

AIC(modelBYON)

modelLLY = lm(LLY_Excess_returns~ marketFactor+SMB+HML + ICF_Excess_returns+LLY_searches)##
summary(modelLLY)
plot(modelLLY$residuals)
pacf(modelLLY$residuals)
Box.test(modelLLY$residuals, type=c("Ljung-Box"))
Box.test(modelLLY$residuals^2, type=c("Ljung-Box"))

AIC(modelLLY)

testNVDA = NVDA_Excess_returns[1:1261]
testMK = marketFactor[1:1261]
testSMB = SMB[1:1261] 
testHML = HML[1:1261]
testSOXX = SOXX_Excess_returns[1:1261]
testXLV = XLV[1:1261]
testICF = ICF[1:1261]
modelNVDA = lm(testNVDA~ testMK+testSMB+testHML+testSOXX + testXLV+testICF) ##+NVDA_searches[start:start+1260] ##+NVDA_searches[start:start+1260]


#summary(modelNVDA)

##Discrete rolling time frame
start = 1
for (i in 1:4){
  
  print("Loop I:")
  print(i)
  
  testNVDA = NVDA_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = NVDA_searches[start:(start + 1259)]
  
  modelNVDA = lm(testNVDA~ testMK+testSMB+testHML+testSOXX+ testSearches) 
  print(summary(modelNVDA))
  
  print("AIC:")
  print(AIC(modelNVDA))
  
  plot(modelNVDA$residuals)
  pacf(modelNVDA$residuals)
  Box.test(modelNVDA$residuals, type=c("Ljung-Box"))
  #Box.test(modelNVDA$residuals^2, type=c("Ljung-Box"))
  start = start + 1260
  
  
}

NVDA_Excess_returns[1]
NVDA_Excess_returns[1261]
NVDA_Excess_returns[2521]
NVDA_Excess_returns[3781]
NVDA_Excess_returns[4788]


start = 1
for (i in 1:4){
  
  print("Loop I:")
  print(i)
  
  testLLY = LLY_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = LLY_searches[start:(start + 1259)]
  
  modelLLY = lm(testLLY~ testMK+testSMB+testHML+testXLV+ testSearches) 
  print(summary(modelLLY))
  
  print("AIC:")
  print(AIC(modelLLY))
  
  plot(modelLLY$residuals)
  pacf(modelLLY$residuals)
  Box.test(modelLLY$residuals, type=c("Ljung-Box"))
  #Box.test(modelLLY$residuals^2, type=c("Ljung-Box"))
  start = start + 1260
  
  
}


start = 1
for (i in 1:4){
  
  print("Loop I:")
  print(i)
  
  testBYON = BYON_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = BYON_searches[start:(start + 1259)]
  
  modelBYON = lm(testBYON~ testMK+testSMB+testHML+testICF+ testSearches) 
  print(summary(modelBYON))
  
  print("AIC:")
  print(AIC(modelBYON))
  
  plot(modelBYON$residuals)
  pacf(modelBYON$residuals)
  Box.test(modelBYON$residuals, type=c("Ljung-Box"))
  #Box.test(modelBYON$residuals^2, type=c("Ljung-Box"))
  start = start + 1260
  
  
}


##Continuous rolling time frame
start = 1
for (i in 1:7){
  
  print("Loop I:")
  print(i)
  
  testNVDA = NVDA_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = NVDA_searches[start:(start + 1259)]
  
  modelNVDA = lm(testNVDA~ testMK+testSMB+testHML+testSOXX+ testSearches) 
  print(summary(modelNVDA))
  
  print("AIC:")
  print(AIC(modelNVDA))
  
  plot(modelNVDA$residuals)
  pacf(modelNVDA$residuals)
  Box.test(modelNVDA$residuals, type=c("Ljung-Box"))
  #Box.test(modelNVDA$residuals^2, type=c("Ljung-Box"))
  start = start + 630
  
  
}
test = 1
time = 1

time = time +1
time

test = test +630
test
NVDA_Excess_returns[4411]

3781+1259

start = 1
for (i in 1:7){
  
  print("Loop I:")
  print(i)
  
  testLLY = LLY_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = LLY_searches[start:(start + 1259)]
  
  modelLLY = lm(testLLY~ testMK+testSMB+testHML+testXLV+ testSearches) 
  print(summary(modelLLY))
  
  print("AIC:")
  print(AIC(modelLLY))
  
  plot(modelLLY$residuals)
  pacf(modelLLY$residuals)
  Box.test(modelLLY$residuals, type=c("Ljung-Box"))
  Box.test(modelLLY$residuals^2, type=c("Ljung-Box"))
  start = start + 630
  
  
}


start = 1
for (i in 1:7){
  
  print("Loop I:")
  print(i)
  
  testBYON = BYON_Excess_returns[start:(start + 1259)]
  testMK = marketFactor[start:(start + 1259)]
  testSMB = SMB[start:(start + 1259)] 
  testHML = HML[start:(start + 1259)]
  testSOXX = SOXX_Excess_returns[start:(start + 1259)]
  testXLV = XLV[start:(start + 1259)]
  testICF = ICF[start:(start + 1259)]
  testSearches = BYON_searches[start:(start + 1259)]
  
  modelBYON = lm(testBYON~ testMK+testSMB+testHML+testICF+ testSearches) 
  print(summary(modelBYON))
  
  print("AIC:")
  print(AIC(modelBYON))
  
  plot(modelBYON$residuals)
  pacf(modelBYON$residuals)
  Box.test(modelBYON$residuals, type=c("Ljung-Box"))
  Box.test(modelBYON$residuals^2, type=c("Ljung-Box"))
  start = start + 630
  
  
}



#NVDA Model RMSE, 4.5year train, predict 4.5year
start = 1
prediction_error=matrix(0,1197,2)
for (i in 1:2){
  
  #Train
  testNVDA = NVDA_Excess_returns[start:(start + 1196)]
  testMK = marketFactor[start:(start + 1196)]
  testSMB = SMB[start:(start + 1196)] 
  testHML = HML[start:(start + 1196)]
  testSOXX = SOXX_Excess_returns[start:(start + 1196)]
  testXLV = XLV[start:(start + 1196)]
  testICF = ICF[start:(start + 1196)]
  testSearches = NVDA_searches[start:(start + 1196)]
  
  modelNVDA = lm(testNVDA~ testMK+testSMB+testHML+testSOXX+ testSearches) 
  
  #Test
  new_data <- data.frame(
    testMK = marketFactor[(start+1197):(start+1197 +1196)],
    testSMB = SMB[(start+1197):(start+1197 +1196)],
    testHML = HML[(start+1197):(start+1197 +1196)],
    testSOXX = SOXX_Excess_returns[(start+1197):(start+1197 +1196)],
    testSearches = NVDA_searches[(start+1197):(start+1197 +1196)]
  )
  
  # Generate predictions
  predicted_values <- predict(modelNVDA, newdata = new_data)
  prediction_error[1:1197,i] = abs(predicted_values-NVDA_Excess_returns[(start+1197):(start+1197 + 1196)])
  start=start+1197*2
}
nvda1=sqrt(sum(prediction_error[1:1197,1]^2)/length(prediction_error[1:1197,1]))
nvda2=sqrt(sum(prediction_error[1:1197,2]^2)/length(prediction_error[1:1197,2]))

#BYON Model RMSE, 4.5year train, predict 4.5year
start = 1
prediction_error=matrix(0,1197,2)
for (i in 1:2){
  
  #Train
  testBYON = BYON_Excess_returns[start:(start + 1196)]
  testMK = marketFactor[start:(start + 1196)]
  testSMB = SMB[start:(start + 1196)] 
  testHML = HML[start:(start + 1196)]
  testSOXX = SOXX_Excess_returns[start:(start + 1196)]
  testXLV = XLV_Excess_returns[start:(start + 1196)]
  testICF = ICF_Excess_returns[start:(start + 1196)]
  testSearches = BYON_searches[start:(start + 1196)]
  
  modelBYON = lm(testBYON~ testMK+testSMB+testHML+testICF+ testSearches) 
  
  #Test
  new_data <- data.frame(
    testMK = marketFactor[(start+1197):(start+1197 +1196)],
    testSMB = SMB[(start+1197):(start+1197 +1196)],
    testHML = HML[(start+1197):(start+1197 +1196)],
    testICF = ICF_Excess_returns[(start+1197):(start+1197 +1196)],
    testSearches = BYON_searches[(start+1197):(start+1197 +1196)]
  )
  
  # Generate predictions
  predicted_values <- predict(modelBYON, newdata = new_data)
  prediction_error[1:1197,i] = abs(predicted_values-BYON_Excess_returns[(start+1197):(start+1197 + 1196)])
  start=start+1197*2
}
byon1=sqrt(sum(prediction_error[1:1197,1]^2)/length(prediction_error[1:1197,1]))
byon2=sqrt(sum(prediction_error[1:1197,2]^2)/length(prediction_error[1:1197,2]))

#LLY Model RMSE, 4.5year train, predict 4.5year
start = 1
prediction_error=matrix(0,1197,2)
for (i in 1:2){
  
  #Train
  testLLY = LLY_Excess_returns[start:(start + 1196)]
  testMK = marketFactor[start:(start + 1196)]
  testSMB = SMB[start:(start + 1196)] 
  testHML = HML[start:(start + 1196)]
  testSOXX = SOXX_Excess_returns[start:(start + 1196)]
  testXLV = XLV_Excess_returns[start:(start + 1196)]
  testICF = ICF_Excess_returns[start:(start + 1196)]
  testSearches = LLY_searches[start:(start + 1196)]
  
  modelLLY = lm(testLLY~ testMK+testSMB+testHML+testXLV+ testSearches) 
  
  #Test
  new_data <- data.frame(
    testMK = marketFactor[(start+1197):(start+1197 +1196)],
    testSMB = SMB[(start+1197):(start+1197 +1196)],
    testHML = HML[(start+1197):(start+1197 +1196)],
    testXLV = XLV_Excess_returns[(start+1197):(start+1197 +1196)],
    testSearches = LLY_searches[(start+1197):(start+1197 +1196)]
  )
  
  # Generate predictions
  predicted_values <- predict(modelLLY, newdata = new_data)
  prediction_error[1:1197,i] = abs(predicted_values-LLY_Excess_returns[(start+1197):(start+1197 + 1196)])
  start=start+1197*2
}
lly1=sqrt(sum(prediction_error[1:1197,1]^2)/length(prediction_error[1:1197,1]))
lly2=sqrt(sum(prediction_error[1:1197,2]^2)/length(prediction_error[1:1197,2]))
c(nvda1,nvda2,byon1,byon2,lly1,lly2)
plot(predicted_values,LLY_Excess_returns[(4788-1196):4788])

