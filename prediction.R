library("caret")
dataSet = read.csv("/home/prinzz1208/Desktop/Work/Data Science/car data.csv")
dataSet = data.frame(dataSet)
keeps = c('Present_Price','Kms_Driven','Year','Fuel_Type','Selling_Price')
dataSet = dataSet[keeps]
head(dataSet)
# Present_Price = dataSet$Present_Price
# Kms_Driven = dataSet$Kms_Driven
# Year = dataSet$Year
Fuel_Type = as.numeric(factor(dataSet$Fuel_Type))
dataSet$Fuel_Type = Fuel_Type
# y = dataSet$Selling_Price 

ind = sample(2,nrow(dataSet),replace = TRUE,prob = c(0.8,0.2))
trainingData = dataSet[ind == 1,]
testData = dataSet[ind == 2,]
head(trainingData[5])
head(testData[5])
# x1 = trainingData$Present_Price
# x2 = trainingData$Kms_Driven
# x3 = trainingData$Year
# x4 = as.numeric(factor(trainingData$Fuel_Type))
# y = trainingData$Selling_Price
# mod = lm(y ~ x1+x2+x3+x4,trainingData)
mod = lm(Selling_Price ~ poly(Present_Price,Kms_Driven,Year,degree = 2,raw = T),trainingData)
# mod = lm(y ~ poly(x1,x2,x3,degree = 3,raw = T),data = dataSet)

length(mod)

summary(mod)
plot(trainingData$Present_Price,trainingData$Selling_Price)
lines(smooth.spline(trainingData$Present_Price,predict(mod,trainingData)),col = 'orange',lwd = '6')

pred = predict(mod,testData)

head(pred)
head(testData[5])


#----ACCURACY OF THE PREDICTION MODEL----
MAE(predict(mod,testData),testData$Selling_Price)

RMSE(predict(mod,testData),testData$Selling_Price)
#----ACCURACY OF THE PREDICTION MODEL----
