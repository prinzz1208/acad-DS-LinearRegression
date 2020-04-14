library(GGally)
dataSet = read.csv("/home/prinzz1208/Desktop/Work/Data Science/R/car data.csv")
x1 = dataSet$Present_Price
x2 = dataSet$Kms_Driven
x3 = dataSet$Year
x4 = as.numeric(factor(dataSet$Transmission))
x5 = as.numeric(factor(dataSet$Fuel_Type))
y = dataSet$Selling_Price

ggpairs(data = dataSet,columns = 2:9)

cor(x4,y)
plot(x1,y,xlab = "Present Price",ylab = "Selling Price")
x1_x2 = x1*x2
x12 = x1^2
x22 = x2^2
mod = lm(y ~ x1+x1_x2+x12+x22+x2, data = dataSet)
# mod = lm(y ~ poly(x1,x2,x3,x4,x5,degree = 3,raw = T),data = dataSet)
mod1 = lm(y ~ poly(x1,x2,x3,degree = 2,raw = T), data = dataSet)
mod2 = lm(y ~ poly(x1,degree = 3,raw = T), data = dataSet)
mod3 = lm(y ~ poly(x1,degree = 3,raw = T)+poly(x2,degree = 3,raw = T)
          +poly(x3,degree = 3,raw = T),data = dataSet)

# mod4 = lm(y ~ x1+x2+x3+x4+x5,data = dataSet)

lines(smooth.spline(x1,predict(mod)),col = 'orange',lwd = '2')
# lines(smooth.spline(x1,predict(mod1)),col = 'red',lwd = '2')
# lines(smooth.spline(x1,predict(mod2)),col = 'green',lwd = '3')
# lines(smooth.spline(x1,predict(mod3)),col = 'blue',lwd = '4')
# lines(smooth.spline(x1,predict(mod4)),col = 'gray',lwd = '4')
# lines(smooth.spline(x1,y),col = 'black',lwd = '4')

# coef = mod$coefficients
summary(mod)
summary(mod1)
summary(mod2)
summary(mod3)
anova(mod4,mod3)
