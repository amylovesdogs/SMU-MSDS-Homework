# from 11.6.1 video
# australian air info downloaded from 
ausair <- read.csv("air-transport-passengers-carried.csv", na.strings = "", strip.white = TRUE)
data(ausair)
air <- window(ausair,start=1990,end=2004)
plot(air,ylab="Airline Passengers",xlab="Year")

# ses
fit1 <- ses(air,alpha=0.2,initial="simple",h=3)
fit2 <- ses(air,alpha=0.6,initial="simple",h=3)
fit3 <- ses(air,h=3)
plot(fit1,PI=FALSE,ylab="Airline Passengers",xlab="Year",main="Forecasting using SES",fcol="white",type="o")
lines(fitted(fit1),col="blue",type="o")
lines(fitted(fit2),col="red",type="o")
lines(fitted(fit3),col="green",type="o")
lines(fit1$mean,col="blue",type="o")
lines(fit2$mean,col="red",type="o")
lines(fit3$mean,col="green",type="o")
legend("topleft", lty=1, col=c(1,"blue","red","green"), c("data", expression(alpha == 0.2), expression(alpha == 0.6), expression(alpha == 0.89)),pch=1)

# holt's linear trend
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5)
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5)
plot(fit2,PI=FALSE,ylab="Airline Passengers in Australia (millions)",xlab="Year",main="Forecasting Using Holt's Methods",fcol="white",type="o")
lines(fitted(fit1),col="blue",type="o")
lines(fitted(fit2),col="red",type="o")
lines(fit1$mean,col="blue",type="o")
lines(fit2$mean,col="red",type="o")