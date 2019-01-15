 data72=read.csv(file="D:\\study\\R\\353Data Sets\\Chapter 7\\Problems\\data-prob-7-2.csv", header=TRUE) 
 attach(data72)  
 print(data72)  
 par(mfrow=c(2,2)) 
 
 plot(x,y,main="polynomial model", xlab="months since production", ylab="weight loss")
  

x1=x^2
l1=lm(y~x+x1)
summary(l1)  

r1=l1$residuals    
fitted1=y-r1

lines(x, fitted1, col="red")

plot(fitted1, r1, main="2nd order model") # residual vs fitted plots
abline(h=0)

par(mfrow=c(2,2))                   # check error normality 
qqnorm(r1,main="2nd order model")
qqline(r1)
 