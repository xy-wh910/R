data2=read.csv(file="D:\\study\\R\\353Data Sets\\Chapter 7\\Problems\\data-prob-7-11.csv", header=TRUE) 
attach(data2)
x2=pmax(x-200,0)
lsp=lm(y~x+x2)
summary(lsp)

r2=lsp$residuals    
fitted2=y-r2
lines(x, fitted2, col="red")
MSE=(summary.lm(lsp)$sigma)^2
par(mfrow=c(2,2))
plot(fitted2, r2, main="piecewise polynomial model") # residual vs fitted plots
abline(h=0)
qqnorm(r2,main="piecewise model")
qqline(r2)

