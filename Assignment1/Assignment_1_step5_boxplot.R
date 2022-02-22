table(x)
table(dataframetest)
table(MyRData,dataframetest)

#Box Plots
str(airquality)
boxplot(airquality$Ozone)
b <- boxplot(airquality$Ozone)
b

#Bar Plots

require(datasets)
data(chickwts)
plot(chickwts$feed)