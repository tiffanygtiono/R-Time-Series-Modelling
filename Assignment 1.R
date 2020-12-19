setwd("C:/Users/tiffa/Documents/FINA 6271/Prof_Mejia/Session 1/Assignment 1 11_05_2020")

Housedata <- read.csv("HSN1FNSA.csv",header = TRUE)

head(Housedata)

summary(Housedata)

#Ensure for a numeric class 

class(Housedata$HSN1FNSA)

#plot data over time, i.e. the data in q1 variable by year

plot.ts(Housedata$HSN1FNSA)

#This section converts the original table into a time series
#to get it ready for the decomposition
Housedata.ts <- ts(Housedata$HSN1FNSA, start=1963, frequency = 12)

#The series is decomposed through the 'decompose' function
dec.Housedata.ts <- decompose(Housedata.ts)

#Here we produce the plot of the components
plot(dec.Housedata.ts)

#And finally here we ask R to show the decomposed data,
#component by component

dec.Housedata.ts

#Identify existing cyclical component
#Start by plotting trend component produced through decomposition function
plot(dec.Housedata.ts$trend)

#now convert decomposition output to data frame
dec.Hdata=with(dec.Housedata.ts, data.frame(observed=x, trend=trend,
                                           seasonal=seasonal, random=random))
#Check if there are 'NA' missing values
head(dec.Hdata)

#Excluded NA variables
dec.excdata <- dec.Hdata[complete.cases(dec.Hdata),]

#Create an time variable starting at t=1, just a  time indicator
dec.excdata$time <- seq.int(nrow(dec.excdata))
head(dec.excdata,10)


#Estimate regression, output residuals and predicted linear trend
cyc.lm <- 
      lm(dec.excdata$trend ~ dec.excdata$time)
cyc.cyclical <- resid(cyc.lm)
cyc.puretrend <- predict(cyc.lm)

#Plot Pure Linear Trend Component
plot(
  dec.excdata$time,
  cyc.puretrend,
  ylab = "US New House Sold",
  xlab = "Time",
  main = "Purely Linear Trend Component",
  type = "l",
  lty = 1
)
abline(0, 0)


#Plot Cyclical Component
plot(
  dec.excdata$time,
  cyc.cyclical,
  ylab = "House Sold Fluctuations Around Pure Linear Trend",
  xlab = "Time",
  main = "Cyclical Component",
  type = "l",
  lty = 1
)
abline(0, 0)


RGdomestic <- read.csv("A191RL1Q225SBEA.csv", header = TRUE)

head(RGdomestic)

summary(RGdomestic)

#Ensure for a numeric class 

class(RGdomestic$A191RL1Q225SBEA)

#plot data over time, i.e. the data in q1 variable by year

plot.ts(RGdomestic$A191RL1Q225SBEA)

#This section converts the original table into a time series
#to get it ready for the decomposition
GrossDomestic.ts <- ts(RGdomestic$A191RL1Q225SBEA, start=1963, frequency = 12)

#The series is decomposed through the 'decompose' function
dec.GrossDomestic.ts <- decompose(GrossDomestic.ts)

#Here we produce the plot of the components
plot(dec.GrossDomestic.ts)

#And finally here we ask R to show the decomposed data,
#component by component

dec.GrossDomestic.ts

#Identify existing cyclical component
#Start by plotting trend component produced through decomposition function
plot(dec.GrossDomestic.ts$trend)

#now convert decomposition output to data frame
dec.Grossdata=with(dec.GrossDomestic.ts, data.frame(observed=x, trend=trend,
                                            seasonal=seasonal, random=random))
#Check if there are 'NA' missing values
head(dec.Grossdata)

#Excluded NA variables
dec.Grossexcdata <- dec.Grossdata[complete.cases(dec.Grossdata),]

#Create an time variable starting at t=1, just a  time indicator
dec.Grossexcdata$time <- seq.int(nrow(dec.Grossexcdata))
head(dec.Grossexcdata,10)


#Estimate regression, output residuals and predicted linear trend
cyc.lm <- 
  lm(dec.Grossexcdata$trend ~ dec.Grossexcdata$time)
cyc.cyclical <- resid(cyc.lm)
cyc.puretrend <- predict(cyc.lm)

#Plot Pure Linear Trend Component
plot(
  dec.Grossexcdata$time,
  cyc.puretrend,
  ylab = "Real Gross Domestic",
  xlab = "Time",
  main = "Purely Linear Trend Component",
  type = "l",
  lty = 1
)
abline(0, 0)


#Plot Cyclical Component
plot(
  dec.Grossexcdata$time,
  cyc.cyclical,
  ylab = "Gross Domestic Fluctuations Around Pure Linear Trend",
  xlab = "Time",
  main = "Cyclical Component",
  type = "l",
  lty = 1
)
abline(0, 0)

CPIdata <- read.csv("CPIAUCSL.csv", header = TRUE)

head(CPIdata)

summary(CPIdata)

#Ensure for a numeric class 

class(CPIdata$CPIAUCSL)

#plot data over time, i.e. the data in q1 variable by year

plot.ts(CPIdata$CPIAUCSL)

#This section converts the original table into a time series
#to get it ready for the decomposition
CPIdata.ts <- ts(CPIdata$CPIAUCSL, start=1975, frequency = 12)

#The series is decomposed through the 'decompose' function
dec.CPIdata.ts <- decompose(CPIdata.ts)

#Here we produce the plot of the components
plot(dec.CPIdata.ts)

#And finally here we ask R to show the decomposed data,
#component by component

dec.CPIdata.ts

#Identify existing cyclical component
#Start by plotting trend component produced through decomposition function
plot(dec.CPIdata.ts$trend)

#now convert decomposition output to data frame
dec.CPIdata=with(dec.CPIdata.ts, data.frame(observed=x, trend=trend,
                                            seasonal=seasonal, random=random))
#Check if there are 'NA' missing values
head(dec.CPIdata)

#Excluded NA variables
dec.CPIexcdata <- dec.CPIdata[complete.cases(dec.CPIdata),]

#Create an time variable starting at t=1, just a  time indicator
dec.CPIexcdata$time <- seq.int(nrow(dec.CPIexcdata))
head(dec.CPIexcdata,10)


#Estimate regression, output residuals and predicted linear trend
cyc.lm <- 
  lm(dec.CPIexcdata$trend ~ dec.CPIexcdata$time)
cyc.cyclical <- resid(cyc.lm)
cyc.puretrend <- predict(cyc.lm)

#Plot Pure Linear Trend Component
plot(
  dec.CPIexcdata$time,
  cyc.puretrend,
  ylab = "Consumer Price Index",
  xlab = "Time",
  main = "Purely Linear Trend Component",
  type = "l",
  lty = 1
)
abline(0, 0)


#Plot Cyclical Component
plot(
  dec.CPIexcdata$time,
  cyc.cyclical,
  ylab = " CPI Fluctuations Around Pure Linear Trend",
  xlab = "Time",
  main = "Cyclical Component",
  type = "l",
  lty = 1
)
abline(0, 0)

