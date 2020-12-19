#ARDL PRORGAM


#install.packages("dynamac")
#library(dynamac)
#install.packages("urca")
#library(urca)

setwd("C:/Users/tiffa/Documents/FINA 6271/Prof_Mejia/Session 5/Assignment 3 12_03_2020")
assign3 <-read.csv("Assign3.csv", header=TRUE)

#2. Look at data using the 'head' function.'
head(assign3)


#3. Plot the variables in the data set.
ts.plot(assign3$EMRATIO_CHG)
ts.plot(assign3$PAYEMS_CHG)
ts.plot(assign3$RGDP)
#4. Use the ADF unit root test on 'RGDP'. If you can't reject the null
#hypothesis of non-stationarity, you'll need to difference the series. If
#the p-value is low and the test statistic is high, you'll reject the
#hypothesis of non-stationarity, and you won't need to difference the series.
summary(ur.df(assign3$EMRATIO_CHG, type = c("none"), lags = 1))
summary(ur.df(assign3$PAYEMS_CHG, type = c("none"), lags = 1))
summary(ur.df(assign3$RGDP, type = c("none"), lags = 1))


#In the case of the 'RGDP' variable, the non-stationarity hypothesis was
#not rejected. So here is a second ADF test for the differenced series. It now
#shows that we can reject the hypothesis of non-stationarity. So, the 'RGDP'
#variable would have to be differenced to build the ardl model.
summary(ur.df(diff(assign3$PAYEMS_CHG), type = c("none"), lags = 1))
summary(ur.df(diff(assign3$EMRATIO_CHG), type = c("none"), lags = 1))


#5. For illustration, this step estimates a first ARDL model. Let's call it
#version 1. Here you can see that by setting 'ec=FALSE', you request
#that the dependent variable, 'RGDP', be modeled in differences. This is
#because you couldn't reject the hypothesis of non-stationarity above when
#you run the ADF test on the non-differenced 'RGDP' variable.
mypredicted1 <- dynardl(RGDP ~ EMRATIO_CHG + PAYEMS_CHG, data = assign3, 
                      lags = list("RGDP" = 1, "EMRATIO_CHG" = 1, "PAYEMS_CHG" = 1),
                      diffs = c("EMRATIO_CHG", "PAYEMS_CHG"), 
                      ec = FALSE, simulate = FALSE)

#Show formatted results from the previous step.
summary(mypredicted1)


#You can also calculate one standard deviation of the shock variable to have
#a sense of the magnitude of the shock. 

sd(assign3$PAYEMS_CHG)
mean(assign3$PAYEMS_CHG)

#6. After building the model, simulate the impulse response functions for 
#the second version of the model. We specify the 'PAYEMS_CHG' variable 
#as the 'shock variable', that is the one we will shock to see how the 
#dependent variable, 'RGDP', responds. Remember, the default setting 
#(very convenient) is to use a shock of 1 standard deviation 
#for whatever shock variable we specify
set.seed(5000)
mypredicted2 <- dynardl(RGDP ~ EMRATIO_CHG + PAYEMS_CHG, data = assign3, 
                         lags = list("RGDP" = 1, "EMRATIO_CHG" = 1, "PAYEMS_CHG" = 1),
                         diffs = c("EMRATIO_CHG", "PAYEMS_CHG"), 
                         lagdiffs = list("RGDP" = 1),
                         ec = TRUE, simulate = TRUE, range = 40,
                         shockvar = "PAYEMS_CHG")

#Show simulation results
summary(mypredicted2$model)

#Plot response function. This output provides graphical evidence of the
#magnitude and persistence (number of periods) of the shock. It shows
#how much the dependent variable changes, and for how long that change
#persists, in response to a shock in one standard deviation (whatever
#the standard deviation calculation showed above) in the shock variable.
dynardl.simulation.plot(mypredicted2, type = "area", response = "levels")

#This information is useful to forecasters, financial analysts, risk managers,
#regulartors and other market participants, to understand the effect of
#changes or decisions they make.


