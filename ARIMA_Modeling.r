#import libraries 
library('forecast')
library('stats')
library('tseries')

#read/attach data
data=read.csv("/Users/katelassiter/Downloads/303/312/497T/Variable2.csv")
attach(data)

#generate time series
Expenditure_Quarterly=ts(Expenditure, frequency=4)
print(Expenditure_Quarterly)
plot(Expenditure_Quarterly)

#Create size restrictions
N=length(Expenditure_Quarterly)
WithinSampleLength=N-8
WithinSample=ts(Expenditure_Quarterly[1:WithinSampleLength],frequency=4,start=c(2006,4))
print(WithinSample)
PostTest=Expenditure_Quarterly[(WithinSampleLength+1):N]
print(PostTest)

#Dickey-Fuller Test for Stationarity
#This tests for a unit root process
#adf is augmented dickey fuller test
adf.test(WithinSample,alternative='stationary')
#A large p-value is indicative of a unit root process, that the series is not stationary
#To deal with a not stationary series, we use differencing
#Reject null, must difference

#Differencing the equation
WithinSampleDiff1=diff(WithinSample, differences=1)
plot(WithinSampleDiff1)

#Dickey-Fuller Test for Stationarity on First-Differenced Series
adf.test(WithinSampleDiff1,alternative='stationary')
#Still reject null, difference again 

#Differencing the equation
WithinSampleDiff2=diff(WithinSample, differences=2)
plot(WithinSampleDiff2)

#Dickey-Fuller Test for Stationarity on Second-Differenced Series
adf.test(WithinSampleDiff2,alternative='stationary')
#Now fail to reject null, there is now not a unit root process and the series is stationary

#Getting Partial Autocorrelation Function (PACF) and Autocorrelation Function(ACF)
#Main is how you name a plot
Acf(WithinSampleDiff2,main='This is my Autcorrelation Function')
Pacf(WithinSampleDiff2,main='This is my Partial Autcorrelation Function')
#From visual inspection, it appears to tail off after 3
#Guess: IMA (2,4)

#Now, enough of that fun. Let's do auto-ARIMA. It is selected by running tons of possible combinations of a model and choosing the one with the smallest AIC (Akaike Information Criteria)
auto.arima(WithinSample,seasonal=TRUE)
#This Auto Arima I disagree with, it doesn't make sense it only differenced it once when it is still not stationary yet

#In response I forced it to allow me to difference twice
ARIMAWithin=auto.arima(WithinSample,d=2,seasonal=TRUE)
print(ARIMAWithin)

#Now let's forecast with this Auto Arima 
#h means steps ahead
ARIMAWithinYhats=forecast(ARIMAWithin,h=8)
print(ARIMAWithinYhats)

#Getting post-sample residuals
ARIMAResidualsTest=ARIMAWithinYhats$mean-PostTest
print(ARIMAResidualsTest)

#Getting RMSE
sqrt(mean(ARIMAResidualsTest^2))
