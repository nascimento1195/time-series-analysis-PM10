# Reading data
library(readxl)
dados<-read_excel("basepoluicao.xls")
attach(dados)

# Plotting data
library(ggplot2)
theme_set=(theme_minimal())

ggplot(data = dados, aes(x = DATA, y = PM10))+
  geom_line(color = "#00AFBB", size = 1)

# Transforming data into a time series object
POL<-ts(PM10)

# Augmented Dickey-Fuller Test for stationarity
library(tseries)
adf.test(POL)

# Since the p-value of the test was significative at a 5% level of significance,
# we reject the null hypothesis of existence of a unitary root and conclude that
# we have statistics evidences to prove the stationarity of the process.

# KPSS Test for stationarity
kpss.test(POL)

# Since the p-value of the test was significative at a 5% level of significance,
# we reject the null hypothesis of stationarity of the series and conclude that
# we have statistics evidences that the process is not stationary.

# We have divergence between the tests, so we will choose to apply the first 
# difference of the series in order to get the stationarity factor confirmed.

first_difference<-diff(POL)

adf.test(first_difference)
kpss.test(first_difference)

# Applying both test in the first difference series, we get the same result from 
# both and now we can procede our analysis.

# The next step is to decide if our series has the multiplicative or addictive 
# characteristic. One way to do this is grouping the series into minor groups of 
# any size (the number here generally doesn't impact much, we will use groups of 2),
# calculate the mean and stardand deviation inside this minor groups and then plot
# a graphic of the calculated means agains the calculated standard deviations.

# Calculating mean
mean_vector<-vector()
j<-1
vet<-POL
for(i in 1:(length(first_difference)/2)){
  mean_vector[i]<-(vet[j]+vet[j+1])/2
  j<-j+2
}

# Calculating standard deviation
std_deviation_vector <- vector()
z<-1
for(i in 1:(length(first_difference)/2)){
  std_deviation_vector[i]<-sd(c(vet[z],vet[z+1]))
  z<-z+2
}

plot(mean_vector, std_deviation_vector)
cor(mean_vector, std_deviation_vector)

# Since we do not seem to have a strong correlation coefficient between the two
# vectors generated, we can adjust an ARIMA addictive model. If the coefficient or
# the graphic here had suggested a strong correlation, we would have evidences of
# a multiplicative factor in the series and it would be necessary to apply some
# transformation to take off this factor (log function here uses to work well) so
# we can adjust the ARIMA model.

# There are no clear rules to adjust the parametes of an ARIMA model. There are 
# tough some tips like plotting autocorrelation and partial autocorrelation functions.
# I prefer to test some combinations of parameters and choose the one that have them
# all significative. If more than one series satisfy this condition, then we take the
# simpler one (with less parameters) or the one with the minor AIC.

# Since we will model the first difference of the series, our middle parameter 'q'
# (ARIMA(p,d,q)) has to be 1. This way we will vary only 'p' and 'q' from 0 to 2.

# ARIMA(0,1,1) 
model1<-arima(POL, order = c(0,1,1)) 
t1<-0.1645/0.0776
dt(t1, length(POL)) 
# Model is a candidate, since all parametes are significative (5% significance).

# ARIMA(0,1,2) 
model2<-arima(POL, order = c(0,1,2))
t1<--0.2161/0.0588
t2<--0.5556/0.0615
dt(t1, length(POL)) 
dt(t2, length(POL)) 
# Model is a candidate, since all parametes are significative (5% significance).

# ARIMA(1,1,0) 
model3<-arima(POL, order = c(1,1,0)) 
t1<-0.0562/0.0523
dt(t1, length(POL))
# Model is not a candidate, since at least one of the parametes are not 
# significative (5% significance).

# ARIMA(1,1,1) 
model4<-arima(POL, order = c(1,1,1)) 
t1<--0.4332/0.1264
t2<-0.6101/0.1062
dt(t1, length(POL))
dt(t2, length(POL)) 
# Model is a candidate, since all parametes are significative (5% significance).

# ARIMA(1,1,2) 
model5<-arima(POL, order = c(1,1,2)) 
t1<-0.4113/0.0692
t2<--0.4975/0.064
t3<--0.4334/0.0554
dt(t1, length(POL))
dt(t2, length(POL)) 
dt(t3, length(POL)) 
# Model is a candidate, since all parametes are significative (5% significance).

# ARIMA(2,1,0)
model6<-arima(POL, order = c(2,1,0)) 
t1<-0.0781/0.0486
t2<--0.3746/0.0485
dt(t1, length(POL))
dt(t2, length(POL))
# Model is not a candidate, since at least one of the parametes are not 
# significative (5% significance).

# ARIMA(2,1,1)
model7<-arima(POL, order = c(2,1,1)) 
t1<-0.8161/0.0528
t2<--0.2878/0.0516
t3<--0.94/0.0226
dt(t1, length(POL))
dt(t2, length(POL)) 
dt(t3, length(POL)) 
# Model is a candidate, since all parametes are significative (5% significance).

# ARIMA(2,1,2)
model8<-arima(POL, order = c(2,1,2)) 
t1<-0.4864/0.124
t2<--0.0743/0.0999
t3<--0.5645/0.1156
t4<--0.3643/0.1126
dt(t1, length(POL))
dt(t2, length(POL)) 
dt(t3, length(POL)) 
dt(t4, length(POL)) 
# Model is not a candidate, since at least one of the parametes are not 
# significative (5% significance).

# Since we got some models with all parameters significant, we will get the one with
# minor AIC and check for its residuals properties.

# MODEL1 - AIC = 3158.49
# MODEL2 - AIC = 3100.21
# MODEL4 - AIC = 3150.45
# MODEL5 - AIC = 3074.56
# MODEL7 - AIC = 3082.24

# Let's check for normality and independency of the residuals.
hist(model5$residuals)
cpgram(model5$residuals)
Box.test(model5$residuals, type=c("Box-Pierce"))

