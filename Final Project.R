library(fpp3)

## Dataset chosen here depicts imports made by USA from Canada. 
## Given Canada is a third largest importer for USA 

temp=readr::read_csv("IMPCA.csv")
Import=temp%>%mutate(Date=yearmonth(ymd(Date)))%>%
  as_tsibble(index=Date)
random = Import%>%filter_index("1985 Jan"~"2022 Oct")
Import%>%autoplot()
random

## We can see traces of seasonality in this dataset and the imports in the given
## period have peaked in March and June.

random%>%gg_subseries(IMPCA)
random%>%gg_season(IMPCA)

##Certain behavioural characetristic of data show that the volatility of imports
##has been constantly changing and the trend cannot be said to be linear, 
##further sudden declines in import has to be taken care of.
## S0 we need to use BOX-COX transformation.
##The lambda value is around 0.03, so its better to take log transformation and 
##we can see much improvement in the tendency in terms of linear trends

lambda=random%>%features(IMPCA,guerrero)%>%pull(lambda_guerrero)
lambda


Import%>%autoplot(log(IMPCA))

## ARIMA

##Unit root has been computed for the dataset to check whether differencing is required
## to bring the data to a stationary state
## KPSS test confirms the same

random%>%features(log(IMPCA),unitroot_nsdiffs)
random%>%features(log(IMPCA),unitroot_ndiffs)
random%>%features(log(IMPCA),unitroot_kpss)


## We can see in th ACF and PACF there are non zero values across all lags. 
##So a presence of autoregressive and moving average piece seems possible.
##At 12th,24th, 36th we can see predictable pattern that seem significant in ACF
## The first two lags are significant in PACF with seasonal lags also showing significant 
##spikes untill 60th lag. The decay somewhat is quite complex in ACF as well as PACF.
## guess:(2,1,1)*(3,0,3),(0,1,1)*(3,0,3),(0,0,1)*(3,0,3)

# Model: ARIMA(2,1,1)(3,0,3)
# AIC=-1285.16   AICc=-1284.66   BIC=-1244

# M2
# Model: ARIMA(0,1,1)(3,0,3)[12]
# AIC=-1282.84   AICc=-1282.52   BIC=-1249.91

# M3
# Model: ARIMA(0,1,1)(0,0,3)[12]
# AIC=-1130.2   AICc=-1130.06   BIC=-1109.62

# So if we compare the values of AIC and BIC to find the best model.
# we can easily reject the model M3,
# But if we compare the value of AIC and BIC of models M1 and M2 then we can see that 
# In one model M1 AIC is less and in Model M2 BIC is less so 
# In that case we will prefer the value of BIC and
# The model with a low BIC value is considered as a best model 
# So in this case our best model is M2.


random%>%gg_tsdisplay(difference(log(IMPCA)),lag_max=100,
                      plot_type="partial")

m1=random%>%model(ARIMA(log(IMPCA)~0+pdq(2,1,1)+PDQ(3,0,3)))%>%report()
m2=random%>%model(ARIMA(log(IMPCA)~0+pdq(0,1,1)+PDQ(3,0,3)))%>%report()
m3=random%>%model(ARIMA(log(IMPCA)~0+pdq(0,1,1)+PDQ(0,0,3)))%>%report()





m1%>%gg_tsresiduals(lag_max=100)
m2%>%gg_tsresiduals(lag_max=100)
m3%>%gg_tsresiduals(lag_max=100)
##
##Ljung BOX confirms independence of residuals with its high p value.
##So we can go ahead with the prediction
m2%>%augment()
m2%>%augment()%>%features(.innov,ljung_box,lag=100,dof=7)
m2%>%augment()%>%features(.innov,box_pierce,lag=100,dof=7)
m2%>%forecast(h=4)%>%
  autoplot(Import%>%filter_index("2018 Jan"~"2023 Feb"), level=95)









# NEURAL NETWORK


random%>%gg_tsdisplay(difference(log(IMPCA))%>% 
                        difference(), plot_type= "partial", lag_max=100)

## There is no provision for moving average piece in neural network and differencing
## So we difference the data as per our need and choose p and P values for neural network.
## First 2 lags in differenced data look more significant 
##and remaining look marginally above the confidence band so p =2 
##and adding the difference makes it 3. Similarly spikes around 12th lag dictate P=1

modelNET= random%>%model(NNETAR(log(IMPCA)~AR(p=3,P=1), n_networks=400))
a=modelNET%>%forecast(h=4,times=400, bootstrap=TRUE)
a%>%autoplot(Import%>%filter_index("2021 Jan"~"2023 Feb"), level = 95)

## In total we had 6 significant spike some of them marginally high. 
#Based on this we can revise p=7, and keep P=1 

modelNET1= random%>%model(NNETAR(log(IMPCA)~AR(p=7,P=1), n_networks=400))
b=modelNET1%>%forecast(h=4,times=400, bootstrap=TRUE)
b%>%autoplot(Import%>%filter_index("2021 Jan"~"2023 Feb"),level= 95)



# Forecasting for a period of 6 months ahead


foremodel=Import%>%model(ARIMA(log(IMPCA)~0+pdq(0,1,1)+PDQ(3,0,3)))%>%report()

foremodel%>%forecast(h=6)%>%
  autoplot(Import%>%filter_index("2018 Jan"~"2023 Feb"), level=95)
