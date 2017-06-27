

####### Reading the data set into R 

retail<-read.csv("Global Superstore.csv")

###### cheking the structure of the data set 

str(retail)


############################ Data Understanding & Data Preparation #########################

levels(retail$Segment)

#"Consumer"    "Corporate"   "Home Office"

levels(retail$Market)

# "Africa" "APAC"   "Canada" "EMEA"   "EU"     "LATAM"  "US"  

levels(retail$Order.Date)

str(retail$Order.Date)

retail$Order.Date<-as.Date(retail$Order.Date,"%d-%m-%Y")

retail$MonthYear <- format(as.Date(retail$Order.Date, "%Y-%m-%d"), "%Y-%m")

## Segment-Consumer,Market-Africa

Africaconsumer <- subset(retail,retail$Segment=="Consumer" & retail$Market=="Africa")

Africaconsumer <-Africaconsumer[order(Africaconsumer$Order.Date),]

AfricaconsumerAg <- aggregate(x = Africaconsumer[c("Sales","Quantity","Profit")], 
                              FUN =  sum,by = list(Order.Date = Africaconsumer$MonthYear))

Africaconsumer_cof <- sd(AfricaconsumerAg$Profit)/mean(AfricaconsumerAg$Profit)

Africaconsumer_cof


## Segment-Corporate,Market-Africa
Africacorporate <-subset(retail,retail$Segment=="Corporate" & retail$Market=="Africa")
Africacorporate <-Africacorporate[order(Africacorporate$Order.Date),]
AfricacorporateAg<-aggregate(x = Africacorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                              by = list(Order.Date = Africacorporate$MonthYear))

Africacorporate_cof<-sd(AfricacorporateAg$Profit)/mean(AfricacorporateAg$Profit)
Africacorporate_cof

## Segment-Home Office,Market-Africa
Africahomeoffice <-subset(retail,retail$Segment=="Home Office" & retail$Market=="Africa")
Africahomeoffice <-Africahomeoffice[order(Africahomeoffice$Order.Date),]
AfricahomeofficeAg<-aggregate(x = Africahomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                               by = list(Order.Date = Africahomeoffice$MonthYear))
Africahomeoffice_cof<-sd(AfricahomeofficeAg$Profit)/mean(AfricahomeofficeAg$Profit)
Africahomeoffice_cof


## Segment-Consumer,Market-APAC
APACconsumer <-subset(retail,retail$Segment=="Consumer" & retail$Market=="APAC")
APACconsumer <-APACconsumer[order(APACconsumer$Order.Date),]
APACconsumerAg<-aggregate(x = APACconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = APACconsumer$MonthYear))
APACconsumer_cof<-sd(APACconsumerAg$Profit)/mean(APACconsumerAg$Profit)
APACconsumer_cof

## Segment-Corporate,Market-APAC

APACcorporate <-subset(retail,retail$Segment=="Corporate" & retail$Market=="APAC")
APACcorporate<-APACcorporate[order(APACcorporate$Order.Date),]
APACcorporateAg <-aggregate(x = APACcorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = APACcorporate$MonthYear))
APACcorporate_cof<-sd(APACcorporateAg$Profit)/mean(APACcorporateAg$Profit)
APACcorporate_cof

## Segment-Home Office,Market-APAC

APAChomeoffice <-subset(retail,retail$Segment=="Home Office" & retail$Market=="APAC")
APAChomeoffice <-APAChomeoffice[order(APAChomeoffice$Order.Date),]
APAChomeofficeAg<-aggregate(x = APAChomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = APAChomeoffice$MonthYear))
APAChomeoffice_cof<-sd(APAChomeofficeAg$Profit)/mean(APAChomeofficeAg$Profit)
APAChomeoffice_cof

## Segment-Consumer,Market-Canada
Canadaconsumer<-subset(retail,retail$Segment=="Consumer" & retail$Market=="Canada")
Canadaconsumer<-Canadaconsumer[order(Canadaconsumer$Order.Date),]
CanadaconsumerAg<-aggregate(x = Canadaconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = Canadaconsumer$MonthYear))
Canadaconsumer_cof<-sd(CanadaconsumerAg$Profit)/mean(CanadaconsumerAg$Profit)
Canadaconsumer_cof

## Segment-Corporate,Market-Canada
Canadacorporate <-subset(retail,retail$Segment=="Corporate" & retail$Market=="Canada")
Canadacorporate <-Canadacorporate[order(Canadacorporate$Order.Date),]
CanadacorporateAg <-aggregate(x = Canadacorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = Canadacorporate$MonthYear))
Canadacorporate_cof<-sd(CanadacorporateAg$Profit)/mean(CanadacorporateAg$Profit)
Canadacorporate_cof

## Segment-Home Office,Market-Canada
Canadahomeoffice<-subset(retail,retail$Segment=="Home Office" & retail$Market=="Canada")
Canadahomeoffice<-Canadahomeoffice[order(Canadahomeoffice$Order.Date),]
CanadahomeofficeAggr<-aggregate(x = Canadahomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = Canadahomeoffice$MonthYear))
Canadahomeoffice_cof<-sd(CanadahomeofficeAggr$Profit)/mean(CanadahomeofficeAggr$Profit)
Canadahomeoffice_cof


## Segment-Consumer,Market-EMEA
EMEAconsumer<-subset(retail,retail$Segment=="Consumer" & retail$Market=="EMEA")
EMEAconsumer<-EMEAconsumer[order(EMEAconsumer$Order.Date),]
EMEAconsumerAggr<-aggregate(x = EMEAconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EMEAconsumer$MonthYear))
EMEAconsumer_cof<-sd(EMEAconsumerAggr$Profit)/mean(EMEAconsumerAggr$Profit)
EMEAconsumer_cof


## Segment-Corporate,Market-EMEA
EMEAcorporate<-subset(retail,retail$Segment=="Corporate" & retail$Market=="EMEA")
EMEAcorporate<-EMEAcorporate[order(EMEAcorporate$Order.Date),]
EMEAcorporateAggr<-aggregate(x = EMEAcorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EMEAcorporate$MonthYear))
EMEAcorporate_cof<-sd(EMEAcorporateAggr$Profit)/mean(EMEAcorporateAggr$Profit)
EMEAcorporate_cof

## Segment-Home Office,Market-EMEA
EMEAhomeoffice<-subset(retail,retail$Segment=="Home Office" & retail$Market=="EMEA")
EMEAhomeoffice<-EMEAhomeoffice[order(EMEAhomeoffice$Order.Date),]
EMEAhomeofficeAggr<-aggregate(x = EMEAhomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EMEAhomeoffice$MonthYear))
EMEAhomeoffice_cof<-sd(EMEAhomeofficeAggr$Profit)/mean(EMEAhomeofficeAggr$Profit)
EMEAhomeoffice_cof


## Segment-Consumer,Market-EU
EUconsumer<-subset(retail,retail$Segment=="Consumer" & retail$Market=="EU")
EUconsumer<-EUconsumer[order(EUconsumer$Order.Date),]
EUconsumerAggr<-aggregate(x = EUconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EUconsumer$MonthYear))
EUconsumer_cof<-sd(EUconsumerAggr$Profit)/mean(EUconsumerAggr$Profit)
EUconsumer_cof




## Segment-Corporate,Market-EU
EUcorporate<-subset(retail,retail$Segment=="Corporate" & retail$Market=="EU")
EUcorporate<-EUcorporate[order(EUcorporate$Order.Date),]
EUcorporateAggr<-aggregate(x = EUcorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EUcorporate$MonthYear))
EUcorporate_cof<-sd(EUcorporateAggr$Profit)/mean(EUcorporateAggr$Profit)
EUcorporate_cof


## Segment-Home Office,Market-EU
EUhomeoffice<-subset(retail,retail$Segment=="Home Office" & retail$Market=="EU")
EUhomeoffice<-EUhomeoffice[order(EUhomeoffice$Order.Date),]
EUhomeofficeAggr<-aggregate(x = EUhomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = EUhomeoffice$MonthYear))
EUhomeoffice_cof<-sd(EUhomeofficeAggr$Profit)/mean(EUhomeofficeAggr$Profit)
EUhomeoffice_cof


## Segment-Consumer,Market-LATAM
LATAMconsumer<-subset(retail,retail$Segment=="Consumer" & retail$Market=="LATAM")
LATAMconsumer<-LATAMconsumer[order(LATAMconsumer$Order.Date),]
LATAMconsumerAggr<-aggregate(x = LATAMconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = LATAMconsumer$MonthYear))
LATAMconsumer_cof<-sd(LATAMconsumerAggr$Profit)/mean(LATAMconsumerAggr$Profit)
LATAMconsumer_cof

## Segment-Corporate,Market-LATAM
LATAMcorporate<-subset(retail,retail$Segment=="Corporate" & retail$Market=="LATAM")
LATAMcorporate<-LATAMcorporate[order(LATAMcorporate$Order.Date),]
LATAMcorporateAggr<-aggregate(x = LATAMcorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = LATAMcorporate$MonthYear))
LATAMcorporate_cof<-sd(LATAMcorporateAggr$Profit)/mean(LATAMcorporateAggr$Profit)
LATAMcorporate_cof


## Segment-Home Office,Market-LATAM
LATAMhomeoffice<-subset(retail,retail$Segment=="Home Office" & retail$Market=="LATAM")
LATAMhomeoffice<-LATAMhomeoffice[order(LATAMhomeoffice$Order.Date),]
LATAMhomeofficeAggr<-aggregate(x = LATAMhomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = LATAMhomeoffice$MonthYear))
LATAMhomeoffice_cof<-sd(LATAMhomeofficeAggr$Profit)/mean(LATAMhomeofficeAggr$Profit)
LATAMhomeoffice_cof

## Segment-Consumer,Market-US
USconsumer<-subset(retail,retail$Segment=="Consumer" & retail$Market=="US")
USconsumer<-USconsumer[order(USconsumer$Order.Date),]
USconsumerAggr<-aggregate(x = USconsumer[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = USconsumer$MonthYear))
USconsumer_cof<-sd(USconsumerAggr$Profit)/mean(USconsumerAggr$Profit)
USconsumer_cof

## Segment-Corporate,Market-US
UScorporate<-subset(retail,retail$Segment=="Corporate" & retail$Market=="US")
UScorporate<-UScorporate[order(UScorporate$Order.Date),]
UScorporateAggr<-aggregate(x = UScorporate[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = UScorporate$MonthYear))
UScorporate_cof<-sd(UScorporateAggr$Profit)/mean(UScorporateAggr$Profit)
UScorporate_cof



## Segment-Home Office,Market-US
UShomeoffice<-subset(retail,retail$Segment=="Home Office" & retail$Market=="US")
UShomeoffice<-UShomeoffice[order(UShomeoffice$Order.Date),]
UShomeofficeaggr<-aggregate(x = UShomeoffice[c("Sales","Quantity","Profit")], FUN =  sum, 
                                by = list(Order.Date = UShomeoffice$MonthYear))
UShomeoffice_cof<-sd(UShomeofficeaggr$Profit)/mean(UShomeofficeaggr$Profit)
UShomeoffice_cof



## Top 5 most profitable Segments ##

## Consumer EU
## ConsumerAPAc
## LATAMconsumer
## CorporateAPAC
## EUcorporate

###################### Model 1 : Consumer EU ###################################

############ 1.Quantity

## Training into train and test Data sets

EUconsumer_timeseries <- EUconsumerAggr[1:42,]

EUconsumer_test <- EUconsumerAggr[43:48,]
 
### Plot the timeseries

timeseries1<-ts(EUconsumer_timeseries$Quantity)

plot.ts(timeseries1)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries <- filter(timeseries1, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries, col=cols[i],lwd=2)
}

width = 2

#### Smoothing the TIme series 

smoothedseries <- filter(timeseries1, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries_month<-as.data.frame(cbind(EUconsumer_timeseries$Order.Date,smoothedseries))

colnames(smoothedseries_month)<-c("Month","Quantity")

View(smoothedseries_month)

smoothedseries_month$Quantity<-as.numeric(as.character(smoothedseries_month$Quantity))

##### Ordering the Data points in time series order

smoothedseries_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

## diff_1<-smoothedseries_month$Quantity[3] - smoothedseries_month$Quantity[2]

## smoothedseries_month$Quantity[1]<-smoothedseries_month$Quantity[2]-diff_1

diff_2<-smoothedseries_month$Quantity[41]-smoothedseries_month$Quantity[40]

smoothedseries_month$Quantity[42]<-smoothedseries_month$Quantity[41]+diff_2


plot.ts(smoothedseries_month$Quantity)

##### Plotting a Graph using regression for the trend and seasonality

lmfit <- lm(Quantity ~ sin(0.4*(Month))*poly(Month,1) + cos (0.4*(Month))*poly(Month,1)+Month , data=smoothedseries_month)

Month <- c(1:42)

trend<- predict(lmfit,data.frame(Month))

lines(smoothedseries_month$Month,trend,col="blue",lwd=2)

### Residuals

resi<-timeseries1-trend

plot(resi, col='red')

acf(resi)

acf(resi, type="partial")

####### Checking for th best model for the residual standard series

library(forecast)

armafit <- auto.arima(resi)

tsdiag(armafit)

armafit

########### Building a model for the residual standardized series 

resi <- ts(resi)

tsresi <- arima(resi, order = c(2,0,2))

forecasetsresi <- as.data.frame(forecast(tsresi, h = 6))

colnames(forecasetsresi)[1] <- "forecast"

forecasetsresi$forecast

EUconsumerAggr[c(43:48),3]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast <- predict(lmfit,data.frame(Month))


##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final1 <- fcast + forecasetsresi$forecast 

###################### Model Creation using Arima ##################

autoarima <- auto.arima(timeseries1)
autoarima 

#p,d,q ARIMA(2,1,0)

tsdiag(autoarima)

plot(autoarima$x, col="black")

plot(timeseries1)

lines(fitted(autoarima), col="blue")

Arimaforecast <- data.frame(forecast.Arima(autoarima, h = 6))

plot(forecast.Arima(autoarima, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final1,EUconsumerAggr[c(43:48),3])

#####MAPE Value for ARIMA#########

accuracy(Arimaforecast$Point.Forecast,EUconsumerAggr[43:48,3])

############################ 2. Sales ####################################################

## Training into train and test Data sets

EUconsumer_timeseries11 <- EUconsumerAggr[1:42,]

EUconsumer_test11 <- EUconsumerAggr[43:48,]

### Plot the timeseries

timeseries11<-ts(EUconsumer_timeseries11$Sales)

plot.ts(timeseries11)

## Smoothening

cols<-c("red","green","blue", "yellow","Brown")

for(i in seq(2:5))
{
  smoothedseries11 <- filter(timeseries11, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries11, col=cols[i],lwd=2)
}

width = 3

#### Smoothing the TIme series 

smoothedseries11 <- filter(timeseries11, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries11, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries11_month<-as.data.frame(cbind(EUconsumer_timeseries11$Order.Date,smoothedseries11))

colnames(smoothedseries11_month)<-c("Month","Sales")

View(smoothedseries11_month)

smoothedseries11_month$Sales<-as.numeric(as.character(smoothedseries11_month$Sales))

##### Ordering the Data points in time series order

smoothedseries11_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries11_month$Sales[3] - smoothedseries11_month$Sales[2]

smoothedseries11_month$Sales[1]<-smoothedseries11_month$Sales[2]-diff_1

diff_2<-smoothedseries11_month$Sales[41]-smoothedseries11_month$Sales[40]

smoothedseries11_month$Sales[42]<-smoothedseries11_month$Sales[41]+diff_2

plot.ts(smoothedseries11_month$Sales)

##### Plotting a Graph using regression for the trend11 and seasonality

lmfit11 <- lm(Sales ~ sin(0.4*(Month))*poly(Month,1) + cos (0.4*(Month))*poly(Month,1)+Month , data=smoothedseries11_month)

Month <- c(1:42)

trend11<- predict(lmfit11,data.frame(Month))

lines(smoothedseries11_month$Month,trend11,col="blue",lwd=2)

### Resi11duals

resi11<-timeseries11-trend11

plot(resi11, col='red')

acf(resi11)

acf(resi11, type="partial")

####### Checking for th best model for the resi11dual standard series

library(forecast)

armafit11 <- auto.arima(resi11)

tsdiag(armafit11)

armafit11


########### Building a model for the resi11dual standardized series 

resi11 <- ts(resi11)

tsresi11 <- arima(resi11, order = c(1,0,1))

forecasetsresi11 <- as.data.frame(forecast(tsresi11, h = 6))

colnames(forecasetsresi11)[1] <- "forecast"

forecasetsresi11$forecast

EUconsumerAggr[c(43:48),2]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast11 <- predict(lmfit11,data.frame(Month))

##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final11 <- fcast11 + forecasetsresi11$forecast 

###################### Model Creation using Arima ##################

autoarima11 <- auto.arima(timeseries11)
autoarima11 


tsdiag(autoarima11)

plot(autoarima11$x, col="black")

plot(timeseries11)

lines(fitted(autoarima11), col="blue")

Arimaforecast <- data.frame(forecast.Arima(autoarima11, h = 6))

plot(forecast.Arima(autoarima11, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final11,EUconsumerAggr[c(43:48),2])

#####MAPE Value for ARIMA#########

accuracy(EUconsumerAggr[43:48,2], Arimaforecast$Point.Forecast)


##########################################################################################


########################### MOdel 2 : APAcConsumer #######################################

########### 1. Quantity

## Training into train and test Data sets

APAcConsumer_timeseries2  <-  APACconsumerAg[1:42,]

APAcConsumer_test <- APACconsumerAg[43:48,]

### Plot the timeseries2

timeseries2 <-ts(APAcConsumer_timeseries2$Quantity)

plot.ts(timeseries2)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries2 <- filter(timeseries2, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries2, col=cols[i],lwd=2)
}

width = 2

#### Smoothing the TIme series 

smoothedseries2 <- filter(timeseries2, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries2, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries2_month<-as.data.frame(cbind(APAcConsumer_timeseries2$Order.Date,smoothedseries2))

colnames(smoothedseries2_month)<-c("Month","Quantity")

View(smoothedseries2_month)

smoothedseries2_month$Quantity<-as.numeric(as.character(smoothedseries2_month$Quantity))

##### Ordering the Data points in time series order

smoothedseries2_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

## diff_1<-smoothedseries2_month$Quantity[3] - smoothedseries2_month$Quantity[2]

## smoothedseries2_month$Quantity[1]<-smoothedseries2_month$Quantity[2]-diff_1

diff_2<-smoothedseries2_month$Quantity[41]-smoothedseries2_month$Quantity[40]

smoothedseries2_month$Quantity[42]<-smoothedseries2_month$Quantity[41]+diff_2

plot.ts(smoothedseries2_month$Quantity)

##### Plotting a Graph using regression for the trend and seasonality

lmfit2 <- lm(Quantity ~ sin(0.5*(Month))*poly(Month,1) + cos (0.5*(Month))*poly(Month,1)+ Month, data=smoothedseries2_month)

Month <- c(1:42)

trend2 <- predict(lmfit2,data.frame(Month))

lines(smoothedseries2_month$Month,trend2,col="blue",lwd=2)

### Resi1duals

resi1<-timeseries2-trend2

plot(resi1, col='red')

acf(resi1)

acf(resi1, type="partial")

####### Checking for th best model for the resi1dual standard series

library(forecast)

armafit1 <- auto.arima(resi1)

tsdiag(armafit1)

armafit1

plot(armafit1$x, col="black")

lines(fitted(armafit1), col="red")

########### Building a model for the resi1dual standardized series 

resi1 <- ts(resi1)

tsresi1 <- arima(resi1, order = c(2,4,2))

forecasetsresi1 <- as.data.frame(forecast(tsresi1, h = 6))

colnames(forecasetsresi1)[1] <- "forecast"

forecasetsresi1$forecast

APACconsumerAg[c(43:48),3]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast <- predict(lmfit2,data.frame(Month))

##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final2 <- fcast + forecasetsresi1$forecast 

###################### Model Creation using Arima ##################

autoarima1 <- auto.arima(timeseries2)
autoarima1 

#p,d,q ARIMA(2,1,0)

tsdiag(autoarima1)

plot(autoarima1$x, col="black")

plot(timeseries2)

lines(fitted(autoarima1), col="blue")

Arimaforecast <- data.frame(forecast.Arima(autoarima1, h = 6))

plot(forecast.Arima(autoarima1, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final2,APACconsumerAg[c(43:48),3])

#####MAPE Value for ARIMA#########

accuracy(APACconsumerAg[43:48,3], Arimaforecast$Point.Forecast)

######### 2 .Sales


## Training into train and test Data sets

APAcConsumer_timeseries21  <-  APACconsumerAg[1:42,]

APAcConsumer_test21 <- APACconsumerAg[43:48,]

### Plot the timeseries21

timeseries21 <-ts(APAcConsumer_timeseries21$Sales)

plot.ts(timeseries21)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries21 <- filter(timeseries21, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries21, col=cols[i],lwd=2)
}

width = 3

#### Smoothing the TIme series 

smoothedseries21 <- filter(timeseries21, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries21, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries21_month<-as.data.frame(cbind(APAcConsumer_timeseries21$Order.Date,smoothedseries21))

colnames(smoothedseries21_month)<-c("Month","Sales")

View(smoothedseries21_month)

smoothedseries21_month$Sales<-as.numeric(as.character(smoothedseries21_month$Sales))

##### Ordering the Data points in time series order

smoothedseries21_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries21_month$Sales[3] - smoothedseries21_month$Sales[2]

smoothedseries21_month$Sales[1]<-smoothedseries21_month$Sales[2]-diff_1

diff_2<-smoothedseries21_month$Sales[41]-smoothedseries21_month$Sales[40]

smoothedseries21_month$Sales[42]<-smoothedseries21_month$Sales[41]+diff_2

plot.ts(smoothedseries21_month$Sales)

##### Plotting a Graph using regression for the trend and seasonality

lmfit21 <- lm(Sales ~ sin(0.5*(Month))*poly(Month,1) + cos (0.5*(Month))*poly(Month,1)+ Month, data=smoothedseries21_month)

Month <- c(1:42)

trend21 <- predict(lmfit21,data.frame(Month))

lines(smoothedseries21_month$Month,trend21,col="blue",lwd=2)

### Resi21duals

resi21<-timeseries21-trend21

plot(resi21, col='red')

acf(resi21)

acf(resi21, type="partial")

####### Checking for th best model for the resi21dual standard series

library(forecast)

armafit21  <- auto.arima(resi21)

tsdiag(armafit21 )

armafit21 

plot(armafit21 $x, col="black")

lines(fitted(armafit21 ), col="red")

########### Building a model for the resi21dual standardized series 

resi21 <- ts(resi21)

tsresi21 <- arima(resi21, order = c(2,2,2))

forecasetsresi21 <- as.data.frame(forecast(tsresi21, h = 6))

colnames(forecasetsresi21)[1] <- "forecast"

forecasetsresi21$forecast

APACconsumerAg[c(43:48),2]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast21  <- predict(lmfit21,data.frame(Month))


##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final21 <- fcast21  + forecasetsresi21$forecast 

###################### Model Creation using Arima ##################

autoarima21 <- auto.arima(timeseries21)
autoarima21 


tsdiag(autoarima21)

plot(autoarima21$x, col="black")

plot(timeseries21)

lines(fitted(autoarima21), col="blue")

Arimaforecast <- data.frame(forecast.Arima(autoarima21, h = 6))

plot(forecast.Arima(autoarima21, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(APACconsumerAg[c(1:42),2],trend21)
accuracy(APACconsumerAg[c(43:48),2],final21)

accuracy(final21,APACconsumerAg[c(43:48),2])

#####MAPE Value for ARIMA#########

accuracy(APACconsumerAg[43:48,2], Arimaforecast$Point.Forecast)



######################## Model 3 LATAMconsumer #############################

########### 1. Quantity 

LATAMconsumer_timeseries3  <-  LATAMconsumerAggr[1:42,]

LATAMconsumer_test <- LATAMconsumerAggr[43:48,]

### Plot the timeseries3

timeseries3 <-ts(LATAMconsumer_timeseries3$Quantity)

plot.ts(timeseries3)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries3 <- filter(timeseries3, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries3, col=cols[i],lwd=2)
}

width = 2

#### Smoothing the TIme series 

smoothedseries3 <- filter(timeseries3, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries3, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries3_month<-as.data.frame(cbind(LATAMconsumer_timeseries3$Order.Date,smoothedseries3))

colnames(smoothedseries3_month)<-c("Month","Quantity")

View(smoothedseries3_month)

smoothedseries3_month$Quantity<-as.numeric(as.character(smoothedseries3_month$Quantity))

##### Ordering the Data points in time series order

smoothedseries3_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

## diff_1<-smoothedseries3_month$Quantity[3] - smoothedseries3_month$Quantity[2]

## smoothedseries3_month$Quantity[1]<-smoothedseries3_month$Quantity[2]-diff_1

diff_2<-smoothedseries3_month$Quantity[41]-smoothedseries3_month$Quantity[40]

smoothedseries3_month$Quantity[42]<-smoothedseries3_month$Quantity[41]+diff_2

plot.ts(smoothedseries3_month$Quantity)

##### Plotting a Graph using regression for the trend and seasonality

lmfit3<- lm(Quantity ~ sin(1*(Month))*poly(Month,1) + cos (1*(Month))*poly(Month,1)+ Month, data=smoothedseries3_month)

Month <- c(1:42)

trend3 <- predict(lmfit3,data.frame(Month))

lines(smoothedseries3_month$Month,trend3,col="blue",lwd=2)

### Resi3duals

resi3<-timeseries3-trend3

plot(resi3, col='red')

acf(resi3)

acf(resi3, type="partial")

####### Checking for th best model for the resi3dual standard series

library(forecast)

armafit3<- auto.arima(resi3)

tsdiag(armafit3)

armafit3

plot(armafit3$x, col="black")

lines(fitted(armafit3), col="red")

########### Building a model for the resi3dual standardized series 

resi3 <- ts(resi3)

tsresi3 <- arima(resi3, order = c(6,2,6))

forecasetsresi3 <- as.data.frame(forecast(tsresi3, h = 6))

colnames(forecasetsresi3)[1] <- "forecast"

forecasetsresi3$forecast

LATAMconsumerAggr[c(43:48),3]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast3 <- predict(lmfit3,data.frame(Month))


##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final3 <- fcast3 + forecasetsresi3$forecast 

###################### Model Creation using Arima ##################

autoarima3 <- auto.arima(timeseries3)
autoarima3 


tsdiag(autoarima3)

plot(autoarima3$x, col="black")

plot(timeseries3)

lines(fitted(autoarima3), col="blue")

Arimaforecast3 <- data.frame(forecast.Arima(autoarima3, h = 6))

plot(forecast.Arima(autoarima3, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final3,LATAMconsumerAggr[c(43:48),3])

#####MAPE Value for ARIMA#########

accuracy(Arimaforecast3$Point.Forecast,LATAMconsumerAggr[43:48,3])

#################### 2. Sales

LATAMconsumer_timeseries31  <-  LATAMconsumerAggr[1:42,]

LATAMconsumer_test31 <- LATAMconsumerAggr[43:48,]

### Plot the timeseries31

timeseries31 <-ts(LATAMconsumer_timeseries31$Sales)

plot.ts(timeseries31)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries31 <- filter(timeseries31, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries31, col=cols[i],lwd=2)
}

width = 3

#### Smoothing the TIme series 

smoothedseries31 <- filter(timeseries31, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries31, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries31_month<-as.data.frame(cbind(LATAMconsumer_timeseries31$Order.Date,smoothedseries31))

colnames(smoothedseries31_month)<-c("Month","Sales")

View(smoothedseries31_month)

smoothedseries31_month$Sales<-as.numeric(as.character(smoothedseries31_month$Sales))

##### Ordering the Data points in time series order

smoothedseries31_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries31_month$Sales[3] - smoothedseries31_month$Sales[2]

smoothedseries31_month$Sales[1]<-smoothedseries31_month$Sales[2]-diff_1

diff_2<-smoothedseries31_month$Sales[41]-smoothedseries31_month$Sales[40]

smoothedseries31_month$Sales[42]<-smoothedseries31_month$Sales[41]+diff_2

plot.ts(smoothedseries31_month$Sales)

##### Plotting a Graph using regression for the trend and seasonality

lmfit31<- lm(Sales ~ sin(0.5*(Month))*poly(Month,2) + cos (0.6*(Month))*poly(Month,2)+ Month, data=smoothedseries31_month)

Month <- c(1:42)

trend31 <- predict(lmfit31,data.frame(Month))

lines(smoothedseries31_month$Month,trend31,col="blue",lwd=2)

### Resi31duals

resi31<-timeseries31-trend31

plot(resi31, col='red')

acf(resi31)

acf(resi31, type="partial")

####### Checking for th best model for the resi31dual standard series

library(forecast)

armafit31<- auto.arima(resi31)

tsdiag(armafit31)

armafit31

plot(armafit31$x, col="black")

lines(fitted(armafit31), col="red")

########### Building a model for the resi31dual standardized series 

resi31 <- ts(resi31)

tsresi31 <- arima(resi31, order = c(2,2,1))

forecasetsresi31 <- as.data.frame(forecast(tsresi31, h = 6))

colnames(forecasetsresi31)[1] <- "forecast"

forecasetsresi31$forecast

LATAMconsumerAggr[c(43:48),2]

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast31 <- predict(lmfit31,data.frame(Month))

##### Total value predicted by the model includes values predicted by the linear regression
##### and the value predicted by the time series model created using ARIMA

final31  <- fcast31 + forecasetsresi31$forecast 

###################### Model Creation using Arima ##################

autoarima31 <- auto.arima(timeseries31)
autoarima31 

tsdiag(autoarima31)

plot(autoarima31$x, col="black")

plot(timeseries31)

lines(fitted(autoarima31), col="blue")

Arimaforecast31 <- data.frame(forecast.Arima(autoarima31, h = 6))

plot(forecast.Arima(autoarima31, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final31,LATAMconsumerAggr[c(43:48),2])

#####MAPE Value for ARIMA#########

accuracy(LATAMconsumerAggr[43:48,2], Arimaforecast31$Point.Forecast)


######################## Model 4 :  CorporateAPAC ###################################
############# 1. quantity 

APACcorporate_timeseries4  <-  APACcorporateAg[1:42,]

APACcorporate_test <- APACcorporateAg[43:48,]

### Plot the timeseries4

timeseries4 <-ts(APACcorporate_timeseries4$Quantity)

plot.ts(timeseries4)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries4 <- filter(timeseries4, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries4, col=cols[i],lwd=2)
}

width = 2

#### Smoothing the TIme series 

smoothedseries4 <- filter(timeseries4, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries4, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries4_month<-as.data.frame(cbind(APACcorporate_timeseries4$Order.Date,smoothedseries4))

colnames(smoothedseries4_month)<-c("Month","Quantity")

View(smoothedseries4_month)

smoothedseries4_month$Quantity<-as.numeric(as.character(smoothedseries4_month$Quantity))

##### Ordering the Data points in time series order

smoothedseries4_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries4_month$Quantity[3] - smoothedseries4_month$Quantity[2]

smoothedseries4_month$Quantity[1]<-smoothedseries4_month$Quantity[2]-diff_1

diff_2<-smoothedseries4_month$Quantity[41]-smoothedseries4_month$Quantity[40]

smoothedseries4_month$Quantity[42]<-smoothedseries4_month$Quantity[41]+diff_2

plot.ts(smoothedseries4_month$Quantity)

##### Plotting a Graph using regression for the trend and seasonality

lmfit4<- lm(Quantity ~ sin(0.3*(Month))*poly(Month,1) + cos (0.3*(Month))*poly(Month,1)+ Month, data=smoothedseries4_month)

Month <- c(1:42)

trend4 <- predict(lmfit4,data.frame(Month))

lines(smoothedseries4_month$Month,trend4,col="blue",lwd=2)

### Resi4duals

resi4<-timeseries4-trend4

plot(resi4, col='red')

acf(resi4)

acf(resi4, type="partial")

### acf plot suggests that the residual series is pure white noise so we only need to
### consider the values predicted by the lmfit.

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast4 <- predict(lmfit4,data.frame(Month))


###################### Model Creation using Arima ##################

autoarima4 <- auto.arima(timeseries4)
autoarima4 

tsdiag(autoarima4)

plot(autoarima4$x, col="black")

plot(timeseries4)

lines(fitted(autoarima4), col="blue")

Arimaforecast4 <- data.frame(forecast.Arima(autoarima4, h = 6))

plot(forecast.Arima(autoarima4, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(APACcorporateAg[c(1:42),3],trend)

accuracy(APACcorporateAg[c(43:48),3],fcast4)

accuracy(trend,APACcorporateAg[c(1:42),3])

accuracy(fcast4,APACcorporateAg[c(43:48),3])

#####MAPE Value for ARIMA#########

accuracy(APACcorporateAg[43:48,3], Arimaforecast4$Point.Forecast)


######### 2. Sales

############# 1. Sales 

APACcorporate_timeseries41  <-  APACcorporateAg[1:42,]

APACcorporate_test41 <- APACcorporateAg[43:48,]

### Plot the timeseries41

timeseries41 <-ts(APACcorporate_timeseries41$Sales)

plot.ts(timeseries41)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries41 <- filter(timeseries41, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries41, col=cols[i],lwd=2)
}

width = 3

#### Smoothing the TIme series 

smoothedseries41 <- filter(timeseries41, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries41, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries41_month<-as.data.frame(cbind(APACcorporate_timeseries41$Order.Date,smoothedseries41))

colnames(smoothedseries41_month)<-c("Month","Sales")

View(smoothedseries41_month)

smoothedseries41_month$Sales<-as.numeric(as.character(smoothedseries41_month$Sales))

##### Ordering the Data points in time series order

smoothedseries41_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries41_month$Sales[3] - smoothedseries41_month$Sales[2]

smoothedseries41_month$Sales[1]<-smoothedseries41_month$Sales[2]-diff_1

diff_2<-smoothedseries41_month$Sales[41]-smoothedseries41_month$Sales[40]

smoothedseries41_month$Sales[42]<-smoothedseries41_month$Sales[41]+diff_2

plot.ts(smoothedseries41_month$Sales)

##### Plotting a Graph using regression for the trend and seasonality

lmfit41<- lm(Sales ~ sin(0.3*(Month))*poly(Month,1) + cos (0.3*(Month))*poly(Month,1)+ Month, data=smoothedseries41_month)

Month <- c(1:42)

trend41 <- predict(lmfit41,data.frame(Month))

lines(smoothedseries41_month$Month,trend41,col="blue",lwd=2)

### Resi41duals

resi41<-timeseries41-trend41

plot(resi41, col='red')

acf(resi41)

acf(resi41, type="partial")

### acf plot suggests that the residual series is pure white noise so we only need to
### consider the values predicted by the lmfit.

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast41 <- predict(lmfit41,data.frame(Month))


###################### Model Creation using Arima ##################

autoarima41 <- auto.arima(timeseries41)
autoarima41 

tsdiag(autoarima41)

plot(autoarima41$x, col="black")

plot(timeseries41)

lines(fitted(autoarima41), col="blue")

Arimaforecast41 <- data.frame(forecast.Arima(autoarima41, h = 6))

plot(forecast.Arima(autoarima41, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(APACcorporateAg[c(1:42),2],trend)

accuracy(APACcorporateAg[c(43:48),2],fcast41)

accuracy(trend,APACcorporateAg[c(1:42),2])

accuracy(fcast41,APACcorporateAg[c(43:48),2])

#####MAPE Value for ARIMA#########

accuracy(APACcorporateAg[43:48,2], Arimaforecast41$Point.Forecast)




################ Model 5 : EUcorporate ####################

####### 1.Quantity 

EUcorporate_timeseries5  <-  EUcorporateAggr[1:42,]

EUcorporate_test <- EUcorporateAggr[43:48,]

### Plot the timeseries5

timeseries5 <-ts(EUcorporate_timeseries5$Quantity)

plot.ts(timeseries5)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries5 <- filter(timeseries5, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries5, col=cols[i],lwd=2)
}

width = 2

#### Smoothing the TIme series 

smoothedseries5 <- filter(timeseries5, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries5, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries5_month<-as.data.frame(cbind(EUcorporate_timeseries5$Order.Date,smoothedseries5))

colnames(smoothedseries5_month)<-c("Month","Quantity")

View(smoothedseries5_month)

smoothedseries5_month$Quantity<-as.numeric(as.character(smoothedseries5_month$Quantity))

##### Ordering the Data points in time series order

smoothedseries5_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

## diff_1<-smoothedseries5_month$Quantity[3] - smoothedseries5_month$Quantity[2]

## smoothedseries5_month$Quantity[1]<-smoothedseries5_month$Quantity[2]-diff_1

diff_2<-smoothedseries5_month$Quantity[41]-smoothedseries5_month$Quantity[40]

smoothedseries5_month$Quantity[42]<-smoothedseries5_month$Quantity[41]+diff_2

plot.ts(smoothedseries5_month$Quantity)

##### Plotting a Graph using regression for the trend and seasonality

lmfit5 <- lm(Quantity ~ sin(0.4*(Month))*poly(Month,1) + cos (0.4*(Month))*poly(Month,1)+ Month, data=smoothedseries5_month)

Month <- c(1:42)

trend4 <- predict(lmfit5,data.frame(Month))

lines(smoothedseries5_month$Month,trend4,col="blue",lwd=2)

### Resi5duals

resi5<-timeseries5-trend4

plot(resi5, col='red')

acf(resi5)

acf(resi5, type="partial")

############## 

armafit5 <- arima(resi5, order = c(2,0,2))

armafit51 <- as.data.frame(forecast(armafit5))

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast5 <- predict(lmfit5,data.frame(Month))

final5 <- fcast5 + armafit51$`Point Forecast`

###################### Model Creation using Arima ##################

autoarima5 <- auto.arima(timeseries5)
autoarima5 

tsdiag(autoarima5)

plot(autoarima5$x, col="black")

plot(timeseries5)

lines(fitted(autoarima5), col="blue")

Arimaforecast5 <- data.frame(forecast.Arima(autoarima5, h = 6))

plot(forecast.Arima(autoarima5, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(final5,EUcorporateAggr[c(43:48),3])

#####MAPE Value for ARIMA#########

accuracy(EUcorporateAggr[43:48,3], Arimaforecast5$Point.Forecast)

####### 2.Sales 

EUcorporate_timeseries51  <-  EUcorporateAggr[1:42,]

EUcorporate_test51 <- EUcorporateAggr[43:48,]

### Plot the timeseries51

timeseries51 <-ts(EUcorporate_timeseries51$Sales)

plot.ts(timeseries51)

## Smoothening

cols<-c("red","green","blue", "yellow")

for(i in seq(2:5))
{
  smoothedseries51 <- filter(timeseries51, filter=rep(1/i, i), method="convolution", sides=2)
  lines(smoothedseries51, col=cols[i],lwd=2)
}

width = 3

#### Smoothing the TIme series 

smoothedseries51 <- filter(timeseries51, filter=rep(1/width, width), method="convolution", sides=2)

### Plotting the smoothed series against the original un smoothed series

lines(smoothedseries51, lwd=2)

### Saving the smoothed data into a data set to make predictions

smoothedseries51_month<-as.data.frame(cbind(EUcorporate_timeseries51$Order.Date,smoothedseries51))

colnames(smoothedseries51_month)<-c("Month","Sales")

View(smoothedseries51_month)

smoothedseries51_month$Sales<-as.numeric(as.character(smoothedseries51_month$Sales))

##### Ordering the Data points in time series order

smoothedseries51_month$Month<-seq(1:42)

### Calculating the NA Values Imputed due to smoothing

diff_1<-smoothedseries51_month$Sales[3] - smoothedseries51_month$Sales[2]

smoothedseries51_month$Sales[1]<-smoothedseries51_month$Sales[2]-diff_1

diff_2<-smoothedseries51_month$Sales[41]-smoothedseries51_month$Sales[40]

smoothedseries51_month$Sales[42]<-smoothedseries51_month$Sales[41]+diff_2

plot.ts(smoothedseries51_month$Sales)

##### Plotting a Graph using regression for the trend and seasonality

lmfit51<- lm(Sales ~ sin(0.5*(Month))*poly(Month,2) + cos (0.7*(Month))*poly(Month,2)+ Month, data=smoothedseries51_month)

Month <- c(1:42)

trend51 <- predict(lmfit51,data.frame(Month))

lines(smoothedseries51_month$Month,trend51,col="blue",lwd=2)

### Resi5duals

resi5<-timeseries51-trend51

plot(resi5, col='red')

acf(resi5)

acf(resi5, type="partial")

## The ACf plot implies that the residual is pure white noise so we donot need to model it.

##### Predicting the 6 months using the model reated through regression

Month <- c(43:48)

fcast51 <- predict(lmfit51,data.frame(Month))


final51 <- fcast51

###################### Model Creation using Arima ##################

autoarima51 <- auto.arima(timeseries51)
autoarima51 

tsdiag(autoarima51)

plot(autoarima51$x, col="black")

plot(timeseries51)

lines(fitted(autoarima51), col="blue")

Arimaforecast51 <- data.frame(forecast.Arima(autoarima51, h = 6))

plot(forecast.Arima(autoarima51, h=6))


#####MAPE Value for Regression####

library(forecast)

accuracy(fcast51,EUcorporateAggr[c(43:48),2])

#####MAPE Value for ARIMA#########

accuracy(EUcorporateAggr[43:48,2], Arimaforecast51$Point.Forecast)
