# Part I: Intro / General
# selecting data
# data with locations contain "Japan"
# (with aftershocks and group shocks)
data = read.csv("SolarSystemAndEarthquakes.csv")
Temp = data[grepl("Japan",data[,5]),]
write.csv(Temp,file="project_final.csv",row.names=FALSE)
select = c("earthquake.mag","earthquake.time","earthquake.latitude",
           "earthquake.longitude","MoonPhase.value","MoonPhase.percent",
           "MoonPhase.illumination","Sun.longitude","Sun.latitude",
           "Sun.azimuth","Sun.height","Sun.speed","Sun.sky.longitude",
           "Sun.sky.latitude","Moon.longitude","Moon.latitude",
           "Moon.sky.longitude","Moon.sky.latitude","Moon.azimuth",
           "Moon.height","Moon.speed","earthquake.place")
data1 = Temp[,select]
write.csv(data1,file="project.csv",row.names=FALSE)
dat = read.csv("project_1.csv") # no aftershocks
dat1 = read.csv("project.csv") # with aftershocks
# plotting Japan's earthquake locations and magnitudes
library("mapdata")
map('worldHires','Japan')
n=nrow(dat)
mag=dat$earthquake.mag
long=dat$earthquake.longitude
lat=dat$earthquake.latitude
for (i in 1:n) {
  if (mag[i]<7) {
    points(long[i],lat[i],col="dodgerblue",pch=20,lwd=2)
  } else if (mag[i] >=7 & mag[i] <8) {
    points(long[i],lat[i],col="purple",pch=20,lwd=2)
  } else {
    points(long[i],lat[i],col="red",pch=20,lwd=2)
  }
}
legend("topleft",c("mag<7","7<=mag<8","mag>=7"),
       col=c("dodgerblue","purple","red"),lwd=2)


# Part II: Analysis: Japan
# 2.1: Our Difficulties
#Linear regression for original data
coeff_origin = lm(dat1$earthquake.mag~dat1$MoonPhase.value
                  +dat1$MoonPhase.illumination+dat1$Sun.longitude+dat1$Sun.latitude
                  +dat1$Sun.azimuth+dat1$Moon.longitude+dat1$Moon.latitude
                  +dat1$Moon.azimuth)
summary(coeff_origin)
#linear regression aftershock
coeff = lm(dat$earthquake.mag~dat$MoonPhase.value
           +dat$MoonPhase.illumination+dat$Sun.longitude
           +dat$Sun.latitude+dat$Sun.azimuth+dat$Moon.longitude
           +dat$Moon.latitude+dat$Moon.azimuth)
summary(coeff)
Y = dat$earthquake.mag
fit_full = coeff
fit_null = lm(Y~1,data=dat)
## AIC
F_fit = step(fit_null, scope=list(lower=fit_null,upper=fit_full),
             direction="forward")
B_fit = step(fit_full, scope=list(lower=fit_null,upper=fit_full),
             direction="backward")
T_fit = step(fit_full, scope=list(lower=fit_null,upper=fit_full),
             direction="both")
#BIC
F_fit_bic = step(fit_null, scope=list(lower=fit_null,upper=fit_full),
                 direction="forward", k=log(nrow(dat)))
B_fit_bic = step(fit_full, scope=list(lower=fit_null,upper=fit_full),
                 direction="backward", k=log(nrow(dat)))
T_fit = step(fit_full, scope=list(lower=fit_null,upper=fit_full),
             direction="both", k=log(nrow(dat)))
# Check linear regression among only magnitude,
# sun longitude and moon azimuth
summary(lm(dat$earthquake.mag~dat$Sun.longitude+dat$Moon.azimuth))

# 2.2: Solar and Lunar Eclipses
library("ggplot2")
library("dplyr")
library("gridExtra")
library("Amelia")
library("corrplot")
library("ggfortify")
library("RColorBrewer")
library("forecast")
#Adding lunar and solar eclipses data and clean dataset
lunar = read.csv("lunar_new.csv")
solar = read.csv("solar.csv")
dates = function(temp){
  temp$Calendar.Date<-as.character(temp$Calendar.Date)
  temp$year<-sapply(temp$Calendar.Date,function(x)
    as.numeric(strsplit(x,' ')[[1]][1]))
  temp$day<-sapply(temp$Calendar.Date,function(x)
    as.numeric(strsplit(x,' ')[[1]][3]))
  temp$MONTH<-sapply(temp$Calendar.Date,function(x)
    strsplit(x,' ')[[1]][2])
  temp$month<-match(temp$MONTH,mymonths)
  temp$DATE<-as.POSIXct(paste(paste0(temp$year,'-',
                                     temp$month,'-',temp$day), as.character(temp$Eclipse.Time)),
                        format="%Y-%m-%d %H:%M:%S")
  return(temp)
}
lunar <- dates(lunar)
solar <- dates(solar)
#Find the correlation
cor_Solar <- merge(x = dat[,c("month","year","earthquake.latitude",
                              "earthquake.longitude","earthquake.mag", "DATE")],
                   y = solar[ , c("month","year","Eclipse.Type","Gamma","DATE")],
                   by=c("year","month"), all.x=TRUE)
cor_Lunar <- merge(x = dat[,c("month","year","earthquake.latitude",
                              "earthquake.longitude","earthquake.mag", "DATE")],
                   y = lunar[ , c("month","year","Eclipse.Type",
                                  "Gamma","DATE")], by=c("year","month"), all.x=TRUE)
#plot for solar
full_Solar = cor_Solar[complete.cases(cor_Solar),]
full_Solar$diff = as.numeric(difftime(full_Solar$DATE.x,
                                      full_Solar$DATE.y))
plot(x = full_Solar$earthquake.mag, y = full_Solar$diff,
     main = "Solar eclipse and magnitude", xlab = "Earthquake magnitude",
     ylab = "Difference (hours) btw occurrence of earthquake and
     Solar eclipse")
abline(h=1, col="red")
#plot for lunar
full_Lunar = cor_Lunar[complete.cases(cor_Lunar),]
full_Lunar$diff = as.numeric(difftime(full_Lunar$DATE.x,
                                      full_Lunar$DATE.y))
plot(x = full_Lunar$earthquake.mag, y = full_Lunar$diff,
     main = "Lunar eclipse and magnitude", xlab = "Earthquake magnitude",
     ylab = "Difference (hours) btw occurrence of earthquake and
     Lunar eclipse")
abline(h=1, col="red")
#Bootstrap for Solar and Lunar
diff = full_Solar$diff
diff_mean = mean(diff)
n = length(diff)
B = 10000
diff_mean_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  diff_BT = diff[w]
  diff_mean_BT[i_BT] = mean(diff_BT)
}
t.test(diff_mean_BT,mu=0)
hist(diff_mean_BT, col="skyblue", probability = T,
     main = "Solar difference bootstrap")
abline(v=0, lwd=3, col="red")
#Bootstrap for Lunar
diff = full_Lunar$diff
diff_mean = mean(diff)
n = length(diff)
B = 10000
diff_mean_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  diff_BT = diff[w]
  diff_mean_BT[i_BT] = mean(diff_BT)
}
t.test(diff_mean_BT,mu=0)
hist(diff_mean_BT, col="skyblue", probability = T,
     main = "Lunar difference Bootstrap")
abline(v=0, lwd=3, col="red")


# 2.3: Full / New Moon Pt1: General Analysis
# Test whether hist of moon phase value
# follows a uniform distribution (Use KS Test)
n = nrow(dat)
dat_mp = dat$MoonPhase.value
dat_un = runif(n, min=0,max=29.53)
ks.test(dat_mp,dat_un)$p.value # 0.9997616
# analyze the relationship between earthquake magnitude
# and moon phase illumination
plot(x=dat$earthquake.mag,y=dat$MoonPhase.illumination)
abline(v=50,col="red",lwd=3)
lm(dat$earthquake.mag~dat$MoonPhase.illumination^2)
summary(lm(dat$earthquake.mag~dat$MoonPhase.illumination^2))
n.mag=dat$earthquake.mag
temp1=dat[which(dat$MoonPhase.illumination<=50),]
temp2=dat[which(dat$MoonPhase.illumination>50),]
n.mag1=temp1$earthquake.mag
n.mag2=temp2$earthquake.mag
n.mp_il1=temp1$MoonPhase.illumination
n.mp_il2=temp2$MoonPhase.illumination
summary(lm(n.mag1~n.mp_il1))
summary(lm(n.mag2~n.mp_il2))
n1=nrow(temp1)
n2=nrow(temp2)
fit1=lm(n.mag1~n.mp_il1)
pred1=predict(fit1)
fit1_coeff=fit1$coefficients
coef_BT_res1=matrix(NA,nrow=B,ncol=2)
pva_1=matrix(NA,nrow=B,ncol=2)
summary(fit1)$coefficients[,4]
count1=0
for (i in 1:B) {
  w=sample(n1,n1,replace=T)
  y_bt=pred1+fit1$residuals[w]
  fit_BT=lm(y_bt~n.mp_il1)
  coef_BT_res1[i,]=fit_BT$coefficients
  pva_1[i,]=summary(fit_BT)$coefficients[,4]
  if (pva_1[,2][i]<=.05 & coef_BT_res1[,2][i]<0) {
    count1=count1+1
  }
}
count1/B # 0.0014
fit2=lm(n.mag2~n.mp_il2)
pred2=predict(fit2)
fit2_coeff=fit2$coefficients
coef_BT_res2=matrix(NA,nrow=B,ncol=2)
pva_2=matrix(NA,nrow=B,ncol=2)
summary(fit2)$coefficients[,4]
count2=0
for (i in 1:B) {
  w=sample(n2,n2,replace=T)
  y_bt=pred2+fit2$residuals[w]
  fit_BT=lm(y_bt~n.mp_il2)
  coef_BT_res2[i,]=fit_BT$coefficients
  pva_2[i,]=summary(fit_BT)$coefficients[,4]
  if (pva_2[,2][i]<=.05 & coef_BT_res2[,2][i]>0) {
    count2=count2+1
  }
}
count2/B # 0.114


# 2.4: A Time Series Model
# Basic methodology: block bootstrap
data = read.csv(file="project_.csv - project_.csv",
                header=TRUE)
library("TTR")
initial_data = rep(0,50)
japan_timeseries = ts(data$earthquake.mag)
japan_timeseries
date_reformat = data$earthquake.time
for (i in 292:2){
  difference = difftime(as.Date(date_reformat[i]),
                        as.Date(date_reformat[i - 1]))
  initial_data = c(initial_data, japan_timeseries[i])
  initial_data = c(initial_data, rep(0, abs(as.numeric(difference))))
}
japan_timeseries = ts(initial_data, frequency = 365.25,
                      start = c(1986,124))
plot(x=1)
plot(japan_timeseries)
japan_timeseries_SMA3 = SMA(japan_timeseries, n=365.25)
plot.ts(japan_timeseries_SMA3)
plot(decompose(japan_timeseries))
japan_timeseriescomponents = decompose(japan_timeseries)
plot(japan_timeseries - japan_timeseriescomponents$seasonal)
plot(japan_timeseries - japan_timeseriescomponents$random)
# find the appropriate difference
japan_timeseriesdiff1 = diff(japan_timeseries, differences = 1)
plot.ts(japan_timeseriesdiff1)
library(forecast)
auto.arima(japan_timeseries)
japan_timeseriesarima = arima(japan_timeseries, order=c(2,1,1))
japan_timeseriesarima
japan_timeseriesforecasts = forecast.Arima(japan_timeseriesarima,
                                           h=1095.75)
plot.forecast(japan_timeseriesforecasts)
plot.ts(japan_timeseriesforecasts$residuals)
plotForecastErrors(japan_timeseriesforecasts$residuals)


# 2.5: Full / New Moon Part 2: Tidal Force
Temp = read.csv("project_1.csv") #the variable name can changed latter
sv = ((cos(Temp[,14]*pi/180)-4.258821E-5)/(1-8.517642E-5*
                                             cos(Temp[,14]*pi/180)+1.8177E-9)^(3/2)-cos(Temp[,14]*pi/180))
sh = (sin(Temp[,14]*pi/180)/(1-8.517642E-5*cos(Temp[,14]*pi/180)
                             +1.8177E-9)^(3/2)-sin(Temp[,14]*pi/180))
mv = ((cos(Temp[,23]*pi/180)-1.65921)/(1-3.31842*cos(Temp[,23]
                                                     *pi/180)+2.752975)^(3/2)-cos(Temp[,23]*pi/180))
mh = (sin(Temp[,23]*pi/180)/(1-3.31842*cos(Temp[,23]*pi/180)
                             +2.752975)^(3/2)-sin(Temp[,23]*pi/180))
Temp = cbind(Temp,sv)
Temp = cbind(Temp,sh)
Temp = cbind(Temp,mv)
Temp = cbind(Temp,mh)
v = mv + sv
h = mh + sh
Temp = cbind(Temp,v)
Temp = cbind(Temp,h)
plot(Temp[,32],Temp[,1])#p-value 0.05 for "horizontal tide force"
fit = lm(Temp[,1]~Temp[,32], data=Temp)
summary(fit)
abline(fit,col = "red")
##Include aftershock when considering contingency table:
data = read.csv("project.csv")
sv = ((cos(data[,10]*pi/180)-4.258821E-5)/(1-8.517642E-5*
                                             cos(data[,10]*pi/180)+1.8177E-9)^(3/2)-cos(data[,10]*pi/180))
sh = (sin(data[,10]*pi/180)/(1-8.517642E-5*
                               cos(data[,10]*pi/180)+1.8177E-9)^(3/2)-sin(data[,10]*pi/180))
mv = ((cos(data[,19]*pi/180)-1.65921)/(1-3.31842*cos(data[,19]*pi/180)
                                       +2.752975)^(3/2)-cos(data[,19]*pi/180))
mh = (sin(data[,19]*pi/180)/(1-3.31842*cos(data[,19]*pi/180)+2.752975)
      ^(3/2)-sin(data[,19]*pi/180))
data = cbind(data,sv)
data = cbind(data,sh)
data = cbind(data,mv)
data = cbind(data,mh)
v = mv + sv
h = mh + sh
data = cbind(data,v)
data = cbind(data,h)
data = cbind(data,abs(v))
data = cbind(data,abs(h))
#Contingency table:
sum(data[,30]<0.69)
sum(data[,30]>=0.69)
sum(data[,1]>=6.4)
sum(data[,1]<6.4)
sum((data3[,1]>=6.4)&(data3[,30]<0.69))
table = matrix(c(68,60,69,95),ncol = 2)
chisq.test(table) #p-value = 0.05531
sum(data[,30]<0.80)
sum(data[,30]>=0.80)
sum(data[,1]>=6.4)
sum(data[,1]<6.4)
sum((data[,1]>=6.4)&(data[,30]>=0.8))
table2 = matrix(c(69,94,68,61),ncol = 2)
chisq.test(table2)
#not significant enough(0.0995), but enough for curiosity
# contingency table for data without aftershocks
sum(Temp[,30]<0.69)
sum(Temp[,30]>=0.69)
sum(Temp[,1]>=6.4)
sum(Temp[,1]<6.4)
sum((Temp[,1]>=6.4)&(Temp[,30]<0.69))
table = matrix(c(36,33,70,66),ncol = 2)
chisq.test(table) # p value = 1



# Part III: Comparison: Meanwhile in Chile
# selecting data
Temp2 = data[grepl("Chile",data[,5]),]
write.csv(Temp2,file="Chile.csv",row.names=FALSE)
dat_c=read.csv("final_chile.csv") # without aftershocks
# plot locations and magnitudes
library("mapdata")
map('worldHires','Chile')
dat_c=read.csv("final_chile.csv")
n_c=nrow(dat_c)
mag=dat_c$earthquake.mag
long=dat_c$earthquake.longitude
lat=dat_c$earthquake.latitude
for (i in 1:n_c) {
  if (mag[i]<7) {
    points(long[i],lat[i],col="dodgerblue",pch=20,lwd=2)
  } else if (mag[i] >=7 & mag[i] <8) {
    points(long[i],lat[i],col="purple",pch=20,lwd=2)
  } else {
    points(long[i],lat[i],col="red",pch=20,lwd=2)
  }
}
legend("topleft",c("mag<7","7<=mag<8","mag>=7"),
       col=c("dodgerblue","purple","red"),lwd=2)

# 3.1: Solar and Lunar Eclipses
getDATE<-function(x,pos1,pos2){
  val<-substr(strsplit(as.character(x),'T')[[1]][1],pos1,pos2)
  return(as.numeric(val))
}
getTIME<-function(x,pos1,pos2){
  val<-substr(strsplit(as.character(x),'T')[[1]][2],pos1,pos2)
  return(as.numeric(val))
}
dat_c$year<-sapply(dat_c$earthquake.time,getDATE,1,4)
dat_c$month<-sapply(dat_c$earthquake.time,getDATE,6,7)
dat_c$day<-sapply(dat_c$earthquake.time,getDATE,9,10)
dat_c$hour<-sapply(dat_c$earthquake.time,getTIME,1,2)
dat_c$min<-sapply(dat_c$earthquake.time,getTIME,4,5)
dat_c$sec<-sapply(dat_c$earthquake.time,getTIME,7,8)
mymonths<-c("January","February","March","April","May","June",
            "July","August","September","October","November","December")
dat_c$MonthAbb <- mymonths[ dat_c$month ]
dat_c$ordered_month <- factor(dat_c$MonthAbb, levels = month.name)
dat_c$DATE<-as.POSIXct(paste(paste0(dat_c$year,'-',dat_c$month,'-',
                                    dat_c$day), paste0(dat_c$hour,':',dat_c$min,':',dat_c$sec) ),
                       format="%Y-%m-%d %H:%M:%S")
chi_Solar <- merge(x = dat_c[,c("month","year","earthquake.latitude",
                                "earthquake.longitude","earthquake.mag", "DATE")], y = solar,
                   c("month","year","Eclipse.Type","Gamma","DATE")], 
                   by=c("year","month"),all.x=TRUE)
chi_Lunar <- merge(x = dat_c[,c("month","year","earthquake.latitude",
                                "earthquake.longitude","earthquake.mag", "DATE")], y = lunar[ ,
                                                                                              c("month","year","Eclipse.Type","Gamma","DATE")], by=c("year","month"),
                   all.x=TRUE)
full_Solar_C = chi_Solar[complete.cases(chi_Solar),]
full_Solar_C$diff = as.numeric(difftime(full_Solar_C$DATE.x,full_Solar_C$DATE.y))
plot(x = full_Solar_C$earthquake.mag, y = full_Solar_C$diff,
     main = "Solar eclipse and magnitude", xlab = "Earthquake magnitude",
     ylab = "Difference (hours) btw occurrence of earthquake and Solar
     eclipse")
abline(h=1, col="red")
full_Lunar_C = chi_Lunar[complete.cases(chi_Lunar),]
full_Lunar_C$diff = as.numeric(difftime(full_Lunar_C$DATE.x,
                                        full_Lunar_C$DATE.y))
plot(x = full_Lunar_C$earthquake.mag, y = full_Lunar_C$diff,
     main = "Lunar eclipse and magnitude", xlab = "Earthquake magnitude",
     ylab = "Difference (hours) btw occurrence of earthquake and Lunar
     eclipse")
abline(h=1, col="red")
#Bootstrap
diff = full_Solar_C$diff
diff_mean = mean(diff)
n = length(diff)
B = 10000
diff_mean_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  diff_BT = diff[w]
  diff_mean_BT[i_BT] = mean(diff_BT)
}
t.test(diff_mean_BT,mu=0)
hist(diff_mean_BT, col="skyblue", probability = T,
     main = "Solar difference bootstrap")
abline(v=0, lwd=3, col="red")
#chile lunar
diff = full_Lunar_C$diff
diff_mean = mean(diff)
n = length(diff)
B = 10000
diff_mean_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  diff_BT = diff[w]
  diff_mean_BT[i_BT] = mean(diff_BT)
}
t.test(diff_mean_BT,mu=0)
hist(diff_mean_BT, col="skyblue", probability = T,
     main = "Lunar difference Bootstrap", xlim = c(-250,50))
abline(v=0, lwd=3, col="red")