library("Ecdat")
library("ggplot2")
library("dplyr")



View(Cigarette)
#create boxplot average number of packs per capita per state
ggplot(Cigarette,aes(x=packpc,y=state))+geom_boxplot()

meanfirst<-Cigarette%>%select(packpc,state)%>%group_by(packpc)%>%summarise(state=median(state)
View(meanfirst)
ggplot(meanfirst,aes(x=packpc,y=state))+geom_point()
#find the median over all the states number of packs
Cigarette%>%group_by(packpc,state)%>%summarise(Mean=mean(packpc))%>%arrange(Mean)
#highest numbers of packs.
#198 NH,186 KY, 155 NC, 145 VT, 144 DE
#lowest numbers of packs.
#49.3 UT, 56,9 CA, 64.7 NM, 71.6 NY, 72.0 AZ
unique(Cigarette$year)

ggplot(meanfirst,aes(x=1985,y=1995))+geom_point()
#Cigarette usage average between or grow more between 1985.000-1995.000

ggplot(Cigarette,aes(x=avgprs,y=packpc))+geom_point()
#The slope is downward.My y values is increasing and x decreasing.
#This is negatively correlated.
cor.test(Cigarette$avgprs,Cigarette$packpc,method"Pearson",use="complete.obs")

ggplot(Cigarette,aes(x=avgprs,y=packpc))+geom_point()+geom_smooth

ggplot(Cigarette,aes(x=avgprs,y=packpc,color=year))geom_point()
ggplot(Cigarette,aes(x=avgprs,y=packpc,color=year))+geom_point()+geom_smooth(method=lm)

cor.test(Cigarette$avgprs,Cigarette$packpc,method="pearson",use="complete.obs")
#t=16.562,df=526,p-value <2.2e-16, alternative hypothesis is:
#true correelation is not equal to 0. 95 percent confidence interval.
#-0.6388606 - -0.5264104sample estimates cor -0.5854443


#linear regression
regressvar<-lm(packpc~avgprs,Cigarette)
summary

#how much variability does the line explain?
#-0.40879 the Adjusted R-squared 0.3415 and p-value <2.2e-16
#2 percent 


#adjust the price of a pack of Cigarettes
inflate<-Cigarette%>%mutate(avgadj=avgprs/cpi)

#first row of adjustment
ggplot(inflate,aes(x=avgadj,y=cpi))+geom_point()

ggplot(inflate,aes(x=avgadj,y=cpi,color=avgadj))+geom_point()

#Second row of adjustment
inflate2<-lm(cpi~avgadj,inflate)

ggplot(inflate2,aes(x=cpi,y=avgadj,color=cpi))+geom_point()

summary(inflate2)
# degrees of freedom Multiply R-squared:0.322, Adjusted R-squared
#0.3208, and p-value:<2.2e-16


row1<-Cigarette%>%filter(year==1985)
row2<-Cigarette%>%filter(year==1995)


pcapitav<-row1$packpc
pcapitav<-row2$packpc

pcapitav<-t.test(row1,row2,paired=TRUE)

t.test(row1$packpc,row2$packpc,paired=TRUE)
#was 1995 significantly different than 1985 number of packs per capita

#t=14.789, df=47, p-value <2.2e-16 true mean difference is not equal to
#0 22.21151 29.20576  sample estimates:mean difference 25.70863

