always_allow_html: yes
# Analysis of Airline Ticket Pricing
# NAME: Mayank Sagar
# EMAIL: mayanksagar26@gmail.com
# COLLEGE : R.V.COllege Of Engineering, Bengaluru.



#######PREMIUM ECONOMY VS ECONOMY TICKET PRICING BY AIRLINES#######
###################################################################
#Airline Fares

##setting the directory and assigning a variabel to the data frame
setwd("D:/Data Science and Analytics using R/Data files")

#Reading the dataset and creating a data frame
air.df<-read.csv(paste("SixAirlines.csv",sep = ""))

#Viewing the data frame
View(air.df)

##Analyzing the summary of the data and describing the variables
library(psych)
describe(air.df)
summary(air.df)

##Analyzing data with respect to every airline

by(air.df$PRICE_ECONOMY,air.df$AIRLINE,mean)
by(air.df$PRICE_PREMIUM,air.df$AIRLINE,mean)
by(air.df$SEATS_ECONOMY,air.df$AIRLINE,mean)
by(air.df$SEATS_PREMIUM,air.df$AIRLINE,mean)
by(air.df$WIDTH_ECONOMY,air.df$AIRLINE,mean)
by(air.df$WIDTH_PREMIUM,air.df$AIRLINE,mean)

#Taking information about the type of flights , whether international(1) or domestic(0) , in the data set
table(air.df$INTERNATIONAL)

##Drawing boxplots among comparable parameters
#Reading variables except airline beacause  it's a string
air1 = air.df[,2:17]
View(air1)
##Taking log
air2=log(air1+1)

 boxplot(air2, xlab="Value", ylab="Parameters", main="BoxPlot Presentation Of differnt Parameters")

##Individual boxplots for comparable parameters
 
 par(mfrow=c(1,2))
 with(air.df, boxplot(air.df$PRICE_ECONOMY,main="Price of Economy seats",ylab="Price in $"))
 with(air.df, boxplot(air.df$PRICE_PREMIUM,main="Price of Premium seats",ylab="Price in $"))
 par(mfrow=c(1,1))
 
 par(mfrow=c(1,2))
 with(air.df, boxplot(air.df$WIDTH_ECONOMY,main="Width of Economy seats",ylab="Width in inches"))
 with(air.df, boxplot(air.df$WIDTH_PREMIUM,main="width of Premium seats",ylab="Width in inches"))
 par(mfrow=c(1,1))
 
 par(mfrow=c(1,2))
 with(air.df, boxplot(air.df$PITCH_ECONOMY,main="Pitch of Economy seats",ylab="Pitch in inches"))
 with(air.df, boxplot(air.df$PITCH_PREMIUM,main="Pitch of Premium seats",ylab="Pitch in inches"))
 par(mfrow=c(1,1))
 
 par(mfrow=c(1,2))
 with(air.df, boxplot(air.df$SEATS_ECONOMY,main="No. of Economy seats",ylab="Count"))
 with(air.df, boxplot(air.df$SEATS_PREMIUM,main="No. of Premium seats",ylab="Count"))
 par(mfrow=c(1,1))
 
 boxplot(air.df$FLIGHT_DURATION, main="Duration of Flights",ylab="Hours")
 ##Price of seats varying with aircart company
 boxplot(air.df$PRICE_ECONOMY~air.df$AIRCRAFT,yaxt="n", horizontal=TRUE,main="Price of Economy seats with Aircraft company",xlab="Price in $", ylab="Name of the aircraft company")
 axis(side=2, at=c(1,2), labels = c("Boeing", "Airbus"))
 
 boxplot(air.df$PRICE_PREMIUM~air.df$AIRCRAFT,yaxt="n", horizontal=TRUE,main="Price of Premium seats with Aircraft company",xlab="Price in $", ylab="Name of the aircraft company")
 axis(side=2, at=c(1,2), labels = c("Boeing", "Airbus"))
 
##Plots 
 
 plot(jitter(air.df$INTERNATIONAL), jitter(air.df$AIRCRAFT))
 ##PRICE OF SEAT VARYING WITH FLIGHT DURATION
 plot(air.df$FLIGHT_DURATION~air.df$PRICE_ECONOMY,main="Price of Economy seat with flight duration",xlab = "Flight Duration in Hours", ylab="Price of seats in $", cex=1.1)
 library(car)
 scatterplot(air.df$PRICE_ECONOMY,air.df$FLIGHT_DURATION,main="Price of Economy seat with flight duration",ylab = "Flight Duration in Hours", xlab="Price of seats in $",cex=1.1,pch=19)
 
 
 plot(air.df$FLIGHT_DURATION~air.df$PRICE_PREMIUM,main="Price of Premium seat with flight duration",xlab = "Flight Duration in Hours", ylab="Price of seats in $", cex=1.1)
 scatterplot(air.df$PRICE_PREMIUM,air.df$FLIGHT_DURATION,main="Price of Premium seat with flight duration",ylab = "Flight Duration in Hours", xlab="Price of seats in $",cex=1.1,pch=19)
 
 plot(air.df$FLIGHT_DURATION~air.df$PRICE_RELATIVE,main="Relative price Vs flight duration",ylab = "Flight Duration in Hours", xlab="Ratio", cex=1.1)
 scatterplot(air.df$PRICE_RELATIVE,air.df$FLIGHT_DURATION,main="Relative price Vs flight duration",ylab = "Flight Duration in Hours", xlab="Ratio",cex=1.1,pch=19)
 
 plot(air.df$PITCH_ECONOMY~air.df$PRICE_RELATIVE,main="Relative price Vs flight duration",xlab = "Flight Duration in Hours", ylab="Ratio", cex=1.1)
 scatterplot(air.df$PRICE_RELATIVE,air.df$PITCH_ECONOMY,main="Relative price Vs flight duration",ylab = "Flight Duration in Hours", xlab="Ratio",cex=1.1,pch=19)
 
 
 library(car)
 scatterplotMatrix(
   air.df[
     ,c("FLIGHT_DURATION","PRICE_ECONOMY","PRICE_PREMIUM")], 
   spread=FALSE, smoother.args=list(lty=2),
   main="Scatter Plot Matrix", diagonal = "histogram")
 
 #Visualizing the data through ggvis 
 
 library(ggvis)
 
 ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
 
 ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE,data=air.df)
 
 ggvis(~PRICE_PREMIUM,~WIDTH_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
 
 #Interaction between the price quantities
 
 ggvis(~PRICE_ECONOMY,~PRICE_RELATIVE,fill=~PRICE_PREMIUM,data=air.df)
 
 ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
 
 ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~INTERNATIONAL,data=air.df)
 
 
 
 ##Correlation tests to find relationship between different parameters 
 # Correlation matrix,covariance matrix, Corrgram
 
 cor(air1)
 x<-air.df[,c("FLIGHT_DURATION","LAMBDA","INTERNATIONAL","N", "MONTH","QUALITY","WIDTH_ECONOMY", "WIDTH_PREMIUM","SEATS_ECONOMY", "SEATS_PREMIUM", "PITCH_ECONOMY", "PITCH_PREMIUM")]
 y<-air.df[,c("PRICE_ECONOMY","PRICE_PREMIUM","PRICE_RELATIVE")]
 cor(x,y)
 cov(x,y)
 var(x,y)
 
  #Visualizing relation through corrplots
 
 library(corrplot)
 corrplot(corr=cor(air1[,c(2,5:16)],use = "complete.obs"), method = "ellipse")
 library(gplots)
 corrplot.mixed(corr=cor(air1[,c(2,5:16)],use = "complete.obs"), upper = "ellipse", tl.pos = "lt", col = colorpanel(50, "red", "gray60", "blue4"))
 
 #VIsualizing by corrgram
 
 library(corrgram)
 
 corrgram(air1, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          main="Corrgram of Airline data intercorrelations")
 
 ##From the above cor(),corrplot and corrgram command we get to know the correlation , and can consider
 ## FLIGHT_DURATION as a factor for pricing in ECONOMY and PREMIUM seats and for 
 ## PRICE_RELATIVE factors such as QUALITY,PITCH_ECONOMY, PITCH_PREMIUM and WIDTH_PREMIUM are well correlated
 
 #Hypothesis
 #1. Average COST of Premium seats in Boing aircraft is less than Airbus aircraft 
 t.test(PRICE_PREMIUM~AIRCRAFT,alternative="less",data=air.df)
 
 #2. Average COST of ECONOMY seats in Boing aircraft is less than Airbus aircraft 
 t.test(PRICE_ECONOMY~AIRCRAFT,alternative="less", data=air.df)
 
 #3. Average COST of ECONOMY seats in Domestic is less than International
 t.test(PRICE_ECONOMY~INTERNATIONAL, alternative="less", data = air.df)
 
 #4.Average COST of PREMIUM ECONOMY seats in Domestic is less than International
 t.test(PRICE_PREMIUM~INTERNATIONAL, alternative="less", data = air.df)
 
 
 #Multiple Linear Regressional Model for Price of Economy Flights as dependent variable
 fit1<- lm(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION + QUALITY +PRICE_RELATIVE+ INTERNATIONAL, data = air.df)
 summary(fit1)
 
 #Multiple Linear Regressional Model for Price of Premium Flights as dependent variable
 fit2<- lm(PRICE_PREMIUM~ PITCH_PREMIUM + WIDTH_PREMIUM + FLIGHT_DURATION + QUALITY +PRICE_RELATIVE+ INTERNATIONAL , data = air.df)
 summary(fit2)
 
 #Multiple Linear Regressional Model for Price relative as dependent variable
 fit<-lm(PRICE_RELATIVE~.-AIRCRAFT-MONTH-SEATS_ECONOMY-SEATS_PREMIUM-LAMBDA-N-WIDTH_ECONOMY, data = air1)
 summary(fit)

 
 
 