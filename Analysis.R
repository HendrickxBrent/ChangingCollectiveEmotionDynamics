## author: Brent Hendrickx, University of Leuven
## created at 08/07/2022
## this code accompanies my thesis "Changing collective emotions during the COVID-19 pandemic in the UK."
## go to line 1370 to replicate tv-VAR analysis with the "n2020n2" dataset

## the code in this file has not been cleaned to provide a code file that will replicate all the analysis of my thesis in one go
## the majority will not be interesting to replicate, as a lot of this code does preprocessing of the data

library("corrplot")

#### time series
library(ggfortify)
library(dplyr)
library(tidyverse)

# time series libraries
library(zoo)
library(ggplot2)
library(RColorBrewer)
library(scales)

# tvVAR
library(mgm)
library(qgraph)



##### 
#####


small <- whole_tseriesdata %>% select(datecode, tone_pos, tone_neg, emotion, emo_pos, emo_neg, emo_anx, emo_anger,
                                      emo_sad, prosocial, socbehav, Social, moral, polite, family, friend, 
                                      power, Lifestyle, substances, home,work,money)
small$rank <- order(small$datecode)


ggplot(small,aes(x=rank))+
  geom_line(aes(y=tone_pos),color="blue")+
  geom_line(aes(y=emo_pos),color="red")+
  geom_line(aes(y=emo_neg),color="pink")+
  geom_line(aes(y=emo_anx),color="green")+ 
  geom_line(aes(y=emo_anger),color="grey")+
  geom_line(aes(y=prosocial),color="black")


ggplot(small,aes(x=rank, y=prosocial))+
  geom_line(aes(y=prosocial),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=emo_anx))+
  geom_line(aes(y=emo_anx),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=emo_anger))+
  geom_line(aes(y=emo_anger),color="black")+
  geom_point() +
  #geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "loess")

#small
ggplot(small,aes(x=rank, y=emo_sad))+
  geom_line(aes(y=emo_sad),color="black")+
  geom_point() +
  geom_smooth(method = "loess")


ggplot(small,aes(x=rank, y=socbehav))+
  geom_line(aes(y=socbehav),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

# small
ggplot(small,aes(x=rank, y=family))+
  geom_line(aes(y=family),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

# nothing
ggplot(small,aes(x=rank, y=friend))+
  geom_line(aes(y=friend),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=polite))+
  geom_line(aes(y=polite),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=moral))+
  geom_line(aes(y=moral),color="black")+
  geom_point() +
  geom_smooth(method = "loess")


################################################### not interesting
ggplot(small,aes(x=rank, y=power))+
  geom_line(aes(y=power),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=Lifestyle))+
  geom_line(aes(y=Lifestyle),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=home))+
  geom_line(aes(y=home),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(small,aes(x=rank, y=money))+
  geom_line(aes(y=money),color="black")+
  geom_point() +
  geom_smooth(method = "loess")
###################################################################


########################################################################################################################
#               prepare baseline data
########################################################################################################################




#### 2020 file

all2020 <- all2020[order(all2020$datecode),]
row.names(all2020) <- NULL
row.names(all2020) <- c(1:2398)
all2020$rank<-c(1:2398)



# (all2020$datecode)
# hist(all2020$Hour, )
# hist(all2020$Day, breaks=31)
# d <- density(all2020$Day)
# plot(d)
# 
# x <- table(n2020$datecode)
# xx <- data.frame(x)

all2020n <- all2020[-c(1636, 1637, 1640,1642), ]
write.csv(all2020n, "all2020n.csv")


# remove(April2020CleanLIWC)

# LIWC2019
# March2019CleanLIWC
# Feb2019CleanLIWC


## check data
#--------------------------------------
## March
hist(March2019CleanLIWC$Hour, )
hist(March2019CleanLIWC$Day, breaks=31)
d <- density(March2019CleanLIWC$Day)
plot(d)

### Feb
hist(Feb2019Clean$Hour, )
hist(Feb2019Clean$Day, breaks=31)
d <- density(Feb2019Clean$Day)
plot(d)

hist(Feb2019Rready$Day, breaks=31)
d <- density(Feb2019Rready$Day)
plot(d)



## put Feb and March together
#--------------------------------------
Rready2019 <- rbind(Feb2019Rready,March2019Rready)
LIWC2019 <- rbind(March2019CleanLIWC,Feb2019CleanLIWC)


#### input days of the week
datecode <- c(101:131,201:228,301:331,401:430,501:512)
days <- rep(c("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","Monday"),18) 
extra <- c("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
days <- c(days,extra)
days <- data.frame(datecode, days)

Rready2019$DOW <- NA

for (i in 1:nrow(Rready2019)){
  for (k in 1:nrow(days)){
    if (substr(Rready2019$datecode[i],1,3) == days$datecode[k]){
      Rready2019$DOW[i] <- days$days[k]
      i = i+1
      k = k+1
}}}







## march

procentdata <- LIWC2019 %>% select(datecode,procentscraped)
fulllistprocent <- aggregate(procentdata$procentscraped, by = list(procentdata$datecode), max)
names(fulllistprocent)[names(fulllistprocent) == "Group.1"] <- "datecode"

March19 <- merge(Rready2019,fulllistprocent, by  = "datecode") 
names(March19)[names(March19) == "x"] <- "procent"


hist(March19$procent, )
d <- density(March19$procent)
plot(d)




#  create weights
#------------------


March19$emo_posw    <- March19$emo_pos/March19$procent
March19$emo_negw    <- March19$emo_neg/March19$procent
March19$emo_anxw    <- March19$emo_anx/March19$procent
March19$emo_angerw  <- March19$emo_anger/March19$procent
March19$emo_sadw    <- March19$emo_sad/March19$procent
March19$prosocialw  <- March19$prosocial/March19$procent
March19$Socialw     <- March19$Social/March19$procent
March19$commw       <- March19$comm/March19$procent

March19select <- March19 %>% select(emo_neg, 
                                    emo_anx, 
                                    emo_anger,
                                    emo_sad,
                                    prosocial,
                                    Social, 
                                    comm,     
  emo_posw, emo_negw, emo_anxw,emo_angerw,
                                    emo_sadw,  
                                    prosocialw,
                                    Socialw,   
                                    commw)

### make sum
#-------------

March19$hour <- NA
for (i in 1:nrow(March19)){
  March19$hour[i]<- substring(March19$datecode[i],4,nchar(March19$datecode[i]))
  i <- i+1
}

March19mean <- aggregate(March19[,131:138], by = list(March19$DOW,March19$hour), FUN=mean)
names(March19mean)[names(March19mean) == "Group.1"] <- "DOW"
names(March19mean)[names(March19mean) == "Group.2"] <- "hour"


########################################################################################################################
#               2020 data
########################################################################################################################

### use the LIWC data to make %scraped

#### January
procentdata <- Jan2020CleanLIWC %>% select(datecode,procentscraped)
January2020procent <- aggregate(procentdata$procentscraped, by = list(procentdata$datecode), max)
names(January2020procent)[names(January2020procent) == "Group.1"] <- "datecode"

### check
hist(January2020procent$x, )
d <- density(January2020procent$x)
plot(d)
summary(January2020procent$x)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.3161  0.6188  0.8032  0.7777  0.9557  1.0000 

March19 <- merge(Rready2019,fulllistprocent, by  = "datecode") 
names(March19)[names(March19) == "x"] <- "procent"


hist(March19$procent, )
d <- density(March19$procent)
plot(d)


#### Feb+MArch
procentdata <- FebMarch2020LIWC %>% select(datecode,procentscraped)
FebMarch2020procent <- aggregate(procentdata$procentscraped, by = list(procentdata$datecode), max)
names(FebMarch2020procent)[names(FebMarch2020procent) == "Group.1"] <- "datecode"

hist(FebMarch2020procent$x, )
d <- density(FebMarch2020procent$x)
plot(d)
summary(FebMarch2020procent$x)


### April
procentdata2 <- April2020CleanLIWC %>% select(datecode,procentscraped)
April2020procent <- aggregate(procentdata2$procentscraped, by = list(procentdata2$datecode), max)
names(April2020procent)[names(April2020procent) == "Group.1"] <- "datecode"

hist(FebMarch2020procent$x, )
d <- density(FebMarch2020procent$x)
plot(d)
summary(FebMarch2020procent$x)

#### missing
procentdata3 <- missingCleanLIWC %>% select(datecode,procentscraped)
missing2020procent <- aggregate(procentdata3$procentscraped, by = list(procentdata3$datecode), max)
names(missing2020procent)[names(missing2020procent) == "Group.1"] <- "datecode"

hist(missing2020procent$x, )
d <- density(missing2020procent$x)
plot(d)
summary(missing2020procent$x)

## Mai
procentdata3 <- MaiCleanLIWC %>% select(datecode,procentscraped)
Mai2020procent <- aggregate(procentdata3$procentscraped, by = list(procentdata3$datecode), max)
names(Mai2020procent)[names(Mai2020procent) == "Group.1"] <- "datecode"

hist(Mai2020procent$x, )
d <- density(Mai2020procent$x)
plot(d)
summary(Mai2020procent$x)



## clean up procent datasets
## add them first

procent2020 <- rbind(January2020procent,FebMarch2020procent,April2020procent,missing2020procent,Mai2020procent)

# sort
procent2020 <- procent2020[order(procent2020$datecode),]
row.names(procent2020) <- NULL
row.names(procent2020) <- c(1:2398)

d <- density(procent2020$x)
plot(d)
summary(procent2020$x)

## double in the data -> march 31th 21,22,23, april 1st 00
procent2020 <- procent2020[-c(1636, 1637, 1640,1642), ]


n2020n <- merge(all2020n,procent2020, by  = "datecode") 
names(n2020n)[names(n2020n) == "x"] <- "procent"

n2020n2 <- n2020n %>% select(datecode, procent,emo_pos,emo_neg, emo_anx, 
                            emo_anger, emo_sad, prosocial, socbehav, Social, comm
                            )

### multiply with weights


n2020n2$emo_posp          <-NA
n2020n2$emo_negp          <-NA
n2020n2$emo_anxp          <-NA
n2020n2$emo_angerp        <-NA
n2020n2$emo_sadp          <-NA
n2020n2$prosocialp    <-NA
n2020n2$socbehavp     <-NA
n2020n2$Socialp       <-NA
n2020n2$commp         <-NA
                                                                      

for (i in 1:nrow(n2020n2)){
  
  ## calculate % change ->  baseline corrected result = [(new value 2020 - mean value 2019)/mean value 2019]*100
  n2020n2$emo_posp[i]     <- n2020n2$emo_pos[i]/n2020n2$procent[i]
  n2020n2$emo_negp[i]     <- n2020n2$emo_neg[i]/n2020n2$procent[i]
  n2020n2$emo_anxp[i]     <- n2020n2$emo_anx[i]/n2020n2$procent[i]
  n2020n2$emo_angerp[i]     <- n2020n2$emo_anger[i]/n2020n2$procent[i]
  n2020n2$emo_sadp[i]     <- n2020n2$emo_sad[i]/n2020n2$procent[i]
  n2020n2$prosocialp[i]     <- n2020n2$prosocial[i]/n2020n2$procent[i]
  n2020n2$socbehavp[i]     <- n2020n2$socbehav[i]/n2020n2$procent[i]
  n2020n2$Socialp[i]     <- n2020n2$Social[i]/n2020n2$procent[i]
  n2020n2$commp[i]     <- n2020n2$comm[i]/n2020n2$procent[i]

  i = i+1
}



#  make day variable for 2020 dataset
#--------------------------------------
#### input days of the week

## when I have January
# datecode <- c(101:131,201:229,301:331,401:409)
# days <- rep(c("Wednesday","Thursday","Friday","Saturday","Sunday","Monday","Tuesday"),14)   # first jan starts on wednesday
# extra <- c("Wednesday","Thursday")
# days <- c(days,extra)
# days <- data.frame(datecode, days)

datecode <- c(101:131,201:229,301:331,401:430,501:512)
days <- rep(c("Wednesday","Thursday","Friday","Saturday","Sunday","Monday","Tuesday"),18)   # first jan starts on wednesday
extra <- c("Wednesday","Thursday","Friday","Saturday","Sunday","Monday","Tuesday")
days <- c(days,extra)
days <- data.frame(datecode, days)

n2020n2$DOW <- NA

for (i in 1:nrow(n2020n2)){
  for (k in 1:nrow(days)){
    if (substr(n2020n2$datecode[i],1,3) == days$datecode[k]){
      n2020n2$DOW[i] <- days$days[k]
      i = i+1
      k = k+1
    }}}


n2020n2$hour <- NA
# hour
for (i in 1:nrow(n2020n2)){
  n2020n2$hour[i]<- substring(n2020n2$datecode[i],4,nchar(n2020n2$datecode[i]))
  i <- i+1
}




n2020n2$emo_sadb      <-NA
n2020n2$emo_angerb    <-NA
n2020n2$emo_anxb      <-NA
n2020n2$emo_negb      <-NA
n2020n2$emo_posb      <-NA

n2020n2$commb         <-NA
n2020n2$prosocialb    <-NA
n2020n2$Socialb       <-NA
n2020n2$socbehavb     <-NA



## new
n2020n2$DOWhour <- paste(n2020n2$DOW,n2020n2$hour)
March19mean$DOWhour <- paste(March19mean$DOW,March19mean$hour)
n2020n2 <- merge(n2020n2,March19mean, by  = "DOWhour") 


for (i in 1:nrow(n2020n2)){
  
        ## calculate % change ->  baseline corrected result = [(new value 2020 - mean value 2019)/mean value 2019]*100
        n2020n2$emo_sadb[i]     <- ((n2020n2$emo_sadp[i] - n2020n2$emo_sadw[i])/n2020n2$emo_sadw[i])*100
        n2020n2$emo_angerb[i]   <- ((n2020n2$emo_angerp[i] - n2020n2$emo_angerw[i])/n2020n2$emo_angerw[i])*100
        n2020n2$emo_anxb[i]     <- ((n2020n2$emo_anxp[i] - n2020n2$emo_anxw[i])/n2020n2$emo_anxw[i])*100
        n2020n2$emo_negb[i]     <- ((n2020n2$emo_negp[i]- n2020n2$emo_negw[i])/n2020n2$emo_negw[i])*100
        n2020n2$emo_posb[i]     <- ((n2020n2$emo_posp[i] - n2020n2$emo_posw[i])/n2020n2$emo_posw[i])*100
        
        n2020n2$commb[i]  <- ((n2020n2$commp[i] - n2020n2$commw[i])/n2020n2$commw[i])*100
        n2020n2$prosocialb[i]   <- ((n2020n2$prosocialp[i] - n2020n2$prosocialw[i])/n2020n2$prosocialw[i])*100
        n2020n2$Socialb[i]      <- ((n2020n2$Socialp[i] - n2020n2$Socialw[i])/n2020n2$Socialw[i])*100
        #n2020n2$socbehavb[i]    <- ((n2020n2$socbehav[i] - n2020n2$socbehavw[i])/n2020n2$socbehavw[i])*100
        
        i = i+1
}

n2020n2 <- n2020n2[order(n2020n2$datecode),]
row.names(n2020n2) <- NULL
row.names(n2020n2) <- c(1:2392)
n2020n2$rank<-c(1:2392)



ggplot(n2020n2,aes(x=rank))+
  geom_line(aes(y=emo_posb),color="red")+
  geom_line(aes(y=emo_negb),color="pink")+
  geom_line(aes(y=emo_anxb),color="green")+ 
  geom_line(aes(y=emo_angerb),color="grey")+
  geom_line(aes(y=prosocialb),color="black")


ggplot(n2020n2,aes(x=rank, y=emo_posb))+
  geom_line(aes(y=emo_posb),color="green")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(n2020n2,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y=emo_anxb),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(n2020n2,aes(x=rank, y=emo_angerb))+
  geom_line(aes(y=emo_angerb),color="red")+
  geom_point() +
  #geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "loess")

#n2020
ggplot(n2020n2,aes(x=rank, y=emo_sadb))+
  geom_line(aes(y=emo_sadb),color="lightblue")+
  geom_point() +
  geom_smooth(method = "loess")


ggplot(n2020n2,aes(x=rank, y=Socialb))+
  geom_line(aes(y=Socialb),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(n2020n2,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="black")+
  geom_point() +
  geom_smooth(method = "loess")

## delete 18 data point per day (not reliable)



no18n2020<-n2020n2[!(n2020n2$hour.x=="00"),]


ggplot(n2020n2,aes(x=rank, y=emo_angerb))+
  geom_line(aes(y=emo_angerb),color="red")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(no18n2020,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y=emo_anxb),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")
ggplot(n2020n2,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y=emo_anxb),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")







ggplot(no18n2020,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="black")+
  geom_point() +
  geom_smooth(method = "loess")


ggplot(n2020n2,aes(x=rank, y=emo_posb))+
  geom_line(aes(y=emo_posb),color="green")+
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("positive emotions % change over 2019 baseline")+
  ylim(-50, 150)

ggplot(n2020n2,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="blue")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("prosocial behavior % change over 2019 baseline")+
  ylim(-50, 100)

ggplot(n2020n2,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y=emo_anxb),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("anxiety % change over 2019 baseline")+
  ylim(-50, 250)

ggplot(n2020n2,aes(x=rank, y=emo_angerb))+
  geom_line(aes(y=emo_angerb),color="red")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("anger % change over 2019 baseline")+
  ylim(-50, 200)

ggplot(n2020n2,aes(x=rank, y=emo_sadb))+
  geom_line(aes(y=emo_sadb),color="lightblue")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("sad % change over 2019 baseline")+
  ylim(-60, 200)








ggplot(no18n2020,aes(x=rank, y=emo_posb))+
  geom_line(aes(y=emo_posb),color="black")+
  geom_point() +
  geom_smooth(method = "loess")+
  



n2020n2 <- no18n2020

ggplot(covid,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="blue")+
  geom_point() +
  geom_smooth(method = "loess")



############################################################
#               TV VAR
############################################################



library(mgm)


n2020n2$year <- "2020"
n2020n2$month <- NA
n2020n2$day <- NA
n2020n2$hour <- NA

# hour
for (i in 1:nrow(n2020n2)){
  n2020n2$hour[i]<- substring(n2020n2$datecode[i],4,nchar(n2020n2$datecode[i]))
  i <- i+1
}

#day
for (i in 1:nrow(n2020n2)){
  n2020n2$day[i]<- substr(n2020n2$datecode[i],2,3)
  i <- i+1
}

#month
for (i in 1:nrow(n2020n2)){
  n2020n2$month[i]<- substr(n2020n2$datecode[i],1,1)
  i <- i+1
}


n2020n2$month <- paste0("0", n2020n2$month)

n2020n2$time0 <- NA
n2020n2$time0 <- paste0(n2020n2$year,sep="-",n2020n2$month,sep="-",n2020n2$day,sep="-",n2020n2$hour)



time <- n2020n2 %>% select(time0,hour,day,month)
#time$dayno <- NA

for(i in 1:nrow(time)){
  
  if (time$month[i] == '01'){
    time$dayno[i] <- time$day[i]
    i <- i+1
  }
  if (time$month[i] == '02'){
    time$dayno[i] <- (as.numeric(time$day[i])+31)
    i <- i+1
  }
  if (time$month[i] == '03'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29)
    i <- i+1
  }
  if (time$month[i] == '04'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29+31)
    i <- i+1
  }
  if (time$month[i] == '05'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29+31+30)
    i <- i+1
  }
}
time$dayno <- as.numeric(time$dayno)


time$hourno <- NA
time["hourno"][time["hour"] == "07"] <- "1"
time["hourno"][time["hour"] == "08"] <- "2"
time["hourno"][time["hour"] == "09"] <- "3"
time["hourno"][time["hour"] == "10"] <- "4"
time["hourno"][time["hour"] == "11"] <- "5"
time["hourno"][time["hour"] == "12"] <- "6"
time["hourno"][time["hour"] == "13"] <- "7"
time["hourno"][time["hour"] == "14"] <- "8"
time["hourno"][time["hour"] == "15"] <- "9"
time["hourno"][time["hour"] == "16"] <- "10"
time["hourno"][time["hour"] == "17"] <- "11"
time["hourno"][time["hour"] == "18"] <- "12"
time["hourno"][time["hour"] == "19"] <- "13"
time["hourno"][time["hour"] == "20"] <- "14"
time["hourno"][time["hour"] == "21"] <- "15"
time["hourno"][time["hour"] == "22"] <- "16"
time["hourno"][time["hour"] == "23"] <- "17"
time["hourno"][time["hour"] == "00"] <- "18"
time$hourno <- as.numeric(time$hourno)

#time$hourno <- 1:nrow(time)
#time$hourno <- as.numeric(time$hourno)

### look at dataset used in Bringmann paper to see time structure
# dataofmood <- as.matrix(symptom_data$data[,1:12])
# moodlabelsdat <- symptom_data$colnames[1:12]
# timestamps <- symptom_data$data_time

#### now merge all hours together

n2020n2$daysno <-  NA
for (i in 1:nrow(n2020n2)){
  n2020n2$daysno[i]<- substr(n2020n2$datecode[i],1,3)
  i <- i+1
}


#days2020 <- n2020n2 %>% select(datecode, emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocialb, year,month,day,hour,daysno)

days2020 <- aggregate(n2020n2$emo_posb, by = list(n2020n2$daysno), sum)
days2020n <- aggregate(n2020n2$emo_anxb, by = list(n2020n2$daysno), sum)
days2020n2 <- aggregate(n2020n2$emo_angerb, by = list(n2020n2$daysno), sum)
days2020n3 <- aggregate(n2020n2$emo_sadb, by = list(n2020n2$daysno), sum)
days2020n4 <- aggregate(n2020n2$prosocialb, by = list(n2020n2$daysno), sum)



names(days2020)[names(days2020) == "Group.1"] <- "datecode"
names(days2020n)[names(days2020n) == "Group.1"] <- "datecode"
names(days2020n2)[names(days2020n2) == "Group.1"] <- "datecode"
names(days2020n3)[names(days2020n3) == "Group.1"] <- "datecode"
names(days2020n4)[names(days2020n4) == "Group.1"] <- "datecode"

names(days2020)[names(days2020) == "x"] <- "emo_posb"
names(days2020n)[names(days2020n) == "x"] <- "emo_anxb"
names(days2020n2)[names(days2020n2) == "x"] <- "emo_angerb"
names(days2020n3)[names(days2020n3) == "x"] <- "emo_sadb"
names(days2020n4)[names(days2020n4) == "x"] <- "prosocialb"


day2020 <- merge(days2020,days2020n,by  = "datecode")
day2020 <- merge(day2020,days2020n2,by  = "datecode")
day2020 <- merge(day2020,days2020n3,by  = "datecode")
day2020 <- merge(day2020,days2020n4,by  = "datecode")

day2020n <- day2020[,2:6]/18
day2020n$datecode <- day2020$datecode

##################################################################
##  cases
##################################################################


#### cases data in Python
day2020n$cases <- NA



cases <- cases[order(cases$date),]
cases <- cases[1:103,]
row.names(cases) <- NULL
row.names(cases) <- c(1:103)

casedata <- c(rep(0,30))
casedata2 <- cases$cumFirstEpisodesBySpecimenDate
casedata3 <- append(casedata,casedata2)
casedata3 <- data.frame(casedata3)

#casedata3 <- data.frame(casedata3[1:121,])
casedata3$datecode <- day2020n$datecode
day2020n$cases <- casedata3$casedata3
day2020n$rank <- c(1:133)

### check if still makes sense
ggplot(day2020n,aes(x=rank, y=emo_posb))+
  geom_line(aes(y=emo_posb),color="green")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(day2020n,aes(x=rank, y=emo_angerb))+
  geom_line(aes(y=emo_angerb),color="red")+
  geom_point() +
  geom_smooth(method = "loess")


ggplot(day2020n,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y="emo_anxb"),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")


ggplot(day2020n,aes(x=rank, y=emo_sadb))+
  geom_line(aes(y=emo_sadb),color="lightblue")+
  geom_point() +
  geom_smooth(method = "loess")

ggplot(day2020n,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="blue")+
  geom_point() +
  geom_smooth(method = "loess")



########## make time variable for it
day2020n$year <- "2020"
day2020n$month <- NA
day2020n$day <- NA


#day
for (i in 1:nrow(day2020n)){
  day2020n$day[i]<- substr(day2020n$datecode[i],2,3)
  i <- i+1}
#month
for (i in 1:nrow(day2020n)){
  day2020n$month[i]<- substr(day2020n$datecode[i],1,1)
  i <- i+1}


day2020n$month <- paste0("0", day2020n$month)

day2020n$time0 <- NA
day2020n$time0 <- paste0(day2020n$year,sep="-",day2020n$month,sep="-",day2020n$day)



timeday <- day2020n %>% select(time0,day,month)
timeday$dayno <- NA

for(i in 1:nrow(time)){
  
  if (time$month[i] == '01'){
    time$dayno[i] <- time$day[i]
    i <- i+1
  }
  if (time$month[i] == '02'){
    time$dayno[i] <- (as.numeric(time$day[i])+31)
    i <- i+1
  }
  if (time$month[i] == '03'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29)
    i <- i+1
  }
  if (time$month[i] == '04'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29+31)
    i <- i+1
  }
  if (time$month[i] == '05'){
    time$dayno[i] <- (as.numeric(time$day[i])+31+29+31+30)
    i <- i+1
  }
}
timeday$dayno <- as.numeric(time$dayno)

timeday$hourno <-  c(rep(1,133))    #1:nrow(time) 
timeday$hourno <- as.numeric(time$hourno)





########################################################################### OLD

whole_tseriesdata$cases <- 0
cases$date <- paste0(cases$date,sep="-","19")    # news is at 19h

## procent increase
cases$change <- 0
for(i in 1:nrow(cases)){
  if (i <91){
    cases$change[i+1] <- ((cases$cumFirstEpisodesBySpecimenDate[i] - cases$cumFirstEpisodesBySpecimenDate[i+1])/cases$cumFirstEpisodesBySpecimenDate[i+1])*100
    i <- i+1
  }}

## put in the datafile
for(i in 1:nrow(whole_tseriesdata)){
  for(k in 1:nrow(cases)){
    if  (whole_tseriesdata$time0[i] == cases$date[k]){
      whole_tseriesdata$cases[i] <- cases$cumFirstEpisodesBySpecimenDate[k]
      i <- i+1
    }
  }
}

## put in the datafile
whole_tseriesdata$caseschange <- 0
for(i in 1:nrow(whole_tseriesdata)){
  for(k in 1:nrow(cases)){
    if  (whole_tseriesdata$time0[i] == cases$date[k]){
      whole_tseriesdata$caseschange[i] <- cases$change[k]
      i <- i+1
    }
  }
}

### now calculate the values for other 17 datapoints

## multiples of 18
x = (1:65)
y = 18
z = x*y
z
z <- as.data.frame(z)

copy <- whole_tseriesdata
for(i in 13:nrow(whole_tseriesdata)){
  for (k in 1:nrow(z)){
    if (i == (z$z[k]-5)){
      value <- (whole_tseriesdata$cases[i+18] - whole_tseriesdata$cases[i])/18
      whole_tseriesdata$cases[i+1] <- (whole_tseriesdata$cases[i] + value)
      whole_tseriesdata$cases[i+2] <- (whole_tseriesdata$cases[i] + 2*value)
      whole_tseriesdata$cases[i+3] <- (whole_tseriesdata$cases[i] + 3*value)
      whole_tseriesdata$cases[i+4] <- (whole_tseriesdata$cases[i] + 4*value)
      whole_tseriesdata$cases[i+5] <- (whole_tseriesdata$cases[i] + 5*value)
      whole_tseriesdata$cases[i+6] <- (whole_tseriesdata$cases[i] + 6*value)
      whole_tseriesdata$cases[i+7] <- (whole_tseriesdata$cases[i] + 7*value)
      whole_tseriesdata$cases[i+8] <- (whole_tseriesdata$cases[i] + 8*value)
      whole_tseriesdata$cases[i+9] <- (whole_tseriesdata$cases[i] + 9*value)
      whole_tseriesdata$cases[i+10] <- (whole_tseriesdata$cases[i] + 10*value)
      whole_tseriesdata$cases[i+11] <- (whole_tseriesdata$cases[i] + 11*value)
      whole_tseriesdata$cases[i+12] <- (whole_tseriesdata$cases[i] + 12*value)
      whole_tseriesdata$cases[i+13] <- (whole_tseriesdata$cases[i] + 13*value)
      whole_tseriesdata$cases[i+14] <- (whole_tseriesdata$cases[i] + 14*value)
      whole_tseriesdata$cases[i+15] <- (whole_tseriesdata$cases[i] + 15*value)
      whole_tseriesdata$cases[i+16] <- (whole_tseriesdata$cases[i] + 16*value)
      whole_tseriesdata$cases[i+17] <- (whole_tseriesdata$cases[i] + 17*value)
      i <- i+18
      k <- k+1}
      if (i>534){
        break}
  }
}

z2 <- as.data.frame(z2 <- z$z-1)

for(i in 552:nrow(whole_tseriesdata)){
  for (k in 31:nrow(z2)){
    if ((i) == (z2$z[k]-5)){
      value <- (whole_tseriesdata$cases[i+18] - whole_tseriesdata$cases[i])/18
      whole_tseriesdata$cases[i+1] <- (whole_tseriesdata$cases[i] + value)
      whole_tseriesdata$cases[i+2] <- (whole_tseriesdata$cases[i] + 2*value)
      whole_tseriesdata$cases[i+3] <- (whole_tseriesdata$cases[i] + 3*value)
      whole_tseriesdata$cases[i+4] <- (whole_tseriesdata$cases[i] + 4*value)
      whole_tseriesdata$cases[i+5] <- (whole_tseriesdata$cases[i] + 5*value)
      whole_tseriesdata$cases[i+6] <- (whole_tseriesdata$cases[i] + 6*value)
      whole_tseriesdata$cases[i+7] <- (whole_tseriesdata$cases[i] + 7*value)
      whole_tseriesdata$cases[i+8] <- (whole_tseriesdata$cases[i] + 8*value)
      whole_tseriesdata$cases[i+9] <- (whole_tseriesdata$cases[i] + 9*value)
      whole_tseriesdata$cases[i+10] <- (whole_tseriesdata$cases[i] + 10*value)
      whole_tseriesdata$cases[i+11] <- (whole_tseriesdata$cases[i] + 11*value)
      whole_tseriesdata$cases[i+12] <- (whole_tseriesdata$cases[i] + 12*value)
      whole_tseriesdata$cases[i+13] <- (whole_tseriesdata$cases[i] + 13*value)
      whole_tseriesdata$cases[i+14] <- (whole_tseriesdata$cases[i] + 14*value)
      whole_tseriesdata$cases[i+15] <- (whole_tseriesdata$cases[i] + 15*value)
      whole_tseriesdata$cases[i+16] <- (whole_tseriesdata$cases[i] + 16*value)
      whole_tseriesdata$cases[i+17] <- (whole_tseriesdata$cases[i] + 17*value)
      i <- i+18
      k <- k+1
    }
  }
}


########################## now for % change
for(i in 13:nrow(whole_tseriesdata)){
  for (k in 1:nrow(z)){
    if (i == (z$z[k]-5)){
      value <- (whole_tseriesdata$caseschange[i+18] - whole_tseriesdata$caseschange[i])/18
      whole_tseriesdata$caseschange[i+1] <- (whole_tseriesdata$caseschange[i] + value)
      whole_tseriesdata$caseschange[i+2] <- (whole_tseriesdata$caseschange[i] + 2*value)
      whole_tseriesdata$caseschange[i+3] <- (whole_tseriesdata$caseschange[i] + 3*value)
      whole_tseriesdata$caseschange[i+4] <- (whole_tseriesdata$caseschange[i] + 4*value)
      whole_tseriesdata$caseschange[i+5] <- (whole_tseriesdata$caseschange[i] + 5*value)
      whole_tseriesdata$caseschange[i+6] <- (whole_tseriesdata$caseschange[i] + 6*value)
      whole_tseriesdata$caseschange[i+7] <- (whole_tseriesdata$caseschange[i] + 7*value)
      whole_tseriesdata$caseschange[i+8] <- (whole_tseriesdata$caseschange[i] + 8*value)
      whole_tseriesdata$caseschange[i+9] <- (whole_tseriesdata$caseschange[i] + 9*value)
      whole_tseriesdata$caseschange[i+10] <- (whole_tseriesdata$caseschange[i] + 10*value)
      whole_tseriesdata$caseschange[i+11] <- (whole_tseriesdata$caseschange[i] + 11*value)
      whole_tseriesdata$caseschange[i+12] <- (whole_tseriesdata$caseschange[i] + 12*value)
      whole_tseriesdata$caseschange[i+13] <- (whole_tseriesdata$caseschange[i] + 13*value)
      whole_tseriesdata$caseschange[i+14] <- (whole_tseriesdata$caseschange[i] + 14*value)
      whole_tseriesdata$caseschange[i+15] <- (whole_tseriesdata$caseschange[i] + 15*value)
      whole_tseriesdata$caseschange[i+16] <- (whole_tseriesdata$caseschange[i] + 16*value)
      whole_tseriesdata$caseschange[i+17] <- (whole_tseriesdata$caseschange[i] + 17*value)
      i <- i+18
      k <- k+1}
    if (i>534){
      break}
  }
}

z2 <- as.data.frame(z2 <- z$z-1)
for(i in 552:nrow(whole_tseriesdata)){
  for (k in 31:nrow(z2)){
    if ((i) == (z2$z[k]-5)){
      value <- (whole_tseriesdata$caseschange[i+18] - whole_tseriesdata$caseschange[i])/18
      whole_tseriesdata$caseschange[i+1] <- (whole_tseriesdata$caseschange[i] + value)
      whole_tseriesdata$caseschange[i+2] <- (whole_tseriesdata$caseschange[i] + 2*value)
      whole_tseriesdata$caseschange[i+3] <- (whole_tseriesdata$caseschange[i] + 3*value)
      whole_tseriesdata$caseschange[i+4] <- (whole_tseriesdata$caseschange[i] + 4*value)
      whole_tseriesdata$caseschange[i+5] <- (whole_tseriesdata$caseschange[i] + 5*value)
      whole_tseriesdata$caseschange[i+6] <- (whole_tseriesdata$caseschange[i] + 6*value)
      whole_tseriesdata$caseschange[i+7] <- (whole_tseriesdata$caseschange[i] + 7*value)
      whole_tseriesdata$caseschange[i+8] <- (whole_tseriesdata$caseschange[i] + 8*value)
      whole_tseriesdata$caseschange[i+9] <- (whole_tseriesdata$caseschange[i] + 9*value)
      whole_tseriesdata$caseschange[i+10] <- (whole_tseriesdata$caseschange[i] + 10*value)
      whole_tseriesdata$caseschange[i+11] <- (whole_tseriesdata$caseschange[i] + 11*value)
      whole_tseriesdata$caseschange[i+12] <- (whole_tseriesdata$caseschange[i] + 12*value)
      whole_tseriesdata$caseschange[i+13] <- (whole_tseriesdata$caseschange[i] + 13*value)
      whole_tseriesdata$caseschange[i+14] <- (whole_tseriesdata$caseschange[i] + 14*value)
      whole_tseriesdata$caseschange[i+15] <- (whole_tseriesdata$caseschange[i] + 15*value)
      whole_tseriesdata$caseschange[i+16] <- (whole_tseriesdata$caseschange[i] + 16*value)
      whole_tseriesdata$caseschange[i+17] <- (whole_tseriesdata$caseschange[i] + 17*value)
      i <- i+18
      k <- k+1
    }
  }
}


####################################################################################################
### clean text COVID

Jan2020CleanCOVID <- read.csv("~/Psychology/KUL/Psychologie/thesis/Twitter/January/weeks/Jan2020CleanCOVID.csv")
selected <- Jan2020CleanCOVID %>% select(COVID,corona,virus,coronavirus,lockdown,pandemic,epidemic,outbreak,quarantine,social,distance,infected,cases)
x <- cor(selected)
x

## sort first
Jan2020CleanCOVID <- Jan2020CleanCOVID[order(Jan2020CleanCOVID$corona),]
Jan2020CleanCOVID <- Jan2020CleanCOVID[order(Jan2020CleanCOVID$virus),]
row.names(Jan2020CleanCOVID) <- NULL
row.names(Jan2020CleanCOVID) <- c(1:2759056)

Jan2020CleanCOVID$corona_virus <-NA
for (i in 2750000:nrow(Jan2020CleanCOVID)){
  if (Jan2020CleanCOVID$corona[i] == Jan2020CleanCOVID$virus[i]){
    if (Jan2020CleanCOVID$corona[i] == '1'){
    Jan2020CleanCOVID$corona_virus[i] <-1
    i = i+1
  }}
}

Jan2020CleanCOVID$corona_virus[is.na(Jan2020CleanCOVID$corona_virus)] <- 0

write.csv(Jan2020CleanCOVID, "Jan2020CleanCOVIDtoP.csv")


# feb march
### clean text COVID

FebMarch2020COVID <- read.csv("~/Psychology/KUL/Psychologie/thesis/Twitter/Feb/FebMarch2020COVID.csv")
selected <- FebMarch2020COVID %>% select(COVID,corona,virus,coronavirus,lockdown,pandemic,epidemic,outbreak,quarantine,social,distance,infected,cases)
x <- cor(selected)
x

## sort first
FebMarch2020COVID <- FebMarch2020COVID[order(FebMarch2020COVID$corona),]
FebMarch2020COVID <- FebMarch2020COVID[order(FebMarch2020COVID$virus),]
row.names(FebMarch2020COVID) <- NULL
row.names(FebMarch2020COVID) <- c(1:7897616)

FebMarch2020COVID$corona_virus <-NA
for (i in 7791000:nrow(FebMarch2020COVID)){
  if (FebMarch2020COVID$corona[i] == FebMarch2020COVID$virus[i]){
    if (FebMarch2020COVID$corona[i] == '1'){
      FebMarch2020COVID$corona_virus[i] <-1
      i = i+1
    }}
}

FebMarch2020COVID$corona_virus[is.na(FebMarch2020COVID$corona_virus)] <- 0

write.csv(FebMarch2020COVID, "FebMarch2020COVIDtoP.csv")




# April
### clean text COVID


selected <- April2020COVID %>% select(COVID,corona,virus,coronavirus,lockdown,pandemic,epidemic,outbreak,quarantine,social,distance,infected,cases)
x <- cor(selected)
x

## sort first
April2020COVID <- April2020COVID[order(April2020COVID$corona),]
April2020COVID <- April2020COVID[order(April2020COVID$virus),]
row.names(April2020COVID) <- NULL
row.names(April2020COVID) <- c(1:3161784)

April2020COVID$corona_virus <-NA
for (i in 3127000:nrow(April2020COVID)){
  if (April2020COVID$corona[i] == April2020COVID$virus[i]){
    if (April2020COVID$corona[i] == '1'){
      April2020COVID$corona_virus[i] <-1
      i = i+1
    }}
}

April2020COVID$corona_virus[is.na(April2020COVID$corona_virus)] <- 0

write.csv(April2020COVID, "April2020COVIDtoP.csv")

remove(April2020COVID)

#### missing

selected <- missingCOVID %>% select(COVID,corona,virus,coronavirus,lockdown,pandemic,epidemic,outbreak,quarantine,social,distance,infected,cases)
x <- cor(selected)
x

## sort first
missingCOVID <- missingCOVID[order(missingCOVID$corona),]
missingCOVID <- missingCOVID[order(missingCOVID$virus),]
row.names(missingCOVID) <- NULL
row.names(missingCOVID) <- c(1:161370)

missingCOVID$corona_virus <-NA
for (i in 155000:nrow(missingCOVID)){
  if (missingCOVID$corona[i] == missingCOVID$virus[i]){
    if (missingCOVID$corona[i] == '1'){
      missingCOVID$corona_virus[i] <-1
      i = i+1
    }}
}

missingCOVID$corona_virus[is.na(missingCOVID$corona_virus)] <- 0
write.csv(missingCOVID, "missingCOVIDtoP.csv")


#### missing

selected <- MaiCOVID %>% select(COVID,corona,virus,coronavirus,lockdown,pandemic,epidemic,outbreak,quarantine,social,distance,infected,cases)
x <- cor(selected)
x
## sort first
MaiCOVID <- MaiCOVID[order(MaiCOVID$corona),]
MaiCOVID <- MaiCOVID[order(MaiCOVID$virus),]
row.names(MaiCOVID) <- NULL
row.names(MaiCOVID) <- c(1:1027014)

MaiCOVID$corona_virus <-NA
for (i in 1010000:nrow(MaiCOVID)){
  if (MaiCOVID$corona[i] == MaiCOVID$virus[i]){
    if (MaiCOVID$corona[i] == '1'){
      MaiCOVID$corona_virus[i] <-1
      i = i+1
    }}
}
MaiCOVID$corona_virus[is.na(MaiCOVID$corona_virus)] <- 0

write.csv(MaiCOVID, "MaiCOVIDtoP.csv")



#### all together

COVID <- rbind(April2020COVIDRready,FebMarch2020COVIDRready,Jan2020COVIDRready,missingCOVIDRready,MaiCOVIDRready)
x <- cor(COVID)
x
corrplot(x)

COVID <- COVID[-c(1636, 1637, 1640,1642), ]
#COVID <- rbind(COVID,missingCOVIDRready)
covid <- COVID %>% select(datecode,COVID,corona_virus,lockdown,quarantine,pandemic,outbreak,cases)
covid$covid19 <- rowSums(covid[,2:3])
y <- cor(covid)
y
corrplot(y)

data <- merge(covid,n2020n2, by='datecode')

data$covid19p <- data$covid19 / data$procent


library("corrplot")
corrplot(x, method="color")

COVID <- COVID[order(COVID$datecode),]
row.names(COVID) <- NULL
row.names(COVID) <- c(1:2394)
COVID$rank<-c(1:2394)

ggplot(COVID,aes(x=rank))+
  #geom_line(aes(y=lockdown),color="red")+
  geom_line(aes(y=pandemic),color="pink")+
  geom_line(aes(y=quarantine),color="green")+ 
  geom_line(aes(y=pandemic),color="grey")

ggplot(COVID,aes(x=rank))+
  geom_line(aes(y=COVID),color="red")+
  geom_line(aes(y=corona_virus),color="pink")
  #geom_line(aes(y=corona),color="green")+ 
  #geom_line(aes(y=virus),color="grey")+
  #geom_line(aes(y=coronavirus),color="black")

ggplot(data,aes(x=rank))+
  geom_line(aes(y=covid19p),color="red")+
  geom_line(aes(y=emo_anxb),color="grey")

ggplot(data,aes(x=rank))+
  geom_line(aes(y=covid19p),color="red")

ggplot(data,aes(x=rank))+
  geom_line(aes(y=emo_sadb),color="pink")



ggplot(n2020n2,aes(x=rank, y=emo_posb))+
  geom_line(aes(y=emo_posb),color="green")+
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("positive emotions % difference over 2019 baseline")+
  ylim(-50, 150)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))

ggplot(n2020n2,aes(x=rank, y=prosocialb))+
  geom_line(aes(y=prosocialb),color="blue")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("prosocial behavior % difference over 2019 baseline")+
  ylim(-50, 100)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))

ggplot(n2020n2,aes(x=rank, y=emo_anxb))+
  geom_line(aes(y=emo_anxb),color="purple")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("anxiety % difference over 2019 baseline")+
  ylim(-50, 250)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))

ggplot(n2020n2,aes(x=rank, y=emo_angerb))+
  geom_line(aes(y=emo_angerb),color="red")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("anger % difference over 2019 baseline")+
  ylim(-50, 250)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))

ggplot(n2020n2,aes(x=rank, y=emo_sadb))+
  geom_line(aes(y=emo_sadb),color="lightblue")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("sad % difference over 2019 baseline")+
  ylim(-60, 500)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))

ggplot(data,aes(x=rank, y=covid19p))+
  geom_line(aes(y=covid19p),color="pink")+
  geom_point() +
  geom_smooth(method = "loess")+
  ylab("amount of mentions of COVID-19 per hour")+
  ylim(0, 1000)+
  scale_x_continuous(breaks = seq(0, 2500, by = 50))





## some t-tests to see if some periods are already increasing

datak <- data[397:522,]      # jan 30th to feb 5th
datan <- data[523:648,]      # jan 30th to feb 5th
datam <- data[649:774,]      # feb 6th to feb 12th
datao <- data[775:900,]      # feb 13th to feb 20th
datap <- data[901:1026,]      # feb 21th to feb 27th
dataq <- data[1027:1152,]      # feb 28th to March 5th
datar <- data[1153:1277,]      # March 6th to march 12th  x
datas <- data[1278:1399,]      # March 12th to march 18th
datat <- data[1400:1525,]      # March 19th to march 25th
datau <- data[1526:1655,]      # March 26th to April 1st
datav <- data[1656:1781,]      # April 2nd to April 8st
dataw <- data[1782:1906,]      # April 9st to April 15st
datax <- data[1907:2032,]      # April 16th to April 22th
datay <- data[2033:2157,]      # April 23th to April 29th
dataz <- data[2157:2282,]      # April 30th to April 6th
dataa <- data[2283:2393,]      # April 7th to April 12th



t.test(datak$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datan$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datam$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datao$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datap$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(dataq$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datar$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datas$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datat$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datau$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datav$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataw$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datax$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(datay$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(dataz$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(dataa$emo_anxb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi

#sad
t.test(datak$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datan$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datam$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datao$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datap$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(dataq$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datar$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datas$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datat$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datau$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datav$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataw$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datax$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datay$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataz$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataa$emo_sadb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher

#anger
t.test(datak$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datan$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datam$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datao$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datap$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher   # either coronavirus or EU-UK
t.test(dataq$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datar$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datas$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datat$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datau$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(datav$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataw$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datax$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(datay$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(dataz$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi
t.test(dataa$emo_angerb,mu=0,alternative="two.sided",conf.level = 0.95) # not signi



#pos
t.test(datak$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datan$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datam$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datao$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datap$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(dataq$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datar$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datas$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datat$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datau$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datav$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataw$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datax$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datay$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataz$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataa$emo_posb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher

#prosocial
t.test(datak$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datan$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95)
t.test(datam$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datao$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datap$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(dataq$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datar$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi lower
t.test(datas$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datat$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datau$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datav$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataw$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datax$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(datay$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataz$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher
t.test(dataa$prosocialb,mu=0,alternative="two.sided",conf.level = 0.95) # signi higher

############################################################
#               TV VAR
############################################################





######################## for model  1

#whole_tseriesdata$cases[whole_tseriesdata$cases == 0] <- NA
mood <- n2020n2 %>% select(emo_posb, emo_anxb, emo_angerb,emo_sadb,prosocialb,covid19p
                                      #commb,
                                     #cases
                                     )
mood <- data %>% select(emo_posb, emo_anxb, emo_angerb,emo_sadb,prosocialb,covid19p
                           #commb,
                           #cases
)

mood_labels <- colnames(mood[1:6])
mood_labels <- c("positive", "anxiety", "anger","sad","prosocial","COVID")

mood2 <- data.matrix(mood)

time2 <- time %>% select(hourno,dayno,hour,day,month)
time2 <- data.matrix(time2)


#### model per day
mood <- day2020n %>% select(emo_posb, emo_anxb, emo_angerb,emo_sadb,prosocialb,cases
                            #commb,
                            #cases
)

mood_labels <- colnames(mood[1:6])

mood2 <- data.matrix(mood)

time2 <- time %>% select(hourno,dayno,day,month)
time2 <- data.matrix(time2)


##########################################
#        bandwith selection
##########################################

set.seed(1)
bwSeq <- seq(0.01,0.1,length=10)

bw_object <-bwSelect(data= mood2,
                     type=rep("g",6),     # number = number of variables
                     level=rep(1,6),
                     bwSeq=bwSeq,
                     bwFolds=1,
                     bwFoldsize=20,
                     modeltype="mvar",
                     lags=1,
                     scale=TRUE,
                     #timepoints=      #when no time points are given, we expect them to be equidistant
                     beepvar=time$hourno,       ## model per day -> use: variable with 1:121       model per hour -> use: hourno
                     dayvar=time$dayno,       ## model per day -> use: variable with all 1's     model per hour -> use: dayno
                     pbar=TRUE)
bandwith <- bwSeq[which.min(bw_object$meanError)]
bandwith
bandwithdata0.01_0.1hour <-bw_object$meanError
#bandwithdata0.1_1hour <-bw_object$meanError
#bandwithdata0.01_0.1hour <-bw_object$meanError
#bandwithdata0.1_1hour <-bw_object$meanError
#bandwithdata1_10 <-bw_object$meanError
#bandwithdata0.1_1 <-bw_object$meanError
#bandwithdata0.01_0.1 <-bw_object$meanError
bandwithdata0.1_1hour
bandwithdata0.01_0.1hour

# model per days
#bandwithdata1_10
#bandwithdata0.1_1
#bandwithdata0.01_0.1

##########################################
#               model
##########################################







tvvar_obj <- tvmvar(data=mood2,
                    type=rep("g",6),     # 10 = number of variables
                    level=rep(1,6),
                    lambdaSel="CV",
                    #timepoints= mood$rank, #when no time points are given, we expect them to be equidistant
                    estpoints = seq(0,1,length=20),
                    bandwidth=0.1,
                    lags=1,
                    beepvar=time$hourno,
                    dayvar=time$dayno,             #dayno
                    scale=TRUE)
tvvar_obj

########################################
#    reliability of parameter estimates 
########################################


res_obj <- resample(object=tvvar_obj,
                    data=mood,
                    nB=50,
                    blocks=10,
                    seeds=1:50,
                    quantiles=c(0.05,0.95))
res_obj

########################################
#    time-varying prediction error 
########################################

pred_obj <- predict(object=tvvar_obj,
                    data=mood,
                    errorCon=c("R2","RMSE"),
                    tvMethod="weighted",
                    consec=time$hourno)           ## switch to hourno   for  data per hour

pred_obj
pred_obj$tverrors


####################################################
#    time-varying Mixed Graphical Model (MGM) 
####################################################

fit_mgm <- mgm(data = mood2, 
               type = rep("g",6),
               levels = rep(1,6), 
               k = 3, lambdaSel = "CV", lambdaFolds = 10,ruleReg = "AND")



#emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocial, covid
fit_mgm$interactions$indicator

FactorGraph(object = fit_mgm, labels = mood_labels,PairwiseAsEdge = FALSE)



########################################
#    visualizing tv VAR model
########################################









############################################### mine


library(qgraph)

moodn <- n2020n2 %>% select(emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocialb, cases
                                       #comm, cases
                          )
moodn <- day2020n %>% select(emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocialb, cases
                            #comm, cases
)

colnames(moodn)[1:6]

mood_labelss <- moodn$colnames[1:6]
moodn$colnames[1:6]


f_timeline_new <- function(length = .15, 
                           gap = .005, 
                           mar = c(0,0,0,0), 
                           ylim = c(-.1,.1), 
                           ytext = -.1,
                           cex = 1) {
  
  
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(0,1), ylim=ylim)
  # box()
  
  # arrows
  p_weeks <- c(31,29,31,30,12)
  bor_end <- c(0,cumsum(p_weeks)/sum(p_weeks))
  for(i in 1:5) {
    arrows(bor_end[i]+gap, 0, bor_end[i+1]-gap, code=3, length=length, lwd=1.5)
  }
  
  # text
  t_lengths <- p_weeks / sum(p_weeks)
  midpoints <- bor_end[-1] - t_lengths/2
  
  
  text(midpoints, rep(ytext, 5), c("January",
                                   "February",
                                   "March",
                                   "April",
                                   "Mai (first 12 days)"),
       cex = 0.8)
  
  # lockdown
  points((83) / (sum(p_weeks)), rep(0,1), pch=20, cex=1.5, col = "red")    ## not sure about this!!   (sum(p_weeks)*7-10)  = 53, what is it?
  points((76) / (sum(p_weeks)), rep(0,1), pch=20, cex=1.5, col = "orange")
  points(c(6,70,108) / (sum(p_weeks)), rep(0,3), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 1
  #points((42) / (sum(p_weeks)*7-5), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 2
  #points(c(42,98) / (sum(p_weeks)*7-5), rep(0,2), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 3
}


# ----- Preprocessing  ------

# Compute mean movel over time to create decent layout
mean_wadj <- apply(tvvar_obj$wadj[, , 1, ], 1:2, mean)

par_ests <- tvvar_obj$wadj
ind_negative <- which(tvvar_obj$signs == -1, arr.ind = T)
par_ests[ind_negative] <- par_ests[ind_negative] * -1

# Find parameters with highest SD
wadj_ws <- tvvar_obj$wadj
wadj_ws[tvvar_obj$edgecolor=="red"] <- wadj_ws[tvvar_obj$edgecolor=="red"] * -1
parm_sds <- apply(wadj_ws, 1:2, sd)
parm_sds_mat <- matrix(NA, 12^2, 3)
counter <- 1
for(i in 1:6) {                              ##### change this to the amount of variables
  for(j in 1:6) {                            ##### change this to the amount of variables
    parm_sds_mat[counter, ] <- c(i, j, parm_sds[i, j]) 
    counter <- counter + 1
  }
}

parm_sds_mat_ord <- parm_sds_mat[order(parm_sds_mat[, 3], decreasing = TRUE), ]
head(parm_sds_mat_ord) # six most time-varying parameters
parm_sds_mat

# ----- Plotting ------

library(qgraph)
figDir <- "C:/Users/Brent/Documents/Psychology/KUL/Psychologie/thesis/Twitter/All2020/figures"

pdf(paste0(figDir, "Fig_Application_mgm.pdf"), width = 8, height = 7)

# 1) Define Layout

lmat <- matrix(c(1, 2, 3,
                 4, 4, 4,
                 5, 5, 5), ncol=3, byrow = T)
lo <- layout(lmat, 
             heights = c(.7,.1, .6), 
             widths = c(1, 1, 1))


# 2) Two Network Plots

# Get layout of mean graph
Q <- qgraph(t(mean_wadj), DoNotPlot=TRUE)
saveRDS(Q$layout, "C:/Users/Brent/Documents/Psychology/KUL/Psychologie/thesis/Twitter/All2020/figures/layout_mgm.RDS")

# Plot graph at selected fixed time points
tpSelect <- c(2, 12, 18)

# Switch to colorblind scheme
tvvar_obj$edgecolor[, , , ][tvvar_obj$edgecolor[, , , ] == "darkgreen"] <- c("darkblue")
lty_array <- array(1, dim=c(6, 6, 1, 20))    ##### change this to the amount of variables
lty_array[tvvar_obj$edgecolor[, , , ] != "darkblue"] <- 2

for(tp in tpSelect) {
  qgraph(t(tvvar_obj$wadj[, , 1, tp]), 
         layout = Q$layout,
         edge.color = t(tvvar_obj$edgecolor[, , 1, tp]), 
         labels = mood_labels, 
         vsize = 13, 
         esize = 10,
         asize = 10, 
         mar = rep(5, 4), 
         minimum = 0, 
         maximum = .5, 
         lty = t(lty_array[, , 1, tp]),
         pie = pred_obj$tverrors[[tp]][, 3])
}

# 4) Timeline
f_timeline_new(length = .1, 
               mar=c(0, 4, 0, 1), 
               ylim = c(-1.2, .2), 
               ytext = -.9, 
               cex = 1)

# 5) Line-plots + CIs
plot.new()
par(mar = c(4,4,0,1))
plot.window(xlim=c(1, 20), ylim=c(-.50, .75))
axis(1, c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20), labels=T)
axis(2, c(-.50, -.25, 0, .25,0.5,0.75), las=2)

title(xlab = "Estimation points", cex.lab = 1.2)
title(ylab = "Parameter estimate", cex.lab = 1.2)
abline(h = 0, col = "grey", lty=2)


head(parm_sds_mat_ord) # pick three highest

#emo_posb, emo_anxb, emo_angerb,emo_sadb,prosocial, cases 
                          #comm, cases
m_par_display <- matrix(c(3, 4, 
                          3, 1, 
                          5, 3), ncol = 2, byrow = T)

# Select colors
cols <- brewer.pal(5, "Set1")[c(2,4,5)] # avoid red/green because used for edges in upper panel

for(i in 1:nrow(m_par_display)) {
  par_row <- m_par_display[i, ]
  ## Plot point estimates
  P1_pointest <- par_ests[par_row[1], par_row[2], 1, ]
  lines(1:20, P1_pointest, col = cols[i], lwd = 2, lty=i) 
  ## Plot uncertainty estimates [new shading]
  # Compute CIs
  CIs <- apply(res_obj$bootParameters[par_row[1], par_row[2], 1, , ], 1, function(x) {
    quantile(x, probs = c(.05, .95))
  } )
  # Plot shading
  polygon(x = c(1:20, 20:1), y = c(CIs[1,], rev(CIs[2,])), col=alpha(colour = cols[i], alpha = .3), border=FALSE)
} # end for: i

# Legend
#emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocial
#comm, cases
legend_labels <- c(expression("sad"["t-1"]  %->%  "anger"["t"]),
                   expression("pos"["t-1"]  %->%  "anger"["t"]),
                   expression("anger"["t-1"]  %->%  "prosocial"["t"]))
legend("bottomleft", 
       legend_labels,
       col = cols, 
       lwd = 2, bty = "n", cex = 1.5, horiz=T, lty=1:3)

dev.off()





######################################################################################
######                  NEW visualization
######################################################################################



f_timeline_new <- function(length = .15, 
                           gap = .005, 
                           mar = c(0,0,0,0), 
                           ylim = c(-.1,.1), 
                           ytext = -.1,
                           cex = 1) {
  
  
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(0,1), ylim=ylim)
  # box()
  
  # arrows
  p_weeks <- c(31,29,31,30,12)
  bor_end <- c(0,cumsum(p_weeks)/sum(p_weeks))
  for(i in 1:5) {
    arrows(bor_end[i]+gap, 0, bor_end[i+1]-gap, code=3, length=length, lwd=1.5)
  }
  
  # text
  t_lengths <- p_weeks / sum(p_weeks)
  midpoints <- bor_end[-1] - t_lengths/2
  
  
  text(midpoints, rep(ytext, 5), c("January",
                                   "February",
                                   "March",
                                   "April",
                                   "Mai (first 12 days)"),
       cex = 0.8)
  
  # lockdown
  points((83) / (sum(p_weeks)), rep(0,1), pch=20, cex=1.5, col = "red")    ## not sure about this!!   (sum(p_weeks)*7-10)  = 53, what is it?
  points((76) / (sum(p_weeks)), rep(0,1), pch=20, cex=1.5, col = "orange")
  points(c(6,70,108) / (sum(p_weeks)), rep(0,3), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 1
  #points((42) / (sum(p_weeks)*7-5), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 2
  #points(c(42,98) / (sum(p_weeks)*7-5), rep(0,2), pch=4, cex=1.5, col = "black")   ## cross for networkmodel 3
}


# ----- Preprocessing  ------

# Compute mean movel over time to create decent layout
mean_wadj <- apply(tvvar_obj$wadj[, , 1, ], 1:2, mean)

par_ests <- tvvar_obj$wadj
ind_negative <- which(tvvar_obj$signs == -1, arr.ind = T)
par_ests[ind_negative] <- par_ests[ind_negative] * -1

# Find parameters with highest SD
wadj_ws <- tvvar_obj$wadj
wadj_ws[tvvar_obj$edgecolor=="red"] <- wadj_ws[tvvar_obj$edgecolor=="red"] * -1
parm_sds <- apply(wadj_ws, 1:2, sd)
parm_sds_mat <- matrix(NA, 12^2, 3)
counter <- 1
for(i in 1:6) {                              ##### change this to the amount of variables
  for(j in 1:6) {                            ##### change this to the amount of variables
    parm_sds_mat[counter, ] <- c(i, j, parm_sds[i, j]) 
    counter <- counter + 1
  }
}

parm_sds_mat_ord <- parm_sds_mat[order(parm_sds_mat[, 3], decreasing = TRUE), ]
head(parm_sds_mat_ord) # six most time-varying parameters
parm_sds_mat

# ----- Plotting ------

library(qgraph)
figDir <- "C:/Users/Brent/Documents/Psychology/KUL/Psychologie/thesis/Twitter/All2020/figures"

pdf(paste0(figDir, "Fig_Application_mgm.pdf"), width = 8, height = 7)

# 1) Define Layout

lmat <- matrix(c(1, 2, 3,
                 4, 4, 4,
                 5, 5, 5), ncol=3, byrow = T)
lo <- layout(lmat, 
             heights = c(.7,.1, .6), 
             widths = c(1, 1, 1))


# 2) Two Network Plots

# Get layout of mean graph
Q <- qgraph(t(mean_wadj), DoNotPlot=TRUE)
saveRDS(Q$layout, "C:/Users/Brent/Documents/Psychology/KUL/Psychologie/thesis/Twitter/All2020/figures/layout_mgm.RDS")

# Plot graph at selected fixed time points
tpSelect <- c(2, 12, 18)

# Switch to colorblind scheme
tvvar_obj$edgecolor[, , , ][tvvar_obj$edgecolor[, , , ] == "darkgreen"] <- c("darkblue")
lty_array <- array(1, dim=c(6, 6, 1, 20))    ##### change this to the amount of variables
lty_array[tvvar_obj$edgecolor[, , , ] != "darkblue"] <- 2

for(tp in tpSelect) {
  qgraph(t(tvvar_obj$wadj[, , 1, tp]), 
         layout = Q$layout,
         edge.color = t(tvvar_obj$edgecolor[, , 1, tp]), 
         labels = mood_labels, 
         vsize = 13, 
         esize = 10,
         asize = 10, 
         mar = rep(5, 4), 
         minimum = 0, 
         maximum = .5, 
         lty = t(lty_array[, , 1, tp]),
         pie = pred_obj$tverrors[[tp]][, 3])
}

# 4) Timeline
f_timeline_new(length = .1, 
               mar=c(0, 4, 0, 1), 
               ylim = c(-1.2, .2), 
               ytext = -.9, 
               cex = 1)

# 5) Line-plots + CIs
plot.new()
par(mar = c(4,4,0,1))
plot.window(xlim=c(1, 20), ylim=c(-.50, .75))
axis(1, c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20), labels=T)
axis(2, c(-0.25,0, .25), las=2)

title(xlab = "Estimation points", cex.lab = 1.2)
title(ylab = "Parameter estimate", cex.lab = 1.2)
abline(h = 0, col = "grey", lty=2)


head(parm_sds_mat_ord) # pick three highest

#emo_posb, emo_anxb, emo_angerb,emo_sadb,prosocial, cases 
#comm, cases
m_par_display <- matrix(c(6, 1, 
                          2, 1, 
                          3, 4), ncol = 2, byrow = T)

# Select colors
cols <- brewer.pal(12, "Spectral")[c(20,2,5)] # avoid red/green because used for edges in upper panel
### set 1             ### set 2
#1 red                 darkgreen
#2 blue             lightorange
#3 purple           greyblue
#4 green             pink
#5 orange           limegreen   x
#6 lightyellow       yellow   x
#7 brown            light brown   x
#8 pink             lightgrey
#9 grey              error


for(i in 1:nrow(m_par_display)) {
  par_row <- m_par_display[i, ]
  ## Plot point estimates
  P1_pointest <- par_ests[par_row[1], par_row[2], 1, ]
  lines(1:20, P1_pointest, col = cols[i], lwd = 2, lty=i) 
  ## Plot uncertainty estimates [new shading]
  # Compute CIs
  CIs <- apply(res_obj$bootParameters[par_row[1], par_row[2], 1, , ], 1, function(x) {
    quantile(x, probs = c(.05, .95))
  } )
  # Plot shading
  polygon(x = c(1:20, 20:1), y = c(CIs[1,], rev(CIs[2,])), col=alpha(colour = cols[i], alpha = .3), border=FALSE)
} # end for: i

# Legend
#emo_posb, emo_anxb, emo_angerb,emo_sadb, prosocial
#comm, cases
legend_labels <- c(expression("positive"["t-1"]  %->%  "covid"["t"]),
                   expression("positive"["t-1"]  %->%  "anxiety"["t"]),
                   expression("sad"["t-1"]  %->%  "anger"["t"]))
legend("bottomright", 
       legend_labels,
       col = cols, 
       lwd = 2, bty = "n", cex = 1, horiz=T, lty=1:3)

dev.off()


