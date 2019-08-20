# data(elnino)

colnames(elnino)<-c('buoy', 'day','latitude',  'longitude', 
                    'zon.winds', 'mer.winds', 'humidity', 'air.temp', 's.s.temp')


elnino.ols<-lm(air.temp~zon.winds+mer.winds+humidity
               +s.s.temp,data=elnino)

summary(elnino.ols)

# separating elnino into two data sets dA and dB

dA<-elnino[,c('buoy','day','latitude','longitude','air.temp')]

dB<-elnino[,c('buoy','day','latitude','longitude',
              'zon.winds','mer.winds','humidity','s.s.temp')]
# uploading package fastLink for linking dA & dB

 library(fastLink)

 # linking A &B based on matching variable "long

link.elnino<-fastLink(dA,dB, c('buoy','latitude','latitude'))

# matched files

matched.elnino<-getMatches(dA,dB,link.elnino)

# merging two linked data sets

merged.elnino<-cbind.data.frame(matched.elnino$dfA.match,matched.elnino$dfB.match)

# OLS regression using the merged data 

merged.ols<-lm(air.temp~zon.winds+mer.winds+humidity+s.s.temp,
               data=merged.elnino)

summary(merged.ols)

# uploading robust package to do robust regression

library(robust)

elnino.rob<-lmRob(air.temp~zon.winds+mer.winds+humidity+s.s.temp,data=merged.elnino)

summary(elnino.rob)

library(MASS)

elnino.rlm<-rlm(air.temp~zon.winds+mer.winds+humidity+s.s.temp,data=merged.elnino)

summary(elnino.rlm)
 
# uploading permutation source code

source("permutaion.R")

