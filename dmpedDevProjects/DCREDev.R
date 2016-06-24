library(stringr)
library(plyr)
library (dplyr)
library(reshape)
### Read in Data ###
### Read in Data ###
### Read in Data ###
reDevRaw<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/DC-RE_Dev/DC_RE_Dev.csv',
                   strip.white=TRUE)[c(3,6:12)] 
reDevRaw[is.na(reDevRaw)] <- 0
reDev<-reDevRaw

### Refactor ###
### Refactor ###
### Refactor ###
reDev$totalSQ<-reDev$Residential+reDev$Retail+reDev$Office+reDev$Hotel+reDev$Institutional
reDev$endYr<-substring(reDev$Construction.End, 1, 4)
reDev$startYr<-substring(reDev$Construction.Start, 1, 4)
reDev<-reDev[which(reDev$endYr!='2036' & reDev$endYr!='2026'),] 

sum(reDev$totalSQ)
#Total SqFT added in next 10 years is 15,897,448

reDev<-reDev[order(reDev$endYr), ]
reDev$endYr<-ifelse(reDev$endYr=='2021','2020-2025',reDev$endYr)

reDev.Ward<-ddply(reDev,c("Ward"),summarise,
                       totalSQFT=sum(totalSQ))
write.csv(reDev.Ward, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-RE_Dev/SqFtbyWard.csv",row.names=FALSE)

reDev.Ward.Time<-ddply(reDev,c("Ward","endYr"),summarise,
                                          totalSQFT=sum(totalSQ))
reDev.Ward.Time<-reDev.Ward.Time[order(reDev.Ward.Time$endYr,reDev.Ward.Time$Ward), ]
reDevWT<-reshape(reDev.Ward.Time, idvar="endYr", timevar="Ward", direction="wide")
reDevWT[is.na(reDevWT)] <- 0
reDevWT<-rename(reDevWT, c("totalSQFT.1"="Ward1", "totalSQFT.2"="Ward2","totalSQFT.4"="Ward4",
            "totalSQFT.5"="Ward5","totalSQFT.6"="Ward6","totalSQFT.7"="Ward7","totalSQFT.8"="Ward8"))
reDevWT<-reDevWT[,c(1,7,4,2,3,5,6,8)]
write.csv(reDevWT, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-RE_Dev/SqFtbyWardTime.csv",row.names=FALSE)

reDev.Ward.Type<-ddply(reDev,c("Ward"),summarise,
                                        totalRes=sum(Residential),
                                        totalRet=sum(Retail),
                                        totalOffice=sum(Office),
                                        totalHotel=sum(Hotel),
                                        totalInst=sum(Institutional))
### Analyze type mix by ward ###
### Analyze type mix by ward ###
### Analyze type mix by ward ###
WardTypeA<-reDev.Ward.Type
WardTypeA$totalSQ<-WardTypeA$totalRes+WardTypeA$totalRet+WardTypeA$totalOffice+WardTypeA$totalHotel+WardTypeA$totalInst
WardTypeA$ResP<-WardTypeA$totalRes/WardTypeA$totalSQ
WardTypeA$OffP<-WardTypeA$totalOff/WardTypeA$totalSQ
WardTypeA$HotelP<-WardTypeA$totalHotel/WardTypeA$totalSQ
WardTypeA$IntP<-WardTypeA$totalInst/WardTypeA$totalSQ


reDevWT2<-as.data.frame(t(reDev.Ward.Type))
reDevWT2<-rename(reDevWT2, c("V1"="Ward1", "V2"="Ward2","V3"="Ward4",
                           "V4"="Ward5","V5"="Ward6","V6"="Ward7","V7"="Ward8"))
reDevWT2 = reDevWT2[-1,]
reDevWT2$Prop<-c('Residential','Retail','Office','Hotel','Institutional/Muni')

write.csv(reDevWT2, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-RE_Dev/SqFtbyWardType.csv",row.names=FALSE)

