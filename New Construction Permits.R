setwd("/Users/katerabinowitz/Documents/DataLensDC/Construction_DC")
library(rgdal)
library(splitstackshape)
library(plyr)
library(ggmap)
###Read in Data###
###Read in Data###
###Read in Data###
pullPermits<-function(url,year) {
  Permit = readOGR(url, "OGRGeoJSON")
  PermitDF<-Permit@data
  PermitDF$YEAR<-rep(year,nrow(PermitDF))
  Build<-subset(PermitDF,PermitDF$PERMIT_TYPE_NAME=="CONSTRUCTION" & PermitDF$PERMIT_SUBTYPE_NAME=="NEW BUILDING")
}

Build16<-pullPermits("http://opendata.dc.gov/datasets/5d14ae7dcd1544878c54e61edda489c3_24.geojson",2016)
Build15<-pullPermits("http://opendata.dc.gov/datasets/981c105beef74af38cc4090992661264_25.geojson",2015)
Build14<-pullPermits("http://opendata.dc.gov/datasets/d4891ca6951947538f6707a6b07ae225_5.geojson",2014)
Build13<-pullPermits("http://opendata.dc.gov/datasets/4911fcf3527246ae9bf81b5553a48c4d_6.geojson",2013)
Build12<-pullPermits("http://opendata.dc.gov/datasets/5f4ea2f25c9a45b29e15e53072126739_7.geojson",2012)

Build1216<-rbind(Build12,Build13,Build14,Build15,Build16)
write.csv(Build1216,"NewConstruction1216.csv",row.names=FALSE)
Build1216<-read.csv("NewConstruction1216.csv", stringsAsFactors=FALSE, strip.white=TRUE)

### Data Clean and Factor###
### Data Clean and Factor###
### Data Clean and Factor###
#remove permits that reissues/extensions of original building permit
BD<-as.data.frame(unique(Build1216$FULL_ADDRESS[duplicated(Build1216$FULL_ADDRESS)]))
colnames(BD)<-c("FULL_ADDRESS")
BuildDup<-merge(BD, Build1216, by=c("FULL_ADDRESS"),type="left")
BuildDup<-BuildDup[order(BuildDup$FULL_ADDRESS,BuildDup$ISSUE_DATE),]
BD2 <- data.table(BuildDup, key = c('FULL_ADDRESS'))
BD<-BD2[unique(BD2[,key(BD2), with = FALSE]), mult = 'first']
DupAddress<-subset(BuildDup,!(BuildDup$PERMIT_ID %in% BD$PERMIT_ID))
Build1216<-subset(Build1216,!(Build1216$PERMIT_ID %in% DupAddress$PERMIT_ID))

#Permits that revise that address should have the original permit removed in order to align with the correct address
Revision<-subset(Build1216,grepl("REVISION|CORRECT ADDRESS",Build1216$DESC_OF_WORK))
Build1216<-subset(Build1216,!(Build1216$PERMIT_ID %in% c("B1140521","B1202952","B1209223","B1300655","B1201080",
                                                       "B1002258","B1201235","B1201241","B1201247","B1201249","B1201251")))
Original<-subset(Build1216,(Build1216$PERMIT_ID %in% c("B1140521","B1202952","B1209223","B1300655","B1201080",
                                                         "B1002258","B1201235","B1201241","B1201247","B1201249","B1201251")))

rm(BD,BD2,BuildDup,DupAddress,Revision)

Build1216$PropType<-ifelse(grepl("SINGLE FAM|SFD|S.F DWELLING|SINGLE-FAMILY|S.F.D|SIGLE FAMILY|ONE FAMILY|SFH",
                          Build1216$DESC_OF_WORK),"Single Family",
                    ifelse(grepl("APARTMENT|UNIT|TWO FAMILY|TWO-FAMILY|2 FAMILY|2 FAMILLY|MIXED USE|MIXED-USE|FLAT",Build1216$DESC_OF_WORK)
                          | grepl("CONDO|MULTI USE|9 STORY RESIDENTIAL BUILDING|DUPLEX|MULTI-FAMILY|MULTIFAMILY",Build1216$DESC_OF_WORK)
                          | grepl("9-STORY BUILDING FOR FOREIGN|FIVE STORY RESIDENTIAL|NEW 3 STORY BUILDING WITH A ROOF DECK",
                          Build1216$DESC_OF_WORK),"Multi Family",
                        ifelse(grepl("TOWNHOUSE|TOWNHOME|NEW HOUSE|BEDROOM HOUSE|3 STORY HOUSE|STORY HOUSE|TOWN HOME",Build1216$DESC_OF_WORK)
                              |grepl("TOWN HOUSE|SEMI DETACHED DWELLING|ROW HOME|HOME|STORY SEMI DETATCHED HOUSE",
                          Build1216$DESC_OF_WORK),"Single Family",NA)))

Build1216$PropType<-ifelse(Build1216$FULL_ADDRESS %in% c("720 JEFFERSON ST NW","410 EASTERN AVE NE","426 EASTERN AVE NE",
                                                        "442 EASTERN AVE NE","444 EASTERN AVE NE","446 EASTERN AVE NE",
                                                        "448 EASTERN AVE NE","450 EASTERN AVE NE"),
                                                        "Single Family",Build1216$PropType)
table(Build1216$PropType)
#Check undefined properties for any leftover residential
NABuild<-subset(Build1216,is.na(Build1216$PropType))

#Check Single Family for mismatches
SFD<-subset(Build1216,Build1216$PropType=="Single Family")
SFD<-SFD[order(SFD$FULL_ADDRESS),]

SFD<-subset(SFD,!grepl("WOOD PERGOLA|ELEVATOR ONLY|EXISTING POOLHOUSE|2013 SOLAR DEC|5TH YOUTH RESIDENTIAL",
                       SFD$DESC_OF_WORK))
SFD$PropType<-ifelse(grepl("2 UNIT BUILDING|10-UNIT APARTMENT BUILDING|MIXED USE",SFD$DESC_OF_WORK),
                           "Multi Family",SFD$PropType)

#Check Multi Family for mismatches
Multi<-subset(Build1216,Build1216$PropType=="Multi Family")
Multi<-subset(Multi,!(grepl("MEDICAL CLINIC|CT MOBILE UNIT|HOTEL|BASEBALL DUGOUTS",Multi$DESC_OF_WORK)
                      | grepl("COMMUNITY USE|TEMPORARY ENGINE COMPANY 14|PROPERTY MANAGEMENT AND COMMUNITY BUILDING",Multi$DESC_OF_WORK)
                      | grepl("COMMUNITY CENTER|HAROLD J. GORDON|SELF STORAGE FACILITY|15 HIGH PRIVATE GARAGE",Multi$DESC_OF_WORK)
                      | grepl("11 STORY MIXED USE BUILDING, RETAIL AND OFFICE WITH UNDERGROUND PARKING.",Multi$DESC_OF_WORK)))
Multi<-subset(Multi, !(Multi$FULL_ADDRESS %in% c("1728 14TH ST NW", "800 22ND ST NW", "2700 MARTIN LUTHER KING JR AVE SE",
                                                "5220 WISCONSIN AVE NW")))
Multi$PropType<-ifelse(grepl("ATTACHED TOWN HOME THAT IS PART OF A 20 UNIT|SIDE ELEVATION END UNIT OPTIONS|SIDE BY SIDE",
                             Multi$DESC_OF_WORK),"Single Family",Multi$PropType)

#Combine for accurate single/multi family
ResBuilding<-rbind(SFD,Multi)

#Differentiate between 2 unit and 2+unit multi family
Multi<-subset(ResBuilding,ResBuilding$PropType=="Multi Family")
Multi$PropType<-ifelse(grepl(" 2 UNIT|TWO UNIT|TWO-UNIT|TWO FAMILY| 2 FAMILLY|TWO-FAMILY| 2 FAMILY| 2-UNIT|DUPLEX|TWO (2) UNIT|(2 UNITS)|2 NEW DWELLING UNIT|2 DWELLING UNIT",
                             Multi$DESC_OF_WORK),"Multi-Two Unit",Multi$PropType)
Multi$PropType<-ifelse(Multi$FULL_ADDRESS %in% c("1400 K ST SE","1402 K ST SE","1404 K ST SE"), "Multi-Two Unit",
                       ifelse(Multi$FULL_ADDRESS %in% c("1341 IRVING ST NW"),"Multi Family",Multi$PropType))

#All residential builds since 2012 differentiating b/w SFD, duplex and multi
ResBuilding<-rbind(SFD,Multi)

NullIsland<-subset(ResBuilding, ResBuilding$LATITUDE==0.00000)
neverNull<-subset(ResBuilding, ResBuilding$LATITUDE!=0.00000)
Null<-paste(NullIsland$FULL_ADDRESS," Washington, DC",sep="")

Address<-geocode(Null, source=c("google"))
noMoreNull<-cbind(NullIsland,Address)[-c(20:21)]
colnames(noMoreNull)[c(38:39)]<-c("LONGITUDE","LATITUDE")

ResBuildingOut<-rbind(neverNull,noMoreNull)
ResBuildingOut<-subset(ResBuildingOut, abs(ResBuildingOut$LONGITUDE)<80)[c(3,20,21,39)]

write.csv(ResBuildingOut,"ResBuild.csv",row.names=FALSE)

clusterMap = readOGR("http://opendata.dc.gov/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson", "OGRGeoJSON")
ResLL<-ResBuildingOut[c(21,20)]

addAll<-SpatialPoints(ResLL, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodID <- over(addAll, clusterMap )
ResBuildHood<-cbind(ResBuildingOut,HoodID)


HoodSums<-ddply(ResBuildHood,c("NBH_NAMES","PropType"),summarise,
      Hood=length(NBH_NAMES))

NoHood<-subset(resBuild,resBuild$NEIGHBORHOODCLUSTER=="NONE")
