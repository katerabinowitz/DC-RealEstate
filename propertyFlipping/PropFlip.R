library(DataCombine)
library(data.table)
library(dplyr)
library(plyr)
library(ggmap)
library(rgdal)
library(sp)
### Read in Data ###
### Read in Data ###
### Read in Data ###
setwd ("/Users/katerabinowitz/Documents/DataLensDC/Large Data Input/MRIS data")
files = list.files(pattern="*.csv")

my_data <- list()
for (i in seq_along(files)) {
  my_data[[i]] <- read.csv(file = files[i], stringsAsFactors=FALSE, strip.white=TRUE)[c(1:4,6:9)]
}
propsale<-(rbindlist(my_data))
setwd ("/Users/katerabinowitz/Documents/DataLensDC/Prop Sales Flip Analysis/Prop-Flip-Analysis")

### Data Cleaning ###
### Data Cleaning ###
### Data Cleaning ###
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
propsale$List.Price<-trim(propsale$List.Price)
propsale$Close.Price<-trim(propsale$Close.Price)

propsale$List.Price<-trim(substring(propsale$List.Price, 2))
propsale$Close.Price<-trim(substring(propsale$Close.Price, 2))

propsale$List.Price<-as.numeric(gsub(",","",propsale$List.Price))
propsale$Close.Price<-as.numeric(gsub(",","",propsale$Close.Price))

#unify date structure
dateToFix<- propsale[grepl('-', propsale$CloseDate), ]
goodDate<-subset(propsale,!(propsale$CloseDate %in% dateToFix$CloseDate))

date<-as.data.frame(strsplit(dateToFix$CloseDate, "-"))
date<-as.data.frame(t(date))
date$m<-rep(4,508)
date$Date<-paste(date$m,'/',date$V1,'/',date$V3,sep='')
date<-date[c(5)]
dateFixed<-cbind(dateToFix,date)
dateFixed$CloseDate<-dateFixed$Date
dateFixed$Date <-NULL

propsale<-rbind(dateFixed,goodDate)
propsale$Date <- as.Date(propsale$CloseDate, "%m/%d/%y")

#naCheck<-propsale[is.na(propsale$Date)] 
propsale<-propsale[!is.na(propsale$Date)] 
propsale<-propsale[order(propsale$Address),]

propsale$year=year(propsale$Date)
propsale$month<-month(propsale$Date)
propsale$MY<-paste(propsale$year,propsale$month)
propsale$StrAddress<-sub('\\ #.*', '', propsale$Address)
write.csv(propsale,"DCPropSale511915.csv")

#zipCheck<-ddply(propsale, c("Zip","MY"), nrow)
#uneven historical coverage by zip code

### Subset to Properties with Multiple Sales ###
### Subset to Properties with Multiple Sales ###
### Subset to Properties with Multiple Sales ###
dupsales<-unique(propsale$Address[duplicated(propsale$Address)])

fliphomes<- propsale[propsale$Address %in% dupsales, ]
fliphomes<-fliphomes[order(fliphomes$Address,fliphomes$Date),]

fliphomes$AddressLag<-lag(fliphomes$Address)
fliphomes$DateLag<-lag(fliphomes$Date)
fliphomes$ListLag<-lag(fliphomes$List.Price)
fliphomes$CloseLag<-lag(fliphomes$Close.Price)
fliphomes$match<-ifelse(fliphomes$AddressLag==fliphomes$Address, 1, 0)

fliphomes2<-subset(fliphomes,fliphomes$match==1)
fliphomes2 <- subset( fliphomes2, select = -c(CloseDate, MY, month,AddressLag,match,StrAddress) )
fliphomes2$DaysBtwn<-fliphomes2$Date-fliphomes2$DateLag
fliphomes2$gain<-fliphomes2$List.Price-fliphomes2$ListLag
flip2yr<-subset(fliphomes2,fliphomes2$DaysBtwn<730 & fliphomes2$DaysBtwn>30)

### Find Single Family Homes Converted to Condos ###
### Find Single Family Homes Converted to Condos ###
### Find Single Family Homes Converted to Condos ###
dupsalesProp<-unique(propsale$StrAddress[duplicated(propsale$StrAddress)])

Addmatch<- propsale[propsale$StrAddress %in% dupsalesProp, ]
Addmatch<-Addmatch[order(Addmatch$Address,Addmatch$Date),]
Addmatch$AddressLag<-lag(Addmatch$Address)
Addmatch$DateLag<-lag(Addmatch$Date)
Conversion<-subset(Addmatch,Addmatch$AddressLag==Addmatch$StrAddress &
                     (grepl('#1',Addmatch$Address)==TRUE| grepl('#2',Addmatch$Address)==TRUE) &
                     Addmatch$DateLag<Addmatch$Date)
Conversion<-Addmatch[Addmatch$StrAddress %in% Conversion$StrAddress, ]
Conversion<-Conversion[order(Conversion$Address,Conversion$Date),]
Conversion$AddressLag<-lag(Conversion$Address)
Conversion$DateLag<-lag(Conversion$Date)
Conversion$ListLag<-lag(Conversion$List.Price)
Conversion$CloseLag<-lag(Conversion$Close.Price)
Conversion$DaysBtwn<-Conversion$Date-Conversion$DateLag
Conversion$gain<-123456789

Conversion<-subset(Conversion,Conversion$AddressLag==Conversion$StrAddress)
Conversion$Address<-Conversion$AddressLag
Conversion <- subset(Conversion, select = -c(CloseDate, MY, month,AddressLag, StrAddress) )
Conv2yr<-subset(Conversion,Conversion$DaysBtwn<730 & Conversion$DaysBtwn>30)

### Full Record of Flipped Single Homes and Single Home to Condo Conversions ###
### Full Record of Flipped Single Homes and Single Home to Condo Conversions ###
### Full Record of Flipped Single Homes and Single Home to Condo Conversions ###
map = readOGR("http://ec2-54-235-58-226.compute-1.amazonaws.com/storage/f/2013-05-12T03%3A50%3A18.251Z/dcneighorhoodboundarieswapo.geojson", "OGRGeoJSON")

FlippedProp<-rbind(flip2yr,Conv2yr)
FlippedHomes<-subset(FlippedProp,grepl('#',FlippedProp$Address)==FALSE)
FlippedHomes2011<-subset(FlippedHomes,FlippedHomes$Zip %in% c(20001,20002,20003,20004,20005,20006,20007,20008,
                                                        20009,20010,20011,20012,20017,20018,20036))
#Use Census geocode (limit 1000 records)
address<-as.data.frame(FlippedHomes2011$Address)
address$city<-rep("Washington",1302)
address$state<-rep("DC",1302)
address$zip<-FlippedHomes2011$Zip
geoaddress1<-head(address,999)
geoaddress2<-tail(address,302)
write.csv(geoaddress1,"geoaddress1.csv")
write.csv(geoaddress2,"geoaddress2.csv")
GeoFlip1<-read.csv('GeocodeResults1.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(6)]
GeoFlip2<-read.csv('GeocodeResults2.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(6)]
GeoFlip<-rbind(GeoFlip1,GeoFlip2)
GeoFlip$LongLat<-as.character(GeoFlip$LongLat)
GeoFlip$Long<-as.numeric(gsub(",.*$", "", GeoFlip$LongLat))
GeoFlip$Lat<-as.numeric(gsub(".*,","",GeoFlip$LongLat))
GeoFlip<-na.omit(GeoFlip)
GeoFlip<-GeoFlip[c(2:3)]

addAll<-SpatialPoints(GeoFlip, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodID <- over(addAll, map )
FlipSaleHood<-cbind(FlippedHomes,GeoFlip,HoodID)
FlipSaleHoodSum<-ddply(FlipSaleHood, c("FlipSaleHood$subhood"), nrow)
colnames(FlipSaleHoodSum)<-c("subhood","propflipR")

#2013-2015
ShorterSales<-subset(FlippedHomes,FlippedHomes$Date > as.Date("2013-9-30"))
Saddress<-as.data.frame(ShorterSales$Address)
Saddress$city<-rep("Washington",835)
Saddress$state<-rep("DC",835)
Saddress$zip<-ShorterSales$Zip
write.csv(Saddress,"Saddress.csv")

GeoFlipS<-read.csv('GeocodeResultsShort.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(6)]
GeoFlipS$LongLat<-as.character(GeoFlipS$Long)
GeoFlipS$Long<-as.numeric(gsub(",.*$", "", GeoFlipS$LongLat))
GeoFlipS$Lat<-as.numeric(gsub(".*,","",GeoFlipS$LongLat))
GeoFlipS<-GeoFlipS[c(1,3)]

addAllS<-SpatialPoints(GeoFlipS, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodIDS <- over(addAllS, map )
FlipHoodS<-cbind(ShorterSales,GeoFlipS,HoodIDS)
FlipHoodSSum<-ddply(FlipHoodS, c("FlipHoodS$subhood"), nrow)
colnames(FlipHoodSSum)<-c("subhood","propflipR")

### All Prop Sales - 2011-2015 ###
### All Prop Sales - 2011-2015 ###
### All Prop Sales - 2011-2015 ###
SingleFamily<-subset(propsale,grepl('#',propsale$Address)==FALSE)
AllSaleZip<-subset(SingleFamily,SingleFamily$Zip %in% c(20001,20002,20003,20004,20005,20006,20007,20008,
                                                   20009,20010,20011,20012,20017,20018,20036))
#Break up addresses to use google geocode
geoaddressAll<-AllSaleZip$Address
geoaddress1<-geoaddressAll[1:2499]
geoaddress2<-geoaddressAll[2500:4999]
geoaddress3<-geoaddressAll[5000:7499]
geoaddress4<-geoaddressAll[7500:9999]
geoaddress5<-geoaddressAll[10000:12499]
geoaddress6<-geoaddressAll[12500:12686]
address1<-paste(geoaddress1,", Washington DC",sep="")
address2<-paste(geoaddress2,", Washington DC",sep="")
address3<-paste(geoaddress3,", Washington DC",sep="")
address4<-paste(geoaddress4,", Washington DC",sep="")
address5<-paste(geoaddress5,", Washington DC",sep="")
address6<-paste(geoaddress6,", Washington DC",sep="")
latlong1<-geocode(address1)
latlong2<-geocode(address2)
latlong3<-geocode(address3)
latlong4<-geocode(address4)
latlong5<-geocode(address5)
latlong6<-geocode(address6)
latlongAll<-rbind(latlong1,latlong2,latlong3,latlong4,latlong5,latlong6)
#Add Neighborhood
addAll<-SpatialPoints(latlongAll, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
ZipID <- over(addAll, map )
AllSaleHood<-cbind(AllSaleZip,latlongAll,ZipID)

allHoodSum<-ddply(AllSaleHood, c("AllSaleHood$subhood"), nrow)
colnames(allHoodSum)<-c("subhood","propsale")

PropFlip <- merge(allHoodSum,FlipSaleHoodSum, by="subhood", all.x=TRUE)
PropFlip$flipsale[is.na(PropFlip$flipsale)] <- 0
PropFlip$flipProp[is.na(PropFlip$flipProp)] <- 0
PropFlip$flipProp<-(PropFlip$flipsale/PropFlip$propsale)*100
PropFlip<-PropFlip[order(PropFlip$flipProp),]

### All Prop Sales - 2013-2015 ###
### All Prop Sales - 2013-2015 ###
### All Prop Sales - 2013-2015 ###
ShortSaleZip<-subset(SingleFamily,SingleFamily$Date > as.Date("2013-9-30"))
geoaddressAll<-ShortSaleZip$Address
geoaddress1<-geoaddressAll[1:2499]
geoaddress2<-geoaddressAll[2500:4999]
geoaddress3<-geoaddressAll[5000:7499]
geoaddress4<-geoaddressAll[7500:8095]
address1<-paste(geoaddress1,", Washington DC",sep="")
address2<-paste(geoaddress2,", Washington DC",sep="")
address3<-paste(geoaddress3,", Washington DC",sep="")
address4<-paste(geoaddress4,", Washington DC",sep="")
latlong1<-geocode(address1)
latlong2<-geocode(address2)
latlong3<-geocode(address3)
latlong4<-geocode(address4)
latlongAll<-rbind(latlong1,latlong2,latlong3,latlong4)
#Add Neighborhood
addAll<-SpatialPoints(latlongAll, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
ZipID <- over(addAll, map )
ShortSaleHood<-cbind(ShortSaleZip,latlongAll,ZipID)

ShortSaleHood<-ddply(ShortSaleHood, c("ShortSaleHood$subhood"), nrow)
colnames(ShortSaleHood)<-c("subhood","propsale")

PropFlipShort <- merge(ShortSaleHood,FlipSaleHoodShort, by="subhood", all.x=TRUE)
PropFlipShort$flipsale[is.na(PropFlipShort$flipsale)] <- 0
PropFlipShort$flipProp[is.na(PropFlipShort$flipProp)] <- 0
PropFlipShort$flipProp<-(PropFlipShort$flipsale/PropFlipShort$propsale)*100
PropFlipShort<-PropFlipShort[order(PropFlipShort$flipProp),]

###Create GeoJson###
###Create GeoJson###
###Create GeoJson###
shortmap<-map
fullmap<-map
fullhistorymap <- merge(fullmap,PropFlip, by="subhood", all.x=TRUE)
shorthistorymap <- merge(shortmap,PropFlipShort, by="subhood",all.x=TRUE)

writeOGR(fullhistorymap, 'fullhistorymap.geojson','fullhistorymap', driver='GeoJSON',check_exists = FALSE)
writeOGR(shorthistorymap, 'shorthistorymap.geojson','shorthistorymap', driver='GeoJSON',check_exists = FALSE)

Flip2011<-subset(PropFlip,PropFlip$propsale>10)
Flip2011<-Flip2011[order(-Flip2011$flipProp),]
Flip2013<-subset(PropFlipShort,PropFlipShort$propsale>10)
Flip2013<-Flip2013[order(-Flip2013$flipProp),]
Flip2011<-head(Flip2011,20)
Flip2013<-head(Flip2013,20)
Flip2011$flipPropPer<-paste((round(Flip2011$flipProp, 2)),"%",sep="")
Flip2013$flipPropPer<-paste((round(Flip2013$flipProp, 2)),"%",sep="")

write.csv(Flip2011,"fullFlipHistory.csv")
write.csv(Flip2013,"shorterHistory.csv")

FHmap = readOGR("fullhistorymap.geojson", "OGRGeoJSON")
FHMapDF<-as.data.frame(FHmap@data)
FHMapDF<-FHMapDF[order(-FHMapDF$flipsale),]

SHmap = readOGR("shorthistorymap.geojson", "OGRGeoJSON")
SHMapDF<-as.data.frame(SHmap@data)
SHhistorymap <- merge(SHMapDF,FlipHoodSSum, by="subhood", all.x=TRUE)
SHhistorymap$propsale<-as.numeric(as.character(SHhistorymap$propsale))
SHhistorymap$flippropC<-ifelse((SHhistorymap$propsale>0) & (is.null(SHhistorymap$flippropR)),0,SHhistorymap$flippropR)
SHhistorymap$flippropR<-is.numeric(SHhistorymap$flippropR)
SHhistorymap$flipPropped<-(SHhistorymap$propflipR/SHhistorymap$propsale)
SHhistorymap<-SHhistorymap[order(-SHhistorymap$flipPropped),]

str(SHhistorymap)
