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
setwd ("/Users/katerabinowitz/Documents/DataLensDC/Prop Sales Flip Analysis/MRIS data")
files = list.files(pattern="*.csv")

my_data <- list()
for (i in seq_along(files)) {
  my_data[[i]] <- read.csv(file = files[i], stringsAsFactors=FALSE, strip.white=TRUE)[c(1:4,6:9)]
}
propsale<-(rbindlist(my_data))
setwd ("/Users/katerabinowitz/Documents/DataLensDC/Prop Sales Flip Analysis/Prop-Flip-Analysis/CensusGeocode")

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
address<-as.data.frame(FlippedHomes$Address)
address$city<-rep("Washington",1426)
address$state<-rep("DC",1426)
address$zip<-FlippedHomes$Zip
geoaddress1<-head(address,999)
geoaddress2<-tail(address,427)
write.csv(geoaddress1,"geoaddress1.csv")
write.csv(geoaddress2,"geoaddress2.csv")
GeoFlip1<-read.csv('FullGeocodeResults1.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(6)]
GeoFlip2<-read.csv('FullGeocodeResults2.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(6)]
GeoFlip<-rbind(GeoFlip1,GeoFlip2)
GeoFlip$LongLat<-as.character(GeoFlip$LongLat)
GeoFlip$lon<-as.numeric(gsub(",.*$", "", GeoFlip$LongLat))
GeoFlip$lat<-as.numeric(gsub(".*,","",GeoFlip$LongLat))
GeoFlip<-na.omit(GeoFlip)
GeoFlip<-GeoFlip[c(2:3)]

addAll<-SpatialPoints(GeoFlip, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodID <- over(addAll, map )
FlipSaleHoodSum<-ddply(HoodID , c("HoodID $subhood"), nrow)
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
GeoFlipS$lon<-as.numeric(gsub(",.*$", "", GeoFlipS$LongLat))
GeoFlipS$lat<-as.numeric(gsub(".*,","",GeoFlipS$LongLat))
GeoFlipS<-GeoFlipS[c(3:4)]

addAllS<-SpatialPoints(GeoFlipS, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodIDS <- over(addAllS, map )

FlipHoodSSum<-ddply(HoodIDS, c("HoodIDS$subhood"), nrow)
colnames(FlipHoodSSum)<-c("subhood","propflipR")

### All Prop Sales - 2013-2015 ###
### All Prop Sales - 2013-2015 ###
### All Prop Sales - 2013-2015 ###
#geocode first pass Census, second pass Google 
SingleFamily<-subset(propsale,grepl('#',propsale$Address)==FALSE)
ShortSaleZip<-subset(SingleFamily,SingleFamily$Date > as.Date("2013-9-30"))
SAddress<-as.data.frame(ShortSaleZip$Address)
SAddress$city<-rep("Washington",8095)
SAddress$state<-rep("DC",8095)
SAddress$zip<-ShortSaleZip$Zip
#remove addresses already geocoded
SGeocode<-SAddress[!(ShortSaleZip$Address %in% ShorterSales$Address),]
colnames(SGeocode)<-c('address','city','state','zip')
SGeoDone<-SAddress[(ShortSaleZip$Address %in% ShorterSales$Address),]

split<-split(SGeocode, sample(rep(1:7)))
for (i in seq_along(split)) {
  filename = paste(i, ".csv")
  write.csv(split[[i]], filename)
}

ShortGeocode1<-read.csv('AllShortGeocode1.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode2<-read.csv('AllShortGeocode2.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode3<-read.csv('AllShortGeocode3.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode4<-read.csv('AllShortGeocode4.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode5<-read.csv('ShortGeocode5.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode6<-read.csv('ShortGeocode6.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode7<-read.csv('ShortGeocode7.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
ShortGeocode<-rbind(ShortGeocode1,ShortGeocode2,ShortGeocode3,ShortGeocode4,ShortGeocode5,
                    ShortGeocode6,ShortGeocode7)
SGNM<-subset(ShortGeocode,ShortGeocode$Match=="No_Match" | ShortGeocode$Match=="Tie")
SGM<-subset(ShortGeocode,ShortGeocode$Match=="Match")
SGNMaddress<-as.character(SGNM$Address)
SGNM.LL<-geocode(SGNMaddress,source="google")

SGM$LongLat<-as.character(SGM$LongLat)
SGM$lon<-as.numeric(gsub(",.*$", "", SGM$LongLat))
SGM$lat<-as.numeric(gsub(".*,","",SGM$LongLat))
SGM<-SGM[c(4:5)]
ShorterSales<-rbind(SGNM.LL,SGM,GeoFlipS)
ShorterSales<-na.omit(ShorterSales)

#Add Neighborhood
addAll<-SpatialPoints(ShorterSales, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
SHoodID <- over(addAll, map )

ShortSaleHood<-ddply(SHoodID, c("SHoodID$subhood"), nrow)
colnames(ShortSaleHood)<-c("subhood","propsale")

PropFlipShort <- merge(ShortSaleHood,FlipHoodSSum, by="subhood", all.x=TRUE)
PropFlipShort$propflipR[is.na(PropFlipShort$propflipR)] <- 0

PropFlipShort$flipProp<-(PropFlipShort$propflipR/PropFlipShort$propsale)*100
PropFlipShort$flipProp[is.na(PropFlipShort$flipProp)] <- 0
PropFlipShort<-PropFlipShort[order(PropFlipShort$flipProp),]
PropFlipShort<-subset(PropFlipShort,PropFlipShort$propsale>2)

### All Prop Sales - 2011-2015 ###
### All Prop Sales - 2011-2015 ###
### All Prop Sales - 2011-2015 ###
AllSaleZip<-subset(SingleFamily,SingleFamily$Zip %in% c(20001,20002,20003,20004,20005,20006,20007,20008,
                                                   20009,20010,20011,20012,20017,20018,20036))
toGC<-AllSaleZip[!(AllSaleZip$Address %in% SGeocode$address),]
toGeoCode<-toGC[!(toGC$Address %in% FlippedHomes2011$Address)]

GC<-as.data.frame(toGeoCode$Address)
GC$city<-rep("Washington",4994)
GC$state<-rep("DC",4994)
GC$zip<-toGeoCode$Zip

Fsplit<-split(GC, sample(rep(1:5)))
for (i in seq_along(Fsplit)) {
  filename = paste(i, ".csv")
  write.csv(Fsplit[[i]], filename)
}
Geocode1<-read.csv('AllGeocode1.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
Geocode2<-read.csv('AllGeocode2.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
Geocode3<-read.csv('AllGeocode3.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
Geocode4<-read.csv('AllGeocode4.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
Geocode5<-read.csv('AllGeocode5.csv',na.strings=c("", "NA"),strip.white=TRUE,)[c(2:3,6)]
Geocode<-rbind(Geocode1,Geocode2,Geocode3,Geocode4,Geocode5)
GNM<-subset(Geocode,Geocode$Match=="No_Match" | Geocode$Match=="Tie")
GNMaddress<-as.character(GNM$Address)
GNM.LL<-geocode(GNMaddress,source="google")

GM<-subset(Geocode,Geocode$Match=="Match")
GM$LongLat<-as.character(GM$LongLat)
GM$lon<-as.numeric(gsub(",.*$", "", GM$LongLat))
GM$lat<-as.numeric(gsub(".*,","",GM$LongLat))
GM<-GM[c(4:5)]

Sales<-rbind(GNM.LL,GM,GeoFlip)
Sales<-na.omit(Sales)

#Add Neighborhood
SPoint<-SpatialPoints(Sales, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
SaleHoodID <- over(SPoint, map )
SaleHood<-ddply(SaleHoodID, c("SaleHoodID$subhood"), nrow)
colnames(SaleHood)<-c("subhood","propsale")
#add ShorterSales in zip codes for whole timeframe
AllSaleHood<-merge(SaleHood,ShortSaleHood, by="subhood", all.x=TRUE)
AllSaleHood$propsale<-AllSaleHood$propsale.x+AllSaleHood$propsale.y
AllSaleHood<-AllSaleHood[c(1,4)]

PropFlip <- merge(AllSaleHood,FlipSaleHoodSum, by="subhood", all.x=TRUE)
PropFlip$propflipR[is.na(PropFlip$propflipR)] <- 0
PropFlip$flipProp<-(PropFlip$propflipR/PropFlip$propsale)*100
#Remove hoods where even part in incomplete history zip codes
PropFlip<-subset(PropFlip, PropFlip$subhood!="Rock Creek Park (north)" & PropFlip$subhood!="Chevy Chase" 
                    & PropFlip$subhood!="Friendship Heights" & PropFlip$subhood!="Tenleytown" &
                       PropFlip$subhood!="Spring Valley" & PropFlip$subhood!="Marshall Heights" &PropFlip$propsale>2)

PropFlip<-PropFlip[order(PropFlip$flipProp),]

###Create GeoJson###
###Create GeoJson###
###Create GeoJson###
setwd ("/Users/katerabinowitz/Documents/DataLensDC/Prop Sales Flip Analysis/Prop-Flip-Analysis")

shortmap<-map
fullmap<-map
fullhistorymapV2 <- merge(fullmap,PropFlip, by="subhood", all.x=TRUE)
shorthistorymapV2 <- merge(shortmap,PropFlipShort, by="subhood",all.x=TRUE)

writeOGR(fullhistorymapV2, 'fullhistorymapV2.geojson','fullhistorymapV2', driver='GeoJSON',check_exists = FALSE)
writeOGR(shorthistorymapV2, 'shorthistorymapV2.geojson','shorthistorymapV2', driver='GeoJSON',check_exists = FALSE)

Flip2011<-subset(PropFlip,PropFlip$propsale>10)
Flip2011$subhood<-gsub("/.*$", "", Flip2011$subhood)
Flip2011<-na.omit(Flip2011)
Flip2011<-Flip2011[order(-Flip2011$flipProp),]
Flip2013<-subset(PropFlipShort,PropFlipShort$propsale>10)
Flip2013<-na.omit(Flip2013)
Flip2013<-Flip2013[order(-Flip2013$flipProp),]
Flip2011<-head(Flip2011,20)
Flip2013<-head(Flip2013,20)
Flip2011$flipPropPer<-paste((round(Flip2011$flipProp, 2)),"%",sep="")
Flip2013$flipPropPer<-paste((round(Flip2013$flipProp, 2)),"%",sep="")

write.csv(Flip2011,"fullFlipHistory.csv")
write.csv(Flip2013,"shorterHistory.csv")
