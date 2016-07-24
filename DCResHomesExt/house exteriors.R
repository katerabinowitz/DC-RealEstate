setwd("/Users/katerabinowitz/Documents/DataLensDC/DC-Real Estate/DCResHomesExt")
library(rgdal)
library(plyr)
library(reshape2)

#Read in assessment data and group exterior data
resCAMA = readOGR("http://opendata.dc.gov/datasets/c5fb3fbe4c694a59a6eef7bf5f8bc49a_25.geojson", "OGRGeoJSON")
res<-data.frame(resCAMA@data)
coords<-data.frame(resCAMA@coords)
resAssess<-cbind(res,coords)

str(resAssess)
table(resAssess$EXTWALL_D)
attach(resAssess)
resAssess$Outer<-ifelse(EXTWALL_D=="Common Brick","Brick",
                  ifelse(EXTWALL_D=="Brick Veneer"|EXTWALL_D=="Brick/Siding"|EXTWALL_D=="Face Brick"|
                  EXTWALL_D=="Brick/Stone"|EXTWALL_D=="Brick/Stucco","Brick Veneer or Combination",
                    ifelse(EXTWALL_D=="Aluminum"|EXTWALL_D=="Vinyl Siding","Aluminum/Vinyl Siding",
                      ifelse(EXTWALL_D=="Shingle"|EXTWALL_D=="Wood Siding","Wood/Shingles",
                        ifelse(EXTWALL_D=="Stone"|EXTWALL_D=="Stone Veneer"|EXTWALL_D=="Stone/Siding"|
                                 EXTWALL_D=="Stone/Stucco","Stone or Stone Veneer",
                          ifelse(EXTWALL_D=="Stucco","Stucco","Other"))))))

#Aggregate to ward and look at distribution
ward=readOGR("http://opendata.dc.gov/datasets/0ef47379cbae44e88267c01eaec2ff6e_31.geojson", "OGRGeoJSON")
latLong<-SpatialPoints(coords, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
wardID <- over(latLong, ward )[c(2)]

assess<-cbind(resAssess,wardID)
extSum<-count(assess, c("Outer", "WARD"))

ext<-dcast(extSum,WARD~Outer)[c(1:8)]
ward<-ext[c(1)]
ext<-ext[c(2:8)]

ext.percent <- ext / rowSums(ext, na.rm = T)
extW<-cbind(ward,ext.percent)
extM<-melt(extW,id="WARD")

#export csv of households and ward aggregates
write.csv(assess,"resAssess.csv")
write.csv(extM,"wardExtSum.csv")

### Groupings for Inner/Outer City Home Differences - home type, yr built, land area ### 
### Groupings for Inner/Outer City Home Differences - home type, yr built, land area ### 
### Groupings for Inner/Outer City Home Differences - home type, yr built, land area ### 
rm(list=ls())
assess<-read.csv("resAssess.csv",
                 fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)
colnames(assess)
str(assess)

table(assess$STRUCT_D)
assess$structureGroup<-ifelse(assess$STRUCT_D %in% c("Multi","Multi Senr"),"Multi",
                        ifelse(assess$STRUCT_D %in% c("Row End","Row Inside"),"Row House",
                          ifelse(assess$STRUCT_D %in% c("Default","Vacant Land","NA's"),"Other",
                            ifelse(assess$STRUCT_D %in% c("Town Inside","Town End"),"Town House",
                              assess$STRUCT_D))))
table(assess$STRUCT_D,assess$structureGroup)
strWard<-count(assess, c("WARD","structureGroup"))
per <- ddply(strWard, .(WARD), summarise, structureGroup = structureGroup, pct = freq / sum(freq))
strByWard <- dcast(per, WARD ~ structureGroup)

quantile(assess$LANDAREA, c(.05,.25, .5,.75,.9,.95,.99)) 
assess$landGroup<-ifelse(assess$LANDAREA<1600,"Less than 1600 sq.ft.",
                    ifelse(assess$LANDAREA<2500,"1600-2500 sq.ft.",
                      ifelse(assess$LANDAREA<4000,"2500-4000 sq.ft.",
                          "4000+ sq.ft.")))

quantile(assess$AYB, c(.05, .25, .5,.75,.9,.95,.99),na.rm=TRUE) 
sum(is.na(assess$AYB))
assess$yearGroup<-ifelse(assess$AYB<1915,"Before 1915",
                         ifelse(assess$AYB<1930,"1915-30",
                                ifelse(assess$AYB<1950,"1930-50",
                                       "After 1950")))
medYL<-ddply(assess, .(WARD), summarize, "medYear"= median(AYB,na.rm=TRUE), "medLand" = median(LANDAREA,na.rm=TRUE))
row<-strByWard[c(1,4)]
resSum<-merge(row,medYL,by="WARD")
resSum$zero<-rep(0,8)
colnames(resSum)[c(2)]<-"rowHomeP"
write.csv(resSum,"resGroupSum.csv")

colnames(assess)
resGroupAssess<-assess[c(2,48:49,52:54)]
write.csv(resGroupAssess,"resGroupAssess.csv")

resGroups<- SpatialPointsDataFrame(resGroupAssess[,c(2,3)],resGroupAssess[,-c(2,3)])

writeOGR(resGroups, 'resGroups.geojson','resGroups', driver='GeoJSON',check_exists = FALSE)
