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