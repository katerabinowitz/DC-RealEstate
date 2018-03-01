require(tidyverse)
require(rgdal)
here()

temp = c("listings0517.csv", "listings1015.csv")
airbnb <- lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         function(x) read.csv(here(x), stringsAsFactors = FALSE, strip.white = TRUE))

reviews <- read.csv(here("dcReviews.csv"), 
                    stringsAsFactors = FALSE, strip.white = TRUE) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(date > "2016-05-10") %>% 
  group_by(listing_id) %>% summarise(reviewN = n())  %>%
  rename(id = listing_id)

### airbnb in dc overview ###
### airbnb in dc overview ###
### airbnb in dc overview ###

# subset all airbnb listings to active listings. 
# active is defined as availability in the next year and reviews within the past year
active <- airbnb %>% map(., ~ mutate(.x, last_reviewDt = as.Date(last_review, "%Y-%m-%d")))

active$listings0517 <- active$listings0517 %>% filter(last_reviewDt > "2016-05-10")
active$listings1015 <- active$listings1015 %>% filter(last_reviewDt > "2014-10-02")

active <-  active %>% map(., ~ subset(.x, availability_365 != 0))

#breakdown of room types
map(active, ~ prop.table(table(.$room_type)))
#growth of active listings over time
nrow(active$listings0517)/nrow(active$listings1015)

#median price by room type
active$listings0517 %>% group_by(room_type) %>% summarise(medPrice = median(price))

# host change breakdown
length(unique(active$listings0517$host_id))
nrow(anti_join(active$listings0517, active$listings1015, by="host_id") %>% distinct(host_id))
nrow(anti_join(active$listings1015, active$listings0517, by="host_id") %>% distinct(host_id))
nrow(inner_join(active$listings1015, active$listings0517, by="host_id") %>% distinct(host_id))

# summary of counts and proportions across neighborhoods
neighborhoodSum <- active %>% map(., ~ group_by(.x, neighbourhood)) %>%
                              map(., ~ summarise(.x, count = n())) %>%
                              reduce(full_join, by = "neighbourhood") %>%
                              rename(count17 = count.x, count15 = count.y) %>%
                              mutate(prop17 = (count17 / sum(count17)) * 100, 
                                     prop15 = (count15 / sum(count15)) * 100, 
                                     chgProp = prop17 - prop15) %>%
                              arrange(prop17) 

typeSum <- active %>% map(., ~ group_by(.x, neighbourhood, room_type)) %>%
                      map(., ~ summarise(.x, typeCount = n())) %>%
                      reduce(inner_join, by = "neighbourhood") %>%
                      rename(unitCount17 = typeCount.x, unitCount15 = typeCount.y) %>% 
                      filter(room_type.x=="Entire home/apt" & room_type.y == "Entire home/apt")

neighborhoodSum <- left_join(neighborhoodSum, typeSum, by="neighbourhood") %>%
                    mutate(unitCount17 = ifelse(is.na(unitCount17), 0, unitCount17),
                           unitProp = round((unitCount17 / count17) * 100,2)) %>%
                    select(neighbourhood, count17, unitProp) %>%
                    arrange(desc(unitProp))
rm(typeSum)

### airbnb commercial host ###
### airbnb commercial host ###
### airbnb commercial host ###

#commercial hosts are those who list multiple whole units or at least three private units
unit <- active %>% map(., ~ subset(.x, room_type!="Shared room"))

# first let's just look at multiple listings
hostN <- unit %>% map(., ~ group_by(.x,host_id, host_name)) %>%
                  map(., ~ summarise(.x, count = n())) %>%
                  map(., ~ mutate(.x, ListCount = case_when( count == 1 ~ "1", 
                                                             count == 2 ~ "2", 
                                                             count < 6 ~ "3 - 5", 
                                                             count > 5 ~ "6 or more")))
prop.table(table(hostN$listings0517$ListCount))
table(hostN$listings0517$ListCount)

# now commercial
hostMulti <- hostN %>%  map(., ~ filter(.x, count > 1))

hostTypeN <- unit %>% map(., ~ group_by(.x, host_id, host_name, room_type)) %>%
                      map(., ~ summarise(.x, countRm = n())) 

hostCounts17 <- left_join(hostMulti$listings0517, hostTypeN$listings0517, by=c("host_id", "host_name")) 
hostCounts15 <- left_join(hostMulti$listings1015, hostTypeN$listings1015, by=c("host_id", "host_name")) 

findComm <- function (dataYr) {
  wholeUnit <- dataYr %>% filter(room_type=="Entire home/apt" & countRm > 1)
  privateRoom <- dataYr %>% filter(!(host_id %in% wholeUnit$host_id)) %>% filter(count > 2)
  commercial <- bind_rows(wholeUnit, privateRoom) %>% select(host_id, host_name, count) %>%
                distinct(host_id, .keep_all=TRUE) %>% arrange(desc(count)) 
  commercial
}

commercial17 <- findComm(hostCounts17)
commercial15 <- findComm(hostCounts15)

# proportion of commercial hosts
length(unique(commercial17$host_id)) / length(unique(active$listings0517$host_id))
length(unique(commercial15$host_id)) / length(unique(active$listings1015$host_id))

# proportion of commercial listings and min night req
airbnb17 <- active$listings0517 %>% mutate(commercial = ifelse(host_id %in% commercial17$host_id, 1, 0))
prop.table(table(airbnb17$commercial))
airbnb17 %>% group_by(commercial) %>% summarise(meanMinNight = mean(minimum_nights),
                                                medMinNight = median(minimum_nights))

# price of commercial listings
airbnb17 %>% group_by(room_type, commercial) %>% summarise(medPrice = median(price),
                                                           meanPrice = mean(price))

# proportion of commercial reviews (proxy for share of stays)
multiListings <- airbnb17 %>% filter(commercial == 1) %>% select(id)
reviews <- reviews %>% mutate(commercial = ifelse(id %in% multiListings$id, 1, 0)) %>% arrange(desc(reviewN))
reviews %>% group_by(commercial) %>% summarise(medReview = median(reviewN),
                                               meanReview = mean(reviewN),
                                               totalReview = sum(reviewN))


multiNeighborhoodSum <- airbnb17 %>% group_by(neighbourhood) %>% 
                                    summarise(total = n(), 
                                              comm = sum(commercial)) %>%
                                    mutate(commProp = round((comm / total)*100,2)) %>%
                                    select(neighbourhood, commProp) 

neighborhoodSum <- inner_join(neighborhoodSum, multiNeighborhoodSum, by="neighbourhood")

megaHost <- commercial17 %>% filter(count > 20) %>% left_join(active$listings0517)

rm(multiNeighborhoodSum, megaHost)

### Look at single-listers ###
### Look at single-listers ###
### Look at single-listers ###
singleOwner <- airbnb17 %>% filter(room_type == "Entire home/apt" & commercial == 0) %>%
                            mutate(min15 = ifelse(minimum_nights > 9, 1, 0))
table(singleOwner$min15)
table(singleOwner$minimum_nights)

# number of reviews to proxy numbers of stays
# we're assuming a review rate of 55%, found last year in a Columbia U study. 
singleOwner <- left_join(singleOwner, reviews, by="id") %>%
                         filter(reviewN != 0) %>%
                         mutate(estStays = ifelse(reviewN / 0.55 > 300, 300, reviewN / 0.55), 
                                estNight = estStays * minimum_nights, 
                                estStayGroup = case_when(estStays < 10 ~ "1-9", 
                                                         estStays < 20 ~ "10-19", 
                                                         estStays < 30 ~ "20-29",
                                                         estStays < 40 ~ "30-39", 
                                                         estStays < 50 ~ "40-49", 
                                                         estStays < 60 ~ "50-59",
                                                         estStays > 59 ~ "60+"),
                                frequent  = ifelse(reviewN > ((60/4.4)*.55) & availability_365 > 119, 1, 0),
                                v_frequent = ifelse(reviewN > ((120/4.4)*.55) & availability_365 > 179, 1, 0))
table(singleOwner$estStayGroup)
table(singleOwner$frequent)
table(singleOwner$v_frequent)

### graphics ###
### graphics ###
### graphics ###

# multiple listings
ggplot(data = hostN$listings0517, aes(x = ListCount , fill = "#4f6460")) + 
  geom_bar(aes(y = (..count..))) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        legend.position="none") +   
  labs(y="", x="") + 
  ggsave("multiBnB.svg", width = 7, height = 5)

# number of stays
ggplot(data = singleOwner, aes(x = estStayGroup, fill = "#4f6460")) + 
  geom_bar(aes(y = (..count..))) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.title = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        legend.position="none") +   
  labs(y="", x="") + 
  ggsave("staysBnB.svg", width = 7, height = 5)
prop.table(table(singleOwner$estStayGroup))


# summary map - output to geojson
hoods = readOGR(dsn=here("neighbourhoods.geojson"), layer="OGRGeoJSON")

neighborhoodSum$neighbourhood
neighborhoodSum$nbhShort <- c("West End, Foggy Bottom", "Downtown, Chinatown", "Capitol Hill, Lincoln Park", 
                              "Kalorama, Adams Morgan", "Dupont Circle, K Street", "Union Station, Kingman Park",
                              "Cardozo/Shaw", "Shaw, Logan Circle", "Georgetown", "Congress Heights, Bellevue",
                              "Cleveland Park, Woodley Park", "Columbia Heights, Mt. Pleasant", "Cathedral Heights, Glover Park",
                              "Brookland, Brentwood", "Palisades, Spring Valley", "Southwest, Waterfront", "Petworth, Crestwood",
                              "Forest Hills, Van Ness", "Navy Yard", "Takoma, Brightwood", "Bloomingdale, Eckington", 
                              "Friendship Heights, Tenleytown", "Woodridge, Ft. Lincoln", "Ivy City, Trinidad", "Fairlawn, Ft. Dupont",
                              "Chevy Chase, Hawthorne", "Hillcrest, Naylor Gardens", "Ft. Totten, Lamont Riggs", "Michigan Park, University Heights",
                              "Mayfair, Hillbrook", "Benning, River Terrace", "Shepherd Park, Colonial Village", "Sherdian, Buena Vista", 
                              "Marshall Heights, Capitol View", "Historic Anacostia", "Woodland/Ft. Stanton", "Kenilworth, Eastland Gardens",
                              "Denwood, Lincoln Heights", "Douglas, Shipley Terrace")


hoodBnbSum  <- merge(hoods, neighborhoodSum, by="neighbourhood", all.x=TRUE) 
writeOGR(hoodBnbSum, 'hoodBnbSum.geojson','hoodBnbSum', driver='GeoJSON',check_exists = FALSE)
