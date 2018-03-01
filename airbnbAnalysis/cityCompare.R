require(tidyverse)
here()

### read in and standardize city data ###
### read in and standardize city data ###
### read in and standardize city data ###
temp = here(list.files(pattern="*.csv"))
temp <- temp[!(temp %in% c("cityHousing.csv", "listings0517.csv", "listings1015.csv", "dcReviews.csv"))]

airbnb <- lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
                 function(x) read.csv(here(x), stringsAsFactors = FALSE, strip.white = TRUE))

airbnb$denverBnb <- airbnb$denverBnb %>% mutate(neighbourhood_group = rep("NA", nrow(airbnb$denverBnb)))

active <- airbnb %>% map(., ~ subset(.x, availability_365 != 0 & room_type=="Entire home/apt")) %>%
                     map(., ~ mutate(.x, last_reviewDt = as.Date(last_review, "%Y-%m-%d"))) %>%
                     map(., ~ select(.x, id, name, host_id, neighbourhood_group, room_type, 
                                     last_reviewDt, number_of_reviews, availability_365))

yrPrior <- c("2016-03-07", "2016-10-06", "2016-05-10", "2016-05-10", "2016-05-16", "2016-05-02", "2016-09-14",
             "2016-06-02", "2016-05-02", "2016-05-02") 

yrReview <- function (x, y) {
  x %>% subset(number_of_reviews==0 | last_reviewDt > y)
}

active <- map2(active, yrPrior, yrReview)
 
airbnb <- active %>% reduce(rbind) %>%
                     mutate(city = rep(names(active), sapply(active, nrow)),
                            city = ifelse(city == "nycBnb", neighbourhood_group, city), 
                            cityShort = ifelse(city=="dcBnb", "WashingtonDCBnb", city)) %>%
                     select(-neighbourhood_group, -city)
                     
airbnb2 <- airbnb %>% filter(!grepl("Staten", cityShort))

### Calculate airbnb units per housing units ###
### Calculate airbnb units per housing units ###
### Calculate airbnb units per housing units ###
citySums <- airbnb2 %>% group_by(cityShort) %>% summarise(count = n()) %>%
                                          arrange(cityShort)

cityHouse <- read.csv("cityHousing.csv", stringsAsFactors = FALSE, strip.white = TRUE)[c(1:2)] %>% 
             filter(city != "") %>% arrange(city) %>%
             mutate(houseUnits = as.numeric(gsub(",", "", houseUnits)))

bnbPC <- cbind(cityHouse, citySums) %>% mutate(pc = (count/ houseUnits) * 100) %>% arrange(desc(pc))

ggplot(data=bnbPC, aes(x=reorder(city, pc) , y=pc)) +
  geom_bar(stat="identity", fill = "#4f6460") +
  coord_flip() +
  scale_y_continuous(limits = c(0,2)) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank()) +   
  labs(y="", x="", 
       title="Airbnb ",
       subtitle = "Number of Active Airbnb Rentals Per 100 Housing Units",
       caption = "\nSource: insideairbnb.com, Census") +
  ggsave(file="cityCompare.svg", width=7, height=5)

### hosts that span cities ###
### hosts that span cities ###
### hosts that span cities ###
hostCitySums <- airbnb %>% filter(room_type!="Shared room") %>% 
                           group_by(host_id, cityShort) %>% summarise(count = n()) %>%
                           arrange(desc(host_id,count))

dcHostSums <- hostCitySums %>% filter(cityShort == "WashingtonDCBnb")

multiCityHosts <- hostCitySums %>% group_by(host_id) %>% summarise(count = n()) %>%
  filter (count > 1)

hostCitySums <- hostCitySums %>% filter(host_id %in% multiCityHosts$host_id) %>%
                                 filter(host_id %in% dcHostSums$host_id)
length(unique(hostCitySums$host_id))
