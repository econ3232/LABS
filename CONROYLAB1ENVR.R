install.packages("rmarkdown")
library(tidyverse)
library(ggplot2) #technically included in tidyverse
library(sf)
p.counties <- "../data/CBW/County_Boundaries.shp"
p.stations <- "../data/CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"


d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse(d.stations)

# check for validity
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid() # returns false for one feature, so we need to fix

# fix it "in place"
d.counties <- d.counties %>% sf::st_make_valid()
#1.1
d.counties <- d.counties %>% group_by(STATEFP10) %>%
  mutate(pct_land_of_state = ALAND10 / sum(ALAND10 + AWATER10) * 100) %>% ungroup()
#1.2
county_most_water<-d.counties %>% mutate( total_area = ALAND10 + AWATER10, pct_water = AWATER10 / total_area) %>% group_by(STATEFP10) %>% filter(pct_water == max(pct_water)) %>% select(STATEFP10, NAME10, pct_water)
#1.3
counties_perstate <- d.counties %>% group_by(STATEFP10) %>% summarise(num_counties= n())
#1.4
shortest_name<- d.stations %>% mutate(short_name = nchar (STATION_NA)) %>% filter(short_name== min(short_name))
#2.1
d.counties %>% ggplot(.,aes(x= ALAND10, y= AWATER10))+ geom_point(aes(color = STATEFP10))+labs(title = "Land Area vs Water Area by County",x = "Land Area",y = "Water Area",color = "State")
#2.2
d.stations %>% ggplot(.,aes(x=Drainage_A)+ geom_histogram() + labs(title = "Stations by drainage area", x= "Area", Y= "Number of Stations")
#2.3
stations_with_state <- st_join(d.stations, d.counties)
ggplot(stations_with_state,aes(x=Drainage_A, fill=(STATEFP10)))+ geom_histogram() + labs(title = "Stations by drainage area", x= "Area", Y= "Number of Stations")



#3

f.lab<- function(mydata) {
  if (is.numeric(mydata)) {
    cat("mean", mean(mydata))
    cat("median", median(mydata))
    cat("max", max(mydata))
    cat("min", min(mydata))
    cat("sorted", sort(mydata))
  } else{
    print("Error not numeric")}
}

#4.1
stations_and_state<- st_join(d.stations,d.counties)
stations_per_state<- stations_and_state %>% group_by(STATEFP10) %>% summarise(number_stations= n())

#4.2

ny_counties<- d.counties %>% filter (STATEFP10== 36)
ny_counties_avg<-ny_counties %>% summarise (avg_size= mean(ALAND10))

#4.3

average_drain<-st_join(d.stations,d.counties) %>% group_by(STATEFP10)%>% summarise(avg_drain = mean(Drainage_A)) %>% filter(avg_drain == max(avg_drain))
