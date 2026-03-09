library(tidyverse)
library(sf)
library(tmap)

bmps <- read_csv("../data/CBW/BMPreport2016_landbmps.csv")

glimpse(bmps)

bmps %>% group_by(StateAbbreviation) %>% summarise(min = min(Cost,na.rm= TRUE),mean = mean(Cost,na.rm= TRUE),median = median(Cost,na.rm= TRUE), max = max(Cost,na.rm= TRUE),stan_div = sd(Cost,na.rm= TRUE))

bmps %>% dplyr::filter(Unit== "Acres Treated" )%>% mutate(Cost_log = log(Cost + 1),TotalAmountCredited_log = log(TotalAmountCredited + 1))%>%ggplot(aes(x= TotalAmountCredited, y= Cost))+geom_point()+labs(title = "Cost vs. TotalAmountCredited", x = "TotalAmountCredited", y = "Cost")



dams %>% filter(YEAR !=0) %>% ggplot(aes(x= YEAR, y=STATE))+ geom_point()+labs(x= "Year", y="State")

longstreams<- streams %>% slice_max(order_by=LengthKM,n=5 )



count_stream <-st_intersection(counties, streams)

