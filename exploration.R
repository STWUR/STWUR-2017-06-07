library(dplyr)

load("./data/oferty_2017-04-27.Rdata")


splitted_general <- filter(oferty, cena != "Zapytaj o cenę")[["generalInfo"]] %>% 
  as.character() %>% 
  strsplit(", ") 

splitted_general[lengths(splitted_general) == 4] %>% 
  do.call(rbind, .)

splitted_general[lengths(splitted_general) == 5] %>% 
  do.call(rbind, .)
