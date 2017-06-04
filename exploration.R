library(dplyr)

load("./data/oferty_2017-04-27.Rdata")


splitted_general <- filter(oferty, cena != "Zapytaj o cenę")[["generalInfo"]] %>% 
  as.character() %>% 
  strsplit(", ") 

# whitespace?
get_pokoj <- function(x) {
  res <- x[grep("pok", x)]
  if(length(res) == 0) {
    NA
  } else {
    strsplit(iconv(res, "UTF-8", "ASCII", sub = " "), " ")[[1]][1] %>% 
      as.numeric
  }
}

get_cena <- function(x) {
  res <- x[grep("zł/m2", x)]
  if(length(res) == 0) {
    NA
  } else {
    gsub(pattern = " ", replacement = "", res) %>% 
      gsub(pattern = "zł/m2", replacement = "", .) %>% 
      as.numeric
  }
}

get_metrarz <- function(x) {
  res <- x[grep(" m2", x)]
  if(length(res) == 0) {
    NA
  } else {
    gsub(pattern = " ", replacement = "", res) %>%
      gsub(pattern = "m2", replacement = "", .) %>% 
      gsub(pattern = ",", replacement = ".", ., fixed = TRUE) %>% 
      as.numeric
  }
}

get_rok <- function(x) {
  res <- x[grep("rok", x)]
  if(length(res) == 0) {
    NA
  } else {
    strsplit(iconv(res, "UTF-8", "ASCII", sub = " "), " ")[[1]][1] %>% 
      as.numeric
  }
}



tmp <- sapply(splitted_general, get_rok)

dzielnica <- filter(oferty, cena != "Zapytaj o cenę")[["nazwa"]] %>% 
  as.character() %>% 
  strsplit(", ") %>% 
  sapply(first) %>% 
  strsplit("Wroc[lł]aw") %>% 
  sapply(last) %>% 
  data.frame(old_name = ., stringsAsFactors = FALSE) %>% 
  left_join(read.csv(file = "./data/dzielnice_nazwy.csv", 
                     stringsAsFactors = FALSE,
                     encoding = "UTF-8")) %>% 
  select(new_name) %>% 
  rename(dzielnica = new_name)

dat <- data.frame(n_pokoj = sapply(splitted_general, get_pokoj),
                  metrarz = sapply(splitted_general, get_metrarz),
                  cena_m2 = sapply(splitted_general, get_cena),
                  rok = sapply(splitted_general, get_rok)) %>% 
  mutate(rok = ifelse(rok < 1500, NA, rok)) %>% # dwa rekordy rok 1 i 6
  cbind(dzielnica)  
  
write.csv(dat, file = "./data/mieszkania_dane.csv", fileEncoding = "UTF-8", row.names = FALSE)
