library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)

h <- read_html("https://www.shmu.sk/sk/?page=1&id=klimat_operativneudaje1&identif=11930&rok=2020&obdobie=1991-2020&sub=1")
scripts <- h %>% html_nodes("script") %>% html_text(trim=TRUE) 
json_long <- scripts[grepl('var wn_longterm_serie = ',scripts)]

dum <- "dfdff.\\..sdfsaf var wn_longterm_serie= asdad....,, data: [[5,4],[4,6]]}; ghsahdAGDS, var wn_current_serie= asdad....,, data: [[5,4],[4,6]]};"

gsub("\\]\\]\\};.*","",dum)

gsub(".*data:","",dum)

str_count(dum,".*data:")

long_temp <- fromJSON(gsub(".*data:","", gsub("\\};.*","", gsub(".*wn\\_longterm\\_serie","",json_long)))) %>%
  as.data.frame() 
colnames(long_temp) <- c("rm_date","longterm_temp")
long_temp <- long_temp %>% select(longterm_temp)

date2020 <- seq(as.Date('2020/01/01'), by = 'day', length.out = 366)

df <- cbind(date2020,long_temp) %>% filter(date2020 != '2020/2/29')

ggplot(df,aes(x=date2020,y=longterm_temp)) + geom_line()
