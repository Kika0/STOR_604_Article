library(tidyverse)
library(stringr)
library(rvest)

h <- read_html("https://www.shmu.sk/sk/?page=1&id=klimat_operativneudaje1&identif=11930&rok=2021&obdobie=1991-2020&sub=1")
h[1]
scripts <- h %>% html_nodes("script") %>% html_text(trim=TRUE) 

new <- scripts[grepl('var wn_longterm_serie =|var wn_current_serie = ',scripts)]


json_long <- scripts[grepl('var wn_longterm_serie = ',scripts)]
h %>% class()

library(jsonlite)
fromJSON(json_long)

str_sub(json_long,,-1)

str_extract(json_long,"data:$")

str_sub( gsub(".*data:", "",json_long),1,200 )

str_sub(json_long,1,800)

long <- fromJSON(str_sub(gsub("\\]\\];.*","",gsub("[^data\\:]+","",gsub(".*wn\\_longterm\\_serie","",json_long))),1,-3)) %>% as.data.frame()

ggplot(long,aes(x=V1,y=V2)) + geom_line()

long %>% slice_head(n=20)
