library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)

# scrape non-leap years separately due to different length
df <- data.frame(nrow=rep(NA,365))
years <- seq(2009,2022,1)[!seq(2009,2022,1) %in% c(2012,2016,2020)]
for (k in 1:length(years)) {
    i <- years[k]
    h <- read_html(paste0("https://www.shmu.sk/sk/?page=1&id=klimat_operativneudaje1&identif=11968&rok=",i,"&obdobie=1981-2010&sub=1"))
    scripts <- h %>% html_nodes("script") %>% html_text(trim=TRUE) 
    json_current <- scripts[grepl('var wn_current_serie = ',scripts)]
    assign(paste0("date",i),value=seq(as.Date(paste0(i,"/01/01")),as.Date(paste0(i,"/12/31")),by="day")) 
    current_temp <- fromJSON(gsub(".*data:","", gsub("\\};.*","", gsub(".*wn\\_current\\_serie","",json_current)))) %>%
      as.data.frame() 
    colnames(current_temp) <- c("rm_date","current_temp")
    current_temp <- current_temp %>% select(current_temp)
    longterm_temp <- fromJSON(gsub(".*data:","", gsub("\\};.*","", gsub(".*wn\\_longterm\\_serie","",json_current)))) %>%
      as.data.frame() 
    colnames(longterm_temp) <- c("rm_date","longterm_temp")
    longterm_temp <- longterm_temp %>% select(longterm_temp)
    # attach to the dataframe
    df[,3*k-2] <-assign(paste0("date",i),value=seq(as.Date(paste0(i,"/01/01")),as.Date(paste0(i,"/12/31")),by="day")) 
    df[,3*k-1] <- current_temp
    df[,3*k] <- longterm_temp
    colnames(df)[3*k-2] <- paste0("date",i)
    colnames(df)[3*k-1] <- paste0("temp",i)
    colnames(df)[3*k] <- paste0("longterm_temp",i)
}

# scrape leap years
df_366 <- data.frame(nrow=rep(NA,366))
leap_years <- c(2012,2016,2020)
for (k in 1:length(leap_years)) {
  i <- leap_years[k]
  h <- read_html(paste0("https://www.shmu.sk/sk/?page=1&id=klimat_operativneudaje1&identif=11968&rok=",i,"&obdobie=1981-2010&sub=1"))
  scripts <- h %>% html_nodes("script") %>% html_text(trim=TRUE) 
  json_current <- scripts[grepl('var wn_current_serie = ',scripts)]
  assign(paste0("date",i),value=seq(as.Date(paste0(i,"/01/01")),as.Date(paste0(i,"/12/31")),by="day")) 
  current_temp <- fromJSON(gsub(".*data:","", gsub("\\};.*","", gsub(".*wn\\_current\\_serie","",json_current)))) %>%
    as.data.frame() 
  colnames(current_temp) <- c("rm_date","current_temp")
  current_temp <- current_temp %>% select(current_temp)
  longterm_temp <- fromJSON(gsub(".*data:","", gsub("\\};.*","", gsub(".*wn\\_longterm\\_serie","",json_current)))) %>%
    as.data.frame() 
  colnames(longterm_temp) <- c("rm_date","longterm_temp")
  longterm_temp <- longterm_temp %>% select(longterm_temp)
  # attach to the dataframe
  df_366[,3*k-2] <-assign(paste0("date",i),value=seq(as.Date(paste0(i,"/01/01")),as.Date(paste0(i,"/12/31")),by="day")) 
  df_366[,3*k-1] <- current_temp
  df_366[,3*k] <- longterm_temp
  colnames(df_366)[3*k-2] <- paste0("date",i)
  colnames(df_366)[3*k-1] <- paste0("temp",i)
  colnames(df_366)[3*k] <- paste0("longterm_temp",i)
}

# plot daily temperature for 2019
ggplot(df,aes(x=date2019,y=temp2019)) + geom_line() + theme_minimal() + xlab("")
