# FinalExam
Final Exam 
# Clear the Enviornment 
rm(list=ls(all = TRUE))

# Loading All Libraries to be safe 
library(dplyr)
library(doBy)
library(rio)
library(stargazer)
library(WDI)
library(lubridate)
library(tidyverse)
library(labelled)
library(googlesheets4)
library(data.table)
library(varhandle)
library(ggrepel)
library(ggplot2)
library(geosphere)
library(rgeos)
library(sp)
library(viridis)
library(mapview)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires) 
devtools::install_github("ropensci/rnaturalearthhires") 
devtools::install_github("yutannihilation/ggsflabel")

# Load WDI Package 
library(WDI)


#Adding Data from the WDI of Female Labor Force Participation 

female_lfp <- WDI(country = "all",
                  indicator = c("SL.TLF.TOTL.FE.ZS"), 
                  start = 2010, end = 2015,
                  extra = FALSE, cache = NULL)

#Renaming female_lfp to flfp
library(data.table) 
setnames(female_lfp,"SL.TLF.TOTL.FE.ZS", "flfp")


#Getting the summary of female_lfp
summary(female_lfp)

#Checking to see if it was renamed 
head(female_lfp)

#Figuring out which variables are in the dataset 
female_lfp$iso2c
female_lfp$country
female_lfp$flfp
female_lfp$year


#Getting the mean of flfp 
mean(female_lfp$flfp, na.rm = TRUE)
mean(female_lfp$year, na.rm = TRUE)

# Collapsing the Data 
collapsed_flfp <-
  female_lfp %>%
  group_by(country_code, country, flfp_mean) %>%

#Showing the countries that have average flfp rates for 2010-2015
  # that are less than 15% 
????
  
  
collapsed_flfp = collapsed_flfp %>%
group_by(flfp) %>%
summarize(log_wb_val = sum(log_wb_val, na.rm=TRUE))
#Mapping the collapsed data. 

#Omitting NA values 
female_lfp2 <-na.omit(subset(female_lfp,
                               select=c("country","flfp", "year", "iso2c")))
#Getting world map 
final <- transform(female_lfp2,
                  female_lfp = factor(replace(as.character(female_lfp2),
                                            list = !female_lfp2 %in% c("Africa Eastern And Southern""year", "Africa Eastern And Southern"),
                                            values = "other"))

world <- ne_countries(scale = "large", returnclass = "sf")

#Plotting Data frame 

female_map = ggplot() + 
  geom_sf(data = collapsed_flfp, aes(fill=`Log Value`)) +
  scale_fill_viridis(option = "viridis") +
  ggtitle("Female Labor Force Participation Rates, 2010-2015")+ theme(plot.title = element_text(hjust = 0.5))+
  theme_void()

print(female_map)
gsave(female_map, filename = "female_map.png", width = 6.5, height = 6)


#7) South Asia
#9) User Interface - Input (unique ID), Output (outputID), 
#server (store direction/render/refer output object to input object )

#String as factors 
female_lfp <- stread("/Users/briannalebron/Desktop/GOV355/Shape Files", stringsAsFactors = FALSE)

#Pulling Mikes .pdf file 

input <- paste0("https://pdf.usaid.gov/pdf_docs/PA00TNMJ.pdf", type, "/", date) chart_page <- xml2::read_pdf(input)

armeniatext <- "https://pdf.usaid.gov/pdf_docs/PA00TNMJ.pdf", date stringsAsFactors = FALSE)

library(tidytext)
armeniatext %>% unnest_tokens(word, text)

#install.packages("rvest") 
#Load package dplyr & rvest
library(dplyr)
library(rvest)
hot100exam <- "https://www.billboard.com/charts/hot-100" 
hot100 <- read_html(hot100exam)
hot100
str(hot100)
head(hot100exam)

body_nodes <- hot100 %>% html_node("body") %>%
  rank <- hot100 %>%
  rvest::html_nodes('body') %>% xml2::xml_find_all("//span[contains(@class,
'chart-element__rank__number')]") %>%
  rvest::html_text()
artist <- hot100 %>% rvest::html_nodes('body') %>% xml2::xml_find_all("//span[contains(@class,
'chart-element__information__artist')]") %>%
  rvest::html_text()
title <- hot100 %>% rvest::html_nodes('body') %>% xml2::xml_find_all("//span[contains(@class,
'chart-element__information__song')]") %>%
  rvest::html_text()
last_week <-  rvest::html_nodes('body') %>% xml2::xml_find_all("//span[contains(@class,
'chart-element__rank__date')]") %>%
  rvest::html_text()
