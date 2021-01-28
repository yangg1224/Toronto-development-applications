####Preamble####
# Purpose: Using opendatatoronto to get data and draw the graph and table
# Author: Yang Wu
# Contact: Yangg.wu@mail.utoronto.ca
# Date: 2021/1/29
# To Do:
#load data from opendatatoronto ☑️
#create a folder with sub-folder on github☑️
#discuss the source of the data and bias of the data
#




#### Workspace set-up ####
# Libraries
library(opendatatoronto)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)


# Get the data
# Based on https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-01/readme.md
all_data <- 
  opendatatoronto::search_packages("Development Applications") %>% 
  opendatatoronto::list_package_resources()%>% 
  dplyr::filter(name %in% "Development Applications Data") %>% 
  group_split(name) %>% # Don't totally get this
  map_dfr(get_resource, .id = "file")

write_csv(all_data, "inputs/data/raw_data.csv")

# Have a quick look at the data
all_data<-read.csv("inputs/data/raw_data.csv")
head(all_data)


#### Data cleaning and prep ####
# the data set has many useless columns for next step analysis, i will drop them first
# janitor package is used to clean the column name
# separate the Date into year, month and day for the further analysis 
development_applications<-
  all_data %>% 
  janitor::clean_names() %>% # It handles problematic variable names and Make the column names easier to type
  # link here:https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html#clean-data.frame-names-with-clean_names
  select(date_submitted, application_type, hearing_date, street_name, postal, description, status) %>%
  separate(date_submitted, into =c("year","month","day"), sep="-", remove = FALSE)


#### Analysis ####
# firstly, remove the rows with missing value to improve the accuracy
# Interested in total numbers of Accepted applications in different types in a specific year.
# Different types applications are interpreted as following:
# CD - Condominium
# OZ - Official Plan/Rezoning
# PL - Part Lot
# SA - Site Plan Application
# SB - Sub Division
# CO - Consent
# MV - Minor Variance
# 
# Based on: https://github.com/llendway/tidy_tuesday_in_thirty/blob/main/2020_12_01_tidy_tuesday.Rmd

accepted_applications <- 
  development_applications %>% 
  tidyr::drop_na(status) %>% # We only want rows that have data for status
  filter(status %in% c("Accepted", "Approved", "Council Approved","OMB Approved")) %>%# filter the applications only in accepted status
  group_by(application_type) %>% # We want to know the total number of applications by type
  count(application_type) %>%
  rename(Number = n) 


p<-ggplot(accepted_applications,aes(application_type, Number, fill=application_type))+
  geom_bar(stat ="identity" )+
  ylab("numbers")+
  ggtitle("Total approved application (2005-2021)")+
  coord_flip()+
  theme_gray()

p

# table 
# interested in what are the all the applications in a specific street 


ADELAIDE_street_applications<-
  development_applications %>%
  tidyr::drop_na(hearing_date,status) %>% # We only want rows that have data for status
  filter(street_name %in% "ADELAIDE") 



ADELAIDE_street_applications %>% 
  filter(year %in% c("2019","2020","2021")) %>%
  select(-year,-month,-day) %>%
  kableExtra::kbl(caption = "ALL applications in ADELAIDE Street (2019-2021)") %>%
  kableExtra::kable_styling()


