#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(countrycode)
library(plotly)
library(shiny)
library(reshape2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(stringr)
library(shinythemes)
library(shinydashboard)

library(readr)
origdata <- read_csv("laurel-world-happiness-report-data/data/data_behind_table_2_1_whr_2017.csv")


happiness <- origdata %>%
  mutate(confidence_in_gov = confidence_in_national_government, 
         Gini_Income = gini_of_household_income_reported_in_gallup_by_wp5_year, 
         Gini_Average = gini_index_world_bank_estimate_average_2000_13,
         continent = countrycode(sourcevar = country, origin = "country.name",destination = "continent"),
         region = countrycode(sourcevar = country, origin = "country.name", destination = "region"),
         continent = as.factor(ifelse(country == "Kosovo", "Europe", as.character(continent)))) %>%
  select(country:perceptions_of_corruption, Gini_Income, Gini_Average, confidence_in_gov, continent, region)




# ggplot(twenty16, aes(x = reorder(continent, -life_ladder), life_ladder, fill = continent)) + stat_summary(fun.y = "mean", geom = "bar")

# ggplot(twenty16, aes(x = reorder(country, -life_ladder), y = life_ladder, fill = continent)) + geom_bar(stat = "identity")



