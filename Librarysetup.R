# Library setup
# Tina Biggs
# 22/04/2024


list.of.packages <- c("lubridate","tidyverse","RCurl","httr","httr2","readr","stringi","ggplot2",
                      "jsonlite","geojsonsf","geojsonio","rsconnect",
                      "data.table","shiny","sf", "spData","lattice",
                      "leafpop","leaflet","units","plyr","dplyr","stringr","htmltools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){install.packages(new.packages)}

library(plyr)
library(base)
library(rsconnect)
library(jsonlite)
library(geojsonsf)
library(geojsonio)
library(lubridate)
library(tidyverse)
library(RCurl)
library(httr)
library(httr2)
library(readr)
library(stringi)
library(ggplot2)
library(data.table)
library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(sf)
library(spData)
library(leaflet)
library(units)
library(stringr)
library(htmltools)
library(lattice)
library(leafpop)
library(openair)
library(stats)
library(dplyr)