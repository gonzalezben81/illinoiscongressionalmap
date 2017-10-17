library(rgdal)
library(leaflet)
library(reshape)
library(reshape2)
library(dplyr)




# Define UI for application that draws a histogram
ui <- fluidPage(leafletOutput("illinois",width = "auto", height = "625px")
                ,title = "Illinois Congressional Map")
