###Load necessary libraries
# library(rgdal) ###rgdal is now defunct and no longer available
library(leaflet)
library(reshape)
library(reshape2)
library(dplyr)
library(raster)
library(terra)
library(sf)


## Illinois Congressional Districts
# houseillower <- readOGR(dsn = "./data",layer = "tl_2016_17_sldl", verbose = FALSE)
# houseillower <- sf::st_read("./data",layer = "tl_2016_17_sldl")
# houseilupper <- readOGR(dsn = "./data",layer = "tl_2016_17_sldu", verbose = FALSE)
# houseilupper <- sf::st_read("./data", layer ="tl_2016_17_sldu")

###Updated to tranform to the following specifications due to what seems
###to be a glitch in how the shape file is read in. This is done for both
###lower and upper house
houseillower <- sf::read_sf("./data/tl_2016_17_sldl.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

houseilupper <- sf::read_sf('./data/tl_2016_17_sldu.shp') %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

##Creates dataframe of SpatialPolygonData
houseillowerframe <- as.data.frame(houseillower)
houseilupperframe <- as.data.frame(houseilupper)

##Reads in CSV
illinois_senate <- read.csv("./data/senate_info_2025.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
illinois_house <- read.csv("./data/house_info_2025.csv", stringsAsFactors = FALSE, encoding = "UTF-8")


# Clean all character columns using iconv to remove invalid characters
clean_columns <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      iconv(x, from = "UTF-8", to = "UTF-8", sub = "byte")
    } else {
      x
    }
  })
  return(df)
}

# Apply the cleaning function to both data frames
illinois_senate <- clean_columns(illinois_senate)
illinois_house <- clean_columns(illinois_house)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ##Rename variables to match data frame for coercion
  illinois_senate <- rename(illinois_senate, SLDUST = District)

  ##Rename variables to match data frame for coercion
  illinois_house <- rename(illinois_house,SLDLST = District)

  ##Can use cbind or left_join to bind the new data to the SpatialPolygonDataFrames
  houseilupper<- cbind(houseilupper, illinois_senate)

  houseillower <- cbind(houseillower,illinois_house)


  ##Subsets the data to be used in a map and create grouping variables for overlays ####
  housedemocrats<- subset(houseillower, houseillower$Party %in% c("D"))

  houserepublicans <- subset(houseillower, houseillower$Party %in% c("R"))

  senatedemocrats<- subset(houseilupper, houseilupper$Party %in% c("D"))

  senaterepublicans <- subset(houseilupper, houseilupper$Party %in% c("R"))

  ##Creates the popups for the datasets

  senatedemo_popup <- paste0("<strong>Senator: </strong>",
                             senatedemocrats$Senator,
                             "<br><strong>Party: </strong>",
                             senatedemocrats$Party,
                             "<br><strong>District: </strong>",
                             senatedemocrats$SLDUST)

  senaterepub_popup <- paste0("<strong>Senator: </strong>",
                              senaterepublicans$Senator,
                              "<br><strong>Party: </strong>",
                              senaterepublicans$Party,
                              "<br><strong>District: </strong>",
                              senaterepublicans$SLDUST)

  housedemo_popup <- paste0("<strong>State Representative: </strong>",
                            housedemocrats$Representative,
                            "<br><strong>Party: </strong>",
                            housedemocrats$Party,
                            "<br><strong>District: </strong>",
                            housedemocrats$SLDLST)

  houserepub_popup <- paste0("<strong>State Representative: </strong>",
                             houserepublicans$Representative,
                             "<br><strong>Party: </strong>",
                             houserepublicans$Party,
                             "<br><strong>District: </strong>",
                             houserepublicans$SLDLST)

  illinoismap<- leaflet(data = houseilupper) %>%
    addTiles() %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addPolygons(data = senatedemocrats,color = "blue",popup = senatedemo_popup,group = "Senate Democrat") %>%
    addPolygons(data = senaterepublicans,color = "red",popup = senaterepub_popup,group = "Senate Republican") %>%
    addPolygons(data = housedemocrats,color = "blue",popup = housedemo_popup,group = "House Democrat") %>%
    addPolygons(data = houserepublicans,color = "red",popup = houserepub_popup,group = "House Republican") %>%
    # addPolygons(data = countyil,fillColor = ~pal3(TOT_POP),
    #             fillOpacity = 0.8,
    #             color = "#BDBDC3",
    #             weight = 1,
    #             popup = county_popup,group = "County") %>%
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap"
      ),
      overlayGroups = c("Senate Democrat","Senate Republican","House Democrat","House Republican"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addMiniMap(toggleDisplay =  TRUE)%>%
    addMeasure()%>%
    addLegend("bottomleft",title = paste0("Illinois Congressional Map: Published ",format(Sys.time()," %b %Y")),colors = c("blue","red"),labels = c("D = Democrat","R = Republican"),values = ~TOT_POP)
  # addLegend("bottomright",values = ~TOT_POP,pal = pal6,title = "Population by County")

  output$illinois <- renderLeaflet(illinoismap)
}

