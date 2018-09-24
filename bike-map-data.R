#import libraries
#1st library - leaflet for interactive map
library(leaflet)
#2nd library - data analysis to work with data
library(tidyverse)
#3rd library - to produce map as webpage
library(shiny)
#4th library - to get file from online source
library(data.table)
#5 library - to filter on columns
library(stringr)

#get file
file <- "https://www.newcastle.gov.uk/newcastle-cycle-parking-kml-1"
#read file
x <- fread(file, header=FALSE, fill=TRUE)

#create dateframes of each category want to capture
df_coords <- filter(x, str_detect(V1, "<Point>"))
df_cond <- filter(x, str_detect(V2, 'name="Condition">'))
df_spaces <- filter(x, str_detect(V2, 'name="Number_Spa">'))

#set loop rows to keep same
drows <- nrow(df_coords)

#create matrix to hold coordinates
kml.coordinates <- matrix(0,drows,3,dimnames=list(c(),c("LON","LAT","ELEV")))  
kml.conditions <- matrix(0,drows,1, dimnames =list(c(), c("CONDITIONS")))  
kml.spaces <- matrix(0,drows,1, dimnames = list(c(), c("SPACES")))

#for loop to loop through matching instances of search string to get coordinates
for(i in 1:drows){  
  
  #remove any spaces
  temp1 <- gsub(" ","",df_coords[i,1])
  #remove known first part of return string
  temp2 <- gsub("<Point><altitudeMode>relativeToGround</altitudeMode><coordinates>", "", temp1)
  #remove know part of end of string
  temp3 <- gsub("</coordinates></Point>","",temp2) 
  #split out string by "," seperator to get individual coordinates and convert to number
  coordinates <- as.numeric(unlist(strsplit(temp3,",")))
  
  #get number of bike spaces
  #remove any spaces
  temp1 <- gsub(" ","",df_spaces[i,2])
  #remove known first part of return string
  temp2 <- gsub("name=\"Number_Spa\">", "", temp1)
  #remove know part of end of string
  spaces <- gsub("</SimpleData>","",temp2)
  
  #get condition of bike space
  #remove any spaces
  temp1 <- gsub(" ","",df_cond[i,2])
  #remove known first part of return string
  temp2 <- gsub("name=\"Condition\">", "", temp1)
  #remove know part of end of string
  condition <- gsub("</SimpleData>","",temp2) 
  
  #add coordinates to matrix
  kml.coordinates[i,] <- matrix(c(coordinates),ncol=3)  
  kml.conditions[i,] <- matrix(c(condition),ncol=1)  
  kml.spaces[i,] <- matrix(c(spaces),ncol=1)  
}

#remove NA's from data
kml.conditions[is.na(kml.conditions)] <- "UNKNOWN"
kml.spaces[is.na(kml.spaces)] <- "0"
kml.spaces[kml.spaces == "N/A"] <- "0"
#convert spaces to number
kml.spaces <- as.numeric(kml.spaces)
df_spaces <- data.frame(kml.spaces)
names(df_spaces) <- "SPACES"
#colnames(kml.spaces) <- "SPACES"

#convert matrices to dataframe for map
df_map <- bind_cols(data.frame(kml.coordinates), data.frame(kml.conditions), df_spaces)
#add "ALL" to top of data set
df_cond <- data.frame(c("ALL"))
names(df_cond) <- "CONDITIONS"
df_cond2 <- data.frame(levels(droplevels(df_map$CONDITIONS)))
names(df_cond2) <- "CONDITIONS"
df_cond <- bind_rows(df_cond, df_cond2)


#function to set the colours of the marker on the map
getColor <- function(conditions) {
  sapply(conditions$CONDITIONS, function(condition) {
    
    if(as.character(condition) == "Good") {
      "green"
    } else if(as.character(condition) == "Damaged") {
      "orange"
    } else {
      "red"
    }
    })
}

#build the marker information - to get different colours and icons
icons <- awesomeIcons(
  icon = 'bicycle',
  iconColor = 'white',
  library = 'fa',
  markerColor = getColor(df_map)
)

#create map using leafet library and shiny
ui <- bootstrapPage(
  #set map to 100% width and height
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  #create side pannel to hold elements
  absolutePanel(top = 10, right = 10,
                h2("Bike Racks in Newcastle"),
                sliderInput("range", "Spaces", min(df_map$SPACES), max(df_map$SPACES),
                            value = range(df_map$SPACES), step = 0.1
                ),
                selectInput("condition", "Conditions", df_cond, selected = "ALL", multiple = FALSE
                )
  )
)

server <- function(input, output, session) {
  
  #create filtered data base on selections from panel
  filtered_data <- reactive({
    
    if (input$condition == "ALL") {
      df_map[df_map$SPACES >= input$range[1] & df_map$SPACES <= input$range[2],]
    } else {
      df_map[df_map$CONDITIONS == input$condition & df_map$SPACES >= input$range[1] & df_map$SPACES <= input$range[2],]
    }
    
  })
  
  #create initial map
  output$mymap <- renderLeaflet({
    leaflet(data = df_map) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(icon=icons, lng=~LON, lat=~LAT, popup=~CONDITIONS, label = ~as.character(SPACES))
  })
  
  #create observation for change of inputs and re-populate map with filtered data
  observe({
    
    #re-class icons with filtered data
    icons <- awesomeIcons(
      icon = 'bicycle',
      iconColor = 'white',
      library = 'fa',
      markerColor = getColor(filtered_data())
    )
    
    #re-populate map
    leafletProxy("mymap", data = filtered_data()) %>%
                   clearMarkers() %>%
                   addAwesomeMarkers(icon=icons, lng=~LON, lat=~LAT, popup=~CONDITIONS, label = ~as.character(SPACES))
  })
  
  
}


#clean workspace
#rm(list = ls())

shinyApp(ui, server)

