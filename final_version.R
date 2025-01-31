library(shiny)
library(shinythemes)
library(readxl)
library(leaflet)
library(dplyr) # used to filter data and reorder data
library(slickR)
library(shinyjs)
library(htmltools)
library(stringr)
library(maps)
library(ggplot2)
library(GGally) # used to draw the correlationship graph
library(plotly) # used to make interactive plot
library(scales)
library(shinydashboard)
library(shinyWidgets)
library(fmsb)
library(bslib)
library(DT)
library(readr)
library(sf)
library(magick)
library(shinyjqui)
library(ggiraph)
library(httr)
library(tidyr)
library(geosphere)


# source tableau files
source("tableau-in-shiny-v1.2.R")

################## map_data ####################
#map data
mapboxToken <- "pk.eyJ1IjoiZ3R5MDIyMSIsImEiOiJjbG56aW1uOWcwZnpkMnByb3h6cnk2MjhpIn0.AMNKnw_YVEuESinpbTHQNA"
mapboxStyleURL <- paste0("https://api.mapbox.com/styles/v1/gty0221/clo01nsex004k01mv9yv6bqv9/tiles/256/{z}/{x}/{y}?access_token=", mapboxToken, "&cacheBuster=", Sys.time())

bar_data <- read_csv("data/bar.csv")
museum_data <- read_csv("data/museum.csv")
restaurant_data <- read_csv("data/food.csv")
park_data <- read_csv("data/park.csv")
zoo_data <- read_csv("data/zoo.csv")
school_data <- read_csv("data/school.csv")
hotel_datas <- read_csv("data/hotel.csv")
shopping_data <- read_csv("data/shopping.csv")
station_data <- read_csv("data/station.csv")
toilets_data <- read_csv("data/toilets.csv")
airport_data <- read_csv("data/airport.csv")
shapefile_path <- "data/PTV/PTV_METRO_TRAM_STOP.shp"
tram_stops <- st_read(shapefile_path)
tram_stops <- tram_stops %>% separate_rows(ROUTEUSSP, sep = ",")

data <- read.csv("Data_qfl/18mustgo.csv")
data2 <- read.csv("Data_qfl/cleaned_Attractions_data.csv")
reviews <- read.csv("Data_qfl/Review.csv")
entertainment_data <- read.csv("Data_qfl/Entertainment.csv")

api_key <- "b7e76b7e1472053c54770df2735c7d16"

# List the Free Tram Zone stations and abbreviations
free_tram_stop_keywords <- c(
  "Peel Street", "Queen Victoria Market", "Melbourne Central", "Spring Street", 
  "Collins Street", "Federation Square", "Flinders Street", "Batman Park",
  "Parliament Station", "Nicholson Street", "Russell Street"
)
# Filter out sites containing any keywords
free_tram_zone_stops <- tram_stops %>%
  filter(str_detect(STOP_NAME, paste(free_tram_stop_keywords, collapse = "|")))



all_data <- rbind(
  data.frame(Type = "bar", Name = bar_data$Name, Longitude = bar_data$Longitude, Latitude = bar_data$Latitude, About = bar_data$About, Price = bar_data$Price, Website = bar_data$Website, OPentime = bar_data$`Opening Hours`, Image = bar_data$`Featured Image`, Icon = "project_image/bar.png"),
  data.frame(Type = "museum", Name = museum_data$Name, Longitude = museum_data$Longitude, Latitude = museum_data$Latitude, About = museum_data$About, Price = museum_data$Price, Website = museum_data$Website, OPentime = museum_data$`Opening Hours`, Image = museum_data$`Featured Image`, Icon = "project_image/museum.png"),
  data.frame(Type = "restaurant", Name = restaurant_data$Name, Longitude = restaurant_data$Longitude, Latitude = restaurant_data$Latitude, About = restaurant_data$About, Price = restaurant_data$Price, Website = restaurant_data$Website, OPentime = restaurant_data$Opening.Hours, Image = restaurant_data$Featured.Image, Icon = "project_image/restaurant.png"),
  data.frame(Type = "park", Name = park_data$Name, Longitude = park_data$Longitude, Latitude = park_data$Latitude, About = park_data$About, Price = park_data$Price, Website = park_data$Website, OPentime = park_data$`Opening Hours`, Image = park_data$`Featured Image`, Icon = "project_image/park.png"),
  data.frame(Type = "zoo", Name = zoo_data$Name, Longitude = zoo_data$Longitude, Latitude = zoo_data$Latitude, About = zoo_data$About, Price = zoo_data$Price, Website = zoo_data$Website, OPentime = zoo_data$`Opening Hours`, Image = zoo_data$`Featured Image`, Icon = "project_image/zoo.png"),
  data.frame(Type = "school", Name = school_data$Name, Longitude = school_data$Longitude, Latitude = school_data$Latitude, About = school_data$About, Price = school_data$Price, Website = school_data$Website, OPentime = school_data$`Opening Hours`, Image = school_data$`Featured Image`, Icon = "project_image/school.png"),
  data.frame(Type = "hotel", Name = hotel_datas$Name, Longitude = hotel_datas$Longitude, Latitude = hotel_datas$Latitude, About = hotel_datas$About, Price = hotel_datas$Price, Website = hotel_datas$Website, OPentime = hotel_datas$`Opening Hours`, Image = hotel_datas$`Featured Image`, Icon = "project_image/hotel.png"),
  data.frame(Type = "shopping", Name = shopping_data$Name, Longitude = shopping_data$Longitude, Latitude = shopping_data$Latitude, About = shopping_data$About, Price = shopping_data$Price, Website = shopping_data$Website, OPentime = shopping_data$`Opening Hours`, Image = shopping_data$`Featured Image`, Icon = "project_image/shopping.png"),
  data.frame(Type = "station", Name = station_data$Name, Longitude = station_data$Longitude, Latitude = station_data$Latitude, About = station_data$About, Price = station_data$Price, Website = station_data$Website, OPentime = station_data$`Opening Hours`, Image = station_data$`Featured Image`, Icon = "project_image/station.png"),
  data.frame(Type = "toilets",Name = toilets_data$Name, Longitude = toilets_data$Longitude, Latitude = toilets_data$Latitude, About = toilets_data$About, Price = toilets_data$Price, Website = toilets_data$Website, OPentime = toilets_data$`Opening Hours`, Image = toilets_data$`Featured Image`, Icon = "project_image/toilets.png"),
  data.frame(Type = "airport", Name = airport_data$Name, Longitude = airport_data$Longitude, Latitude = airport_data$Latitude, About = airport_data$About, Price = airport_data$Price, Website = airport_data$Website, OPentime = airport_data$`Opening Hours`, Image = airport_data$`Featured Image`, Icon = "project_image/airport.png")
)
########### load hotel data ##########
hotel_path <- "./data/cleaned_hotel_data.xlsx"
hotel_data <- read_excel(hotel_path, sheet = 1)
hotel_data$Parsed_Hotel_Class <- as.numeric(gsub(" stars", "", hotel_data$`Hotel Class`))
unique_amenities <- unique(unlist(strsplit(hotel_data$Amenities, ",")))
unique_amenities <- trimws(unique_amenities)  # remove empty spaces
min_price <- min(hotel_data$Price, na.rm = TRUE)
max_price <- max(hotel_data$Price, na.rm = TRUE)
hotel_data$Latitude <- as.numeric(hotel_data$Latitude)
hotel_data$Longitude <- as.numeric(hotel_data$Longitude)
hotel_data <- hotel_data %>%
  distinct(Name, .keep_all = TRUE)

# customized icon for hotel
custom_icon <- makeIcon(
  iconUrl = "./www/hotel_icon.png",  # image file path
  iconWidth = 25,  # height
  iconHeight = 25,  # width
  iconAnchorX = 12, 
  iconAnchorY = 25  
)

# image files related to top 5 hotels
image_files <- c("hotel_top1.png", "hotel_top2.png", "hotel_top3.png", "hotel_top4.png", "hotel_top5.png")
# display information for hotel selection list
generateHotelSummary <- function(name, type, rating, address, website, price, image_url, hotel_id, review_count, average_rating,amenities) {
  display_website <- sub("^(https?://[^/]+).*", "\\1", website)
  
  actionLink(
    inputId = paste0("hotel_", hotel_id),
    label = tagList(
      div(style = "border: 1px solid #ddd; padding: 20px; margin-bottom: 15px; display: flex; flex-direction: column; align-items: flex-start; min-height: 180px; cursor: pointer; position: relative;",
          div(style = "display: flex; align-items: flex-start; width: 100%;",
              img(src = image_url, height="150px",style = "margin-right: 20px;max-width: 200px; max-height: 150px;"),
              div(style = "flex: 1;",
                  h4(name, style = "margin: 0; font-size: 18px; font-weight: bold;"),
                  p(paste(type, "•", rating, "Stars •", address), style = "margin: 5px 0; color: #555; font-size: 14px;"),
                  p(paste("Amenities:", amenities), style = "color: #555; font-size: 13px; margin-top: 5px;"),
                  a(href = website, display_website, target = "_blank", style = "color: #337ab7; text-decoration: none; font-size: 13px;")
              ),
              div(style = "margin-left: auto; font-weight: bold; font-size: 16px;",
                  paste("From AU$", price)
              )
          ),
          # display review count and vaerage count at the right-bottom corner
          div(style = "position: absolute; bottom: 10px; right: 20px; color: #555; font-size: 14px; text-align: right;",
              p(HTML(paste("Review Count: ", strong(review_count)))),  # bold font
              p(HTML(paste("Average Rating: ", strong(average_rating))))  # bold font
          )
      )
    ),
    style = "text-decoration: none; color: inherit;"
  )
}

# display image for top5 hotel
generateHotelImage <- function(image_url) {
  div(
    style = "width: 550px; height: 450px; border: 1px solid #ddd; padding: 20px; margin: 10px; background-color: #f9f9f9; display: flex; align-items: center; justify-content: center;",
    img(src = image_url, style = "max-width: 100%; max-height: 100%; object-fit: cover; display: block;")
  )
}
# generate description page for top5 hotel
generateHotelDescription <- function(name, address, description, rating, price, review_count, phone,website,google_maps_url) {
  div(
    style = "width: 550px; height: 450px; border: 1px solid #ddd; padding: 20px; margin: 10px; background-color: #f9f9f9; display: flex; flex-direction: column; justify-content: center;",
    
    h3(name, style = "font-size: 22px; font-weight: bold; text-align: center; margin-bottom: 10px;"),
    
    p(HTML(paste("<strong>Rating:</strong>", rating, "• <strong>Price:</strong> AU$", price, "• <strong>Recent Orders:</strong>", review_count)), 
      style = "text-align: center; font-size: 15px; margin-bottom: 15px;"),
    
    p(description, style = "text-align: justify; font-size: 14px; margin-bottom: 15px;"),
    
    p(HTML(paste("<strong>Phone:</strong>", phone)), style = "text-align: center; font-size: 14px; margin-bottom: 10px;"),
    
    p(HTML(paste("<strong>Address:</strong>", address)), style = "text-align: center; font-size: 14px;"),
    p(a(href = website, "View Website", target = "_blank", 
        style = "text-decoration: none; color: #337ab7; font-weight: bold;"), 
      style = "text-align: center; font-size: 14px;"),
    p(a(href = google_maps_url, "Open in Google Maps", target = "_blank", 
        style = "text-decoration: none; color: #337ab7; font-weight: bold;"), 
      style = "text-align: center; font-size: 14px;")
  )
}

########### hotel pages ########
hotel_tab <- tabPanel(
  "Overview of Hotel",
  fluidPage(
    tags$style(HTML("
      .main-container {
        display: flex;
        justify-content: center;
        padding: 20px;
      }
      .custom-sidebar { 
        background-color: white;
        border: none;
      }
      .centered-tableau {
        margin-left: 27%;
        margin-right: 32%;
        max-height: 500px !important;
        max-width: 800px;
      }
      .centered-list {
        margin-left: 15%;
        margin-right: 15%;
      }
      .details-view {
        display: flex;
        justify-content: center; 
        padding: 20px;
      }
      .details-card {
        max-width: 600px; 
        border: 1px solid #ddd;
        padding: 20px;
        background-color: #f9f9f9;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
    ")),
    div(class= "centered-tableau",
        leafletOutput("hotelMap", height = 500),
    ),
    div(class = "centered-list",  #Add an outer container for centering
        sidebarLayout(
          sidebarPanel(
            class = "custom-sidebar",
            width = 3,
            textInput("location", "Location", placeholder = "Add location"),
            sliderInput("priceRange", "Price Range", min = min_price, max = max_price, value = c(min_price, max_price), pre = "$"),
            checkboxGroupInput("category", "Category",
                               choices = unique_amenities,
                               selected = NULL),
            checkboxGroupInput("hotel_class", "Hotel Class",
                               choices = list("★" = 1,
                                              "★★" = 2,
                                              "★★★" = 3,
                                              "★★★★" = 4,
                                              "★★★★★" = 5),
                               selected = NULL) 
          ),
          mainPanel(
            div(
              div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                # Add a button to select sorting criteria
                selectInput("sort_by", "Sort By",
                            choices = list("Price" = "price", "Rating" = "rating"),
                            selected = "price", width = "200px"),
                
                # Sort direction selector: Low to high or high to low
                selectInput("sort_direction", "Sort Direction",
                            choices = list("Low to High" = "low_to_high", "High to Low" = "high_to_low"), 
                            selected = "low_to_high", 
                            width = "200px")
              ),
              uiOutput("mainContent")
            )
          )
        )
    )
  )
)

top5_hotel <-tabPanel(
  "Top 5 Hotels",
  fluidPage(
    tags$style(HTML("
        .main-container {
          display: flex;
          justify-content: center;
          margin-left: 10%;
          margin-right: 10%;
        }
      ")),
    h2("Top 5 Hotels in Melbourne", style = "text-align: center; padding-bottom: 20px;"),
    
    # Show pictures and descriptions of the hotel left and right
    div(class = "main-container",
        fluidRow(
          column(6, uiOutput("leftColumnHotels")),
          column(6, uiOutput("rightColumnHotels"))
        )
    )
  )
) 
########### home pages #######
home_tab <- tabPanel(
  "Home",
  fluidPage(
    tags$head(
      tags$style(HTML("
        .background-image {
          background-image: url('Melbourne.jpg');
          background-size: cover;
          background-attachment: fixed;
          background-position: center;
          opacity: 0.4;
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          z-index: -1;
        }
        
        .custom-button {
          background-color: #5DADE2 !important;
          color: white !important;
          border: none;
          padding: 10px 20px;
          font-size: 16px;
          border-radius: 5px;
          cursor: pointer;
          transition: background-color 0.3s ease;
        }
        
        .custom-button:hover {
          background-color: #87CEFA !important;
        }
        
        .content-box {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          width: 700px;
        }
        
        .tableau {
          margin-top: 2%;
          margin-left: 20%;
          margin-right: 20%;
          max-width: 1000px;
          max-height:800px;
        }
        .weather-panel {
          position: absolute;
          top: 70px;
          left: 10px;
          background-color: rgba(255, 255, 255, 0.8);
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
          font-size: 14px;
          color: #333;
          max-width: 300px;
          z-index: 1000;
        }
      "))
    ),
    div(class = "background-image"),
    # Weather Panel
    div(class = "weather-panel",
        h4("Real-Time Weather in Melbourne"),
        uiOutput("current_weather")  
    ),
    div(
      style = "display: flex; justify-content: center; margin-top: 60px;", 
      div(
        class = "content-box",
        h2("Welcome to Melbourne!", style = "text-align: center;"),
        p("Melbourne is Australia's mecca for all things trendy and tasty. With exquisite dining, exhilarating sport and abundant art experiences, there are plenty of brilliant things to do in Melbourne.",
          style = "text-align: justify; font-size: 16px; margin-top: 10px;"),
        p("A perfect blend of rich cultural history and new age trends is waiting for you in Melbourne. As the sun goes down, the city comes to life with a vibrant dining scene as well as events and exhibitions. Explore its bustling laneways, trendy neighbourhoods and sophisticated foodie scene to get a taste of what Melbourne is all about.",
          style = "text-align: justify; font-size: 16px; margin-top: 10px;")
      )
    ),
    div(
      style = "display: flex; justify-content: center; margin-top: 20px; gap: 10px;",
      actionButton("find_accommodation", "Find Accommodation", class = "custom-button"),
      actionButton("Food", "Find Food", class = "custom-button"),
      actionButton("find_attractions", "Find Attractions", class = "custom-button"),
      actionButton("find_entertainment", "Find Entertainment", class = "custom-button")
    ),
    div(
      class = "tableau",
      tableauPublicViz(
        id = 'tableauViz',
        url = 'https://public.tableau.com/views/Book2_17299251352120/Dashboard1?:language=en-GB&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
        height = '100%',
        width = '100%'
      )
    ),
    
  )
)



########### food pages #######
custom_navbar <- tags$div(class = "navbar-custom",
                          style = "display: flex; gap: 20px;",  
                          tags$a("Must-visit restaurants in Melbourne", href = "#", onclick = "Shiny.setInputValue('page', 'must_visit_main', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("Restaurants", href = "#", onclick = "Shiny.setInputValue('page', 'restaurants', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("Coffee shop", href = "#", onclick = "Shiny.setInputValue('page', 'coffee_shop', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("Cheap Eats", href = "#", onclick = "Shiny.setInputValue('page', 'cheap_eats', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("Rooftop Bars", href = "#", onclick = "Shiny.setInputValue('page', 'best_bars', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("Breakfast and Brunch", href = "#", onclick = "Shiny.setInputValue('page', 'breakfast_brunch', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;"),
                          tags$a("More", href = "#", onclick = "Shiny.setInputValue('page', 'more', {priority: 'event'})", 
                                 style = "text-decoration: none; color: white;")
)
food <- tabPanel(
  "Food",
  fluidPage(
    useShinyjs(),
    div(class = "food-tab",
        includeCSS("www/style_wwx.css"),
        custom_navbar,
        uiOutput("main_content"))
  )
)

map <- tabPanel(
  "Map",
  fluidPage(
    useShinyjs(),  # Initialize shinyjs
    tags$head(
      tags$style(HTML("
    #control-panel {
      position: absolute;
      top: 60px;
      right: 20px;
      width: 450px;
      z-index: 1000;
      background-color: white;
      padding: 20px;
      box-shadow: 0px 0px 15px rgba(0,0,0,0.2);
      border-radius: 10px;
    }
    #info-panel {
      position: absolute;
      top: 120px;
      left: 40px;
      width: 400px;
      z-index: 1100;
      background-color: white;
      padding: 20px;
      box-shadow: 0px 0px 15px rgba(0,0,0,0.2);
      border-radius: 10px;
      display: none; 
    }
    .leaflet-container {
      height: 99vh;
      width: 99vw;
      margin: 0;
      padding: 0;
    }
    #navigation-buttons {
    position: absolute;
    bottom: 20px;
    left: 20px;
    z-index: 1500; /*Make sure the button is on top of the map */
    padding: 5px; /* Adjust the spacing of the overall container */
    }
    
    #navigation-buttons .btn {
      padding: 5px 10px; /* Reduce the padding of the button */
      font-size: 12px;   /* Reduce font size */
      border-radius: 3px; /* Adjust the corner radius of the button */
    }


    .shiny-text-output {
      background-color: rgba(255, 255, 255, 0);  /* Transparent content background */
      border: none;  /* Remove border */
    }
    #control-panel .btn, #navigation-buttons .btn {
    background-color: #ADD8E6 !important; /* Dark blue background */
    color: white !important; /* White font */
    border: none;
    }

    #control-panel .btn:hover, #navigation-buttons .btn:hover {
      background-color: #104E8B !important; /* Darker blue hover effect */
    }

    #control-panel .btn-primary, #control-panel .btn-secondary {
      background-color: #ADD8E6 !important; /* Dark blue background */
      color: white !important;
      border: none;
    }

    #control-panel .btn-primary:hover, #control-panel .btn-secondary:hover {
      background-color: #104E8B !important; /* Darker blue on hover */
    }
  "))
    ),
    
    # Instruction for the user
    div(id = "instructions",
        style = "position: absolute; top: 80px; left: 60px; background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 5px; z-index: 10;",
        "Tip: Click on any empty space on the map to hide the information panel."
    ),
    
    # Control panel (sidebar) as an absolute floating div
    div(
      id = "control-panel",
      textInput("searchPlace", "Search for a place:", value = "", placeholder = "Enter partial or full name"),
      actionButton("search", "Search"),
      uiOutput("matchSelection"),
      actionButton("reset", "Reset"),
      pickerInput(
        inputId = "buildingType", 
        label = "Select Building Types", 
        choices = unique(all_data$Type), 
        selected = NULL,
        multiple = TRUE,
        width = '100%',
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE
        )
      ),
      # Add Tram route selector to control-panel
      pickerInput(
        inputId = "tramRoute", 
        label = "Select Tram Route", 
        choices = sort(unique(tram_stops$ROUTEUSSP)),
        selected = NULL,
        multiple = TRUE,
        width = '100%',
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE
        )
      ),
      div(
        actionButton("show_free_tram_zone", "Show Free Tram Zone", class = "btn-primary"),
        actionButton("hide_free_tram_zone", "Cancel", class = "btn-secondary")
      )
    ),
    
    # Information panel to show details of the selected place
    div(
      id = "info-panel",
      htmlOutput("placeDetails")
    ),
    
    # Navigation buttons and clear selection
    div(id = "navigation-buttons",
        actionButton("enable_click_mode", "Enable Point Generation"),
        actionButton("clear_selection", "Clear Selection")
    ),
    
    # Main map panel
    leafletOutput("map", height = "98vh", width = "98vw")
  )
)

############# attractions #######
# "Must to do" submenu
must_do <- tabPanel(
    "Must to do",
    fluidPage(
      # Title and description
      h1(HTML("<b>Explore the Top Must-Visit Tourist Attractions</b>"), style = "text-align:center; margin-top: 20px;"),
      p("Discover the most breathtaking, historical, and cultural landmarks that you shouldn't miss!",
        style = "text-align:center; font-size: 18px; margin-bottom: 40px;"),
      
      # Custom styles
      tags$style(HTML("
                 /* Fixed review module styles */
                 .fixed-review {
                   position: fixed;
                   top: 50%;
                   transform: translateY(-50%);
                   left: 10px;
                   width: 300px;
                   padding: 20px;
                   background-color: #f9f9f9;
                   border: 1px solid #ddd;
                   border-radius: 5px;
                   box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                   z-index: 1000;
                 }
                 /* Fixed review module height */
                 .review-content {
                   height: 400px;
                   overflow: auto;
                 }
                 /* Adjust main content margin */
                 .main-content {
                   margin-left: 330px;
                 }
                 /* Responsive design */
                 @media (max-width: 768px) {
                   .fixed-review {
                     position: relative;
                     top: auto;
                     transform: none;
                     left: auto;
                     width: auto;
                   }
                   .main-content {
                     margin-left: 0;
                   }
                 }
               ")),
      
      # Page content
      div(
        # Review module
        div(
          class = "fixed-review",
          h3("Reviews"),
          div(
            class = "review-content",
            uiOutput("review_must")
          ),
          actionButton("next_review_must", "Next")
        ),
        # Main content
        div(
          class = "main-content",
          uiOutput("attractions"),
          
          # Pagination controls
          fluidRow(
            column(12, align = "center",
                   actionButton("prev_page", "Previous Page"),
                   textOutput("page_info", inline = TRUE),
                   actionButton("next_page", "Next Page")
            )
          )
        )
      )
    )
  )
# "Things to do" submenu
thing_do <- tabPanel(
    "Things to do",
    fluidPage(
      # Title and description
      h1(HTML("<b>Explore Various Activities and Experiences</b>"), style = "text-align:center; margin-top: 20px;"),
      p("Discover a variety of activities and experiences!",
        style = "text-align:center; font-size: 18px; margin-bottom: 40px;"),
      
      # Custom styles
      tags$style(HTML("
                 .card-horizontal {
                   display: flex;
                   flex-wrap: wrap;
                   margin-bottom: 20px;
                   border: 1px solid #ddd;
                   border-radius: 5px;
                   overflow: hidden;
                   background-color: #fff;
                 }
                 .card-horizontal .card-img {
                   width: 100%;
                   height: auto;
                 }
                 .card-horizontal .card-body {
                   padding: 15px;
                   width: 100%;
                 }
                 @media (min-width: 768px) {
                   .card-horizontal {
                     flex-direction: row;
                   }
                   .card-horizontal .card-img {
                     width: 30%;
                     height: 200px;
                     overflow: hidden;
                   }
                   .card-horizontal .card-body {
                     width: 70%;
                   }
                 }
                 .card-horizontal .card-img img {
                   width: 100%;
                   height: 100%;
                   object-fit: cover;
                 }
                 /* Fixed review module styles */
                 .fixed-review {
                   position: fixed;
                   top: 50%;
                   transform: translateY(-50%);
                   left: 10px;
                   width: 300px;
                   padding: 20px;
                   background-color: #f9f9f9;
                   border: 1px solid #ddd;
                   border-radius: 5px;
                   box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                   z-index: 1000;
                 }
                 /* Fixed review module height */
                 .review-content {
                   height: 400px;
                   overflow: auto;
                 }
                 /* Adjust main content margin */
                 .main-content {
                   margin-left: 330px;
                 }
                 /* Responsive design */
                 @media (max-width: 768px) {
                   .fixed-review {
                     position: relative;
                     top: auto;
                     transform: none;
                     left: auto;
                     width: auto;
                   }
                   .main-content {
                     margin-left: 0;
                   }
                 }
               ")),
      
      # Page content
      div(
        # Review module
        div(
          class = "fixed-review",
          h3("Reviews"),
          div(
            class = "review-content",
            uiOutput("review_things")
          ),
          actionButton("next_review_things", "Next")
        ),
        # Main content
        div(
          class = "main-content",
          uiOutput("things_to_do_attractions"),
          
          # Pagination controls
          fluidRow(
            column(12, align = "center",
                   actionButton("things_prev_page", "Previous Page"),
                   textOutput("things_page_info", inline = TRUE),
                   actionButton("things_next_page", "Next Page")
            )
          )
        )
      )
    )
  )

############# ui #########
ui <- navbarPage(
  header = setUpTableauInShiny(),
  theme = shinytheme("journal"),
  id = "tabs", 
  "Melbourne Exploration",
  home_tab,
  navbarMenu("Hotels",
             tabPanel("Hotel Listings", hotel_tab), 
             tabPanel("Top 5 Hotels", top5_hotel)
  ),
  tabPanel("Food", food),
  
  # Add Attractions Menu
  navbarMenu("Attractions", 
             tabPanel("Must to do", must_do),
             tabPanel("Things to do", thing_do)),
  
  # New "Entertainment" tab
  tabPanel(
    "Entertainment",
    fluidPage(
      # Title and description
      h1(HTML("<b>Explore Entertainment</b>"), style = "text-align:center; margin-top: 20px;"),
      p("Discover a variety of entertainment facilities!",
        style = "text-align:center; font-size: 18px; margin-bottom: 40px;"),
      
      # Custom styles
      tags$style(HTML("
      .entertainment-card {
        display: flex;
        flex-wrap: wrap;
        margin-bottom: 20px;
        border: 1px solid #ddd;
        border-radius: 5px;
        overflow: hidden;
        background-color: #fff;
      }
      .entertainment-card .card-img {
        width: 100%;
        height: auto;
      }
      .entertainment-card .card-body {
        padding: 15px;
        width: 100%;
      }
      @media (min-width: 768px) {
        .entertainment-card {
          flex-direction: row;
        }
        .entertainment-card .card-img {
          width: 30%;
          height: 200px;
          overflow: hidden;
        }
        .entertainment-card .card-body {
          width: 70%;
        }
      }
      .entertainment-card .card-img img {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
    ")),
      
      # Page content with filter and main content
      fluidRow(
        # Filter column
        column(
          width = 2,
          h3("Category"),
          checkboxGroupInput(
            inputId = "entertainment_categories",
            label = NULL,
            choices = c("Bar", "Playground", "Karaoke", "Casino"),
            selected = c("Bar", "Playground", "Karaoke", "Casino")
          )
        ),
        # Main content column
        column(
          width = 9,
          uiOutput("entertainment_facilities"),
          
          # Pagination controls
          fluidRow(
            column(12, align = "center",
                   actionButton("entertainment_prev_page", "Previous Page"),
                   textOutput("entertainment_page_info", inline = TRUE),
                   actionButton("entertainment_next_page", "Next Page")
            )
          )
        )
      )
    )
  ),
  map,
  # New Guide tab
  tabPanel(
    "Guide",
    fluidPage(
      h1(HTML("<b>User Guidance</b>"), style = "text-align:center; margin-top: 20px;"),
      p("Your guide to exploring Melbourne, with tips and recommendations!",
        style = "text-align:center; font-size: 18px; margin-bottom: 40px;"),
      
      # Custom styles for guide section
      tags$style(HTML("
      .guide-section {
        background-color: #f9f9f9;
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 5px;
        margin-bottom: 20px;
        height:550px;
      }
      .guide-section h3 {
        font-weight: bold;
      }
      .guide-section p {
        font-size: 16px;
      }
      ")),
      
      # Guide content
      fluidRow(
        column(
          width = 4,
          div(class = "guide-section",
              h3("Home Page"),
              tags$ul(
                tags$li("A brief introduction to the City of Melbourne in the center!"),
                tags$li("Redirect you to the page you are interested in through our 4 blue Buttons!"),
                tags$li("A glance of current weather conditions in Melbourne on the top-left corner!"),
                tags$li("The tableau integrates the distribution of Hotels, Attractions, Entertainments, Restaurants and so on!
                        You can see (from the pie charts) the price range of everyday consumption in Melbourne and the distribution of shops from different countries."),
                tags$li("A glance of weather conditions in previous days to compare with the current weather!"),
                tags$li("Find the top five popular attractions and Hotels in Melbourne in the bar charts!")
              )
          )
        ),
        column(
          width = 4,
          div(class = "guide-section",
              h3("Hotels Page"),
              tags$ul(
                tags$li("The tabPanel is divided into 2 sub-panels! The top-5 hotel shows the five highest rated five-star hotels!"),
                tags$li("The Hotel listings page is designed to help you find a hotel that matches your needs."),
                tags$li("The Price range selector helps you find the hotels you can afford."),
                tags$li("The Hotel Class selector helps you locate the hotel of your desired level."),
                tags$li("The category selector helps you find hotels with the amenities you need."),
                tags$li("The search box uses fuzzy search to try to find the hotel name you entered for you.")
              )
          )
        ),
        
        
        
        column(
          width = 4,
          div(class = "guide-section",
              h3("Food Page"),
              
              h4("Must-visit Restaurants"),
              p("Browse Melbourne's top dining spots with a rotating image gallery. Click images to view categories like cafes, restaurants, and budget-friendly options."),
              
              h4("Dining Categories"),
              tags$ul(
                tags$li(
                  strong("Cafes:"), 
                  " Explore 5 top-rated cafes with descriptions, ratings, and details."
                ),
                tags$li(
                  strong("Restaurants:"), 
                  " Find 5 recommended restaurants for various occasions."
                ),
                tags$li(
                  strong("Cheap Eats:"), 
                  " Discover 5 affordable yet quality dining options."
                ),
                tags$li(
                  strong("Rooftop Bars:"), 
                  " Enjoy Melbourne’s skyline at 5 selected rooftop bars."
                ),
                tags$li(
                  strong("Breakfast & Brunch:"), 
                  " Start your day at 5 popular breakfast and brunch spots."
                ),
                tags$li(
                  strong("More"),
                  "Need more choices? Visit the 'More' page to filter and explore additional dining options by type and cuisine."
                )
              ),
              
             
          )
        ),
        
        
        column(
          width = 4,
          div(class = "guide-section",
              h3("Map Page"),
              tags$ul(
                tags$li("The map page provides a fuzzy search feature, allowing users to easily locate their desired building by name."),
                tags$li("Select a tram route and enable the Free Tram Zone overlay to identify accessible tram routes."),
                tags$li("Clicking a building icon displays specific information about that location."),
                tags$li("The 'Enable Point Generation' button, located in the lower-left corner, activates Google navigation, allowing users to set two points for route guidance.")
              )
          )
        ),
        
        column(
          width = 4,
          div(class = "guide-section",
              h3("Attractions"),
              tags$ul(
                tags$li("The tabPanel is divided into 2 sub-panels! One is must to go, the other is Tings to go."),
                tags$li("Must to go introduces Melbourne's top attractions."),
                tags$li("Things to go introduces other types of attractions."),
                tags$li("Users can scroll through reviews on the left side related to the attraction content."),
                tags$li("Users can also click the View Details button to jump to Google Maps."),
              )
          )
        ),
        
        column(
          width = 4,
          div(class = "guide-section",
              h3("Entertainment"),
              tags$ul(
                tags$li("Entertainment venues are displayed and category filters are provided, "),
                tags$li("such as bars, amusement parks, karaoke, and casinos."),
                tags$li("which include pictures, descriptions, ratings, and buttons that link to Google Maps."),
              )
          )
        ),
        
        
        
      )
    )
  ),
)

server <- function(input, output, session) {
  
  observeEvent(input$find_accommodation, {
    updateTabsetPanel(session, "tabs", selected = "Hotel Listings")  
  })
  
  observeEvent(input$Food, {
    updateTabsetPanel(session, "tabs", selected = "Food")
  })
  observeEvent(input$find_attractions, {
    updateTabsetPanel(session, "tabs", selected = "Must to do")
  })
  observeEvent(input$find_entertainment, {
    updateTabsetPanel(session, "tabs", selected = "Entertainment")
  })
  
  
  
  top_5_hotels <- reactive({
    hotel_data %>%
      filter(Parsed_Hotel_Class == 5) %>%
      arrange(desc(`Average Rating`)) %>%  # descending by rating
      head(5)  # top 5
  })
  
  selected_hotel <- reactiveVal(NULL)
  sorted_hotels <- reactive({
    data <- filtered_hotels()  # fitlered hotel
    
    # sorting metric
    if (input$sort_by == "price") {
      # by price
      if (input$sort_direction == "low_to_high") {
        data <- data %>% arrange(Price)
      } else {
        data <- data %>% arrange(desc(Price))
      }
    } else if (input$sort_by == "rating") {
      # by rating
      if (input$sort_direction == "low_to_high") {
        data <- data %>% arrange(`Average Rating`)
      } else {
        data <- data %>% arrange(desc(`Average Rating`))
      }
    }
    
    data
  })
  
  
  output$hotelMap <- renderLeaflet({
    leaflet(hotel_data) %>% 
      addProviderTiles("Esri.WorldStreetMap") %>%
      addMarkers(
        ~Longitude, ~Latitude,
        popup = ~Name,
        icon = custom_icon,
        layerId = ~Name
      ) %>%
      setView(lng = 144.965347, lat = -37.814852,zoom = 14)
  })
  
  # Filter hotel data based on user selection
  filtered_hotels <- reactive({
    data <- hotel_data
    
    if (!is.null(input$location) && input$location != "") {
      search_term <- tolower(input$location)  
      matching_indices <- agrep(search_term, tolower(data$Name), max.distance = 0.1)
      data <- data[matching_indices, ]
    }
    
    if (length(input$category) > 0) {
      data <- data[sapply(data$Amenities, function(x) any(input$category %in% trimws(unlist(strsplit(x, ","))))), ]
    }
    
    if (length(input$hotel_class) > 0) {
      data <- data[!is.na(data$Parsed_Hotel_Class) & data$Parsed_Hotel_Class %in% as.numeric(input$hotel_class), ]
    }
    
    if (!is.null(input$priceRange)) {
      data <- data[data$Price >= input$priceRange[1] & data$Price <= input$priceRange[2], ]
    }
    
    return(data)
  })
  
  # Main view: Display all hotel cards
  output$mainContent <- renderUI({
    if (is.null(selected_hotel())) {
      filtered_data <- sorted_hotels()
      # Check if the filtered data is empty
      if (nrow(filtered_data) == 0) {
        div(style = "text-align: center; padding: 20px; color: #555; font-size: 18px; font-weight: bold;",
            "No Hotels Available")
      } else {
        tagList(lapply(1:nrow(filtered_data), function(i) {
          generateHotelSummary(
            name = filtered_data$Name[i],
            type = filtered_data$Categories[i],
            rating = filtered_data$`Hotel Class`[i],
            address = filtered_data$Fulladdress[i],
            website = filtered_data$Website[i],
            price = filtered_data$Price[i],
            image_url = filtered_data$`Featured Image`[i],
            hotel_id = filtered_data$Name[i],
            review_count = filtered_data$`Review Count`[i],  
            average_rating = filtered_data$`Average Rating`[i],
            amenities = filtered_data$Amenities[i]
          )
        }))
      }
    } else {
      i <- selected_hotel()
      description <- ifelse(is.na(hotel_data$Description[i]), 
                            HTML("No Description Available"), 
                            hotel_data$Description[i])
      
      div(
        class = "details-view",
        div(
          class = "details-card",
          h2(hotel_data$Name[i],style = "text-align: center;"),
          div(style = "text-align: center;",
              img(src = hotel_data$`Featured Image`[i], height = "200px", style = "margin: 15px 0;")
          ),
          p(description),  
          p(HTML(paste("<strong>Price:</strong> AU$", hotel_data$Price[i]))),
          p(HTML(paste("<strong>Category:</strong>", hotel_data$Categories[i]))),
          p(HTML(paste("<strong>Hotel Class:</strong>", hotel_data$`Hotel Class`[i]))),
          p(HTML(paste("<strong>Rating:</strong>", hotel_data$`Average Rating`[i]))),
          p(HTML(paste("<strong>Address:</strong>", hotel_data$Fulladdress[i]))),
          p(HTML(paste("<strong>Phone:</strong>", hotel_data$Phone[i]))),  
          p(a(href = hotel_data$Website[i], "Visit Website", target = "_blank",style = "text-decoration: none; font-weight: bold; color: blue;")),
          div(
            style = "display: flex; justify-content: space-between; margin-top: 20px;",
            
            actionButton("back_to_list", "Back to List", style = "margin-top: 20px;"),
            
            # The button on the right "Open in Google Maps"
            p(a(href = hotel_data$`Google Maps URL`[i], "Open in Google Maps", target = "_blank",
                class = "btn btn-primary", style = "margin-top: 20px;"))
          )
        )
      )
    }
  })   
  observeEvent(input$back_to_list, {
    selected_hotel(NULL)
  })
  
  lapply(1:nrow(hotel_data), function(i) {
    observeEvent(input[[paste0("hotel_", hotel_data$Name[i])]], {  # Use hotel name as ID
      # Find the index corresponding to the hotel name
      selected_hotel_index <- which(hotel_data$Name == hotel_data$Name[i])
      selected_hotel(selected_hotel_index)  
    })
  })
  
  observeEvent(input$hotelMap_marker_click, {
    click <- input$hotelMap_marker_click  
    selected_name <- click$id  
    
    # Use the which() function to find the hotel_id corresponding to the hotel name
    hotel_id <- which(hotel_data$Name == selected_name)
    print(hotel_id)
    # If a matching hotel name is found, update selected_hotel
    if (length(hotel_id) > 0) {
      selected_hotel(hotel_id)  
    }
  })
  
  output$leftColumnHotels <- renderUI({
    tagList(
      generateHotelImage(paste0("", image_files[1])), 
      generateHotelDescription(
        name = top_5_hotels()$Name[2],
        address = top_5_hotels()$Fulladdress[2],
        description = top_5_hotels()$Description[2],
        rating = top_5_hotels()$`Average Rating`[2],
        price = top_5_hotels()$Price[2],
        review_count = top_5_hotels()$`Review Count`[2],
        phone = top_5_hotels()$Phone[2],
        website = top_5_hotels()$Website[2],
        google_maps_url = top_5_hotels()$`Google Maps URL`[2]
      ),
      generateHotelImage(paste0("", image_files[3])),  
      generateHotelDescription(
        name = top_5_hotels()$Name[4],
        address = top_5_hotels()$Fulladdress[4],
        description = top_5_hotels()$Description[4],
        rating = top_5_hotels()$`Average Rating`[4],
        price = top_5_hotels()$Price[4],
        review_count = top_5_hotels()$`Review Count`[4],
        phone = top_5_hotels()$Phone[4],
        website = top_5_hotels()$Website[4],
        google_maps_url = top_5_hotels()$`Google Maps URL`[4]
      ),
      generateHotelImage(paste0("", image_files[5]))  
    )
  })
  
  # Render the hotel pictures and descriptions on the right (2, 4)
  output$rightColumnHotels <- renderUI({
    tagList(
      generateHotelDescription(
        name = top_5_hotels()$Name[1],
        address = top_5_hotels()$Fulladdress[1],
        description = top_5_hotels()$Description[1],
        rating = top_5_hotels()$`Average Rating`[1],
        price = top_5_hotels()$Price[1],
        review_count = top_5_hotels()$`Review Count`[1],
        phone = top_5_hotels()$Phone[1],
        website = top_5_hotels()$Website[1],
        google_maps_url = top_5_hotels()$`Google Maps URL`[1]
      ),
      generateHotelImage(paste0("", image_files[2])),  
      generateHotelDescription(
        name = top_5_hotels()$Name[3],
        address = top_5_hotels()$Fulladdress[3],
        description = top_5_hotels()$Description[3],
        rating = top_5_hotels()$`Average Rating`[3],
        price = top_5_hotels()$Price[3],
        review_count = top_5_hotels()$`Review Count`[3],
        phone = top_5_hotels()$Phone[3],
        website = top_5_hotels()$Website[3],
        google_maps_url = top_5_hotels()$`Google Maps URL`[3]
      ),
      generateHotelImage(paste0("", image_files[4])),
      generateHotelDescription(
        name = top_5_hotels()$Name[5],
        address = top_5_hotels()$Fulladdress[5],
        description = top_5_hotels()$Description[5],
        rating = top_5_hotels()$`Average Rating`[5],
        price = top_5_hotels()$Price[5],
        review_count = top_5_hotels()$`Review Count`[5],
        phone = top_5_hotels()$Phone[5],
        website = top_5_hotels()$Website[5],
        google_maps_url = top_5_hotels()$`Google Maps URL`[5]
      )
    )
  })
  
  rv <- reactiveValues(page = "must_visit_main", current_page = 1)
  
  
  observeEvent(input$page, {
    rv$page <- input$page
  })
  
  observeEvent(input$more_page, {
    rv$page <- "more"
  })
  
  observeEvent(input$return_page, {
    rv$page <- "must_visit_main"  
  })
  
  observeEvent(input$bestbars_prev_page, {
    rv$page <- "cheap_eats"  
  })
  
  observeEvent(input$bestbars_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$bestbars_next_page, {
    rv$page <- "breakfast_brunch"  
  })
  
  observeEvent(input$bestbars_more_page, {
    rv$page <- "more"
  })
  
  
  observeEvent(input$brunch_prev_page, {
    rv$page <- "best_bars"  
  })
  
  observeEvent(input$brunch_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$brunch_next_page, {
    rv$page <- "restaurants"  
  })
  
  observeEvent(input$brunch_more_page, {
    rv$page <- "more"
  })
  
  
  observeEvent(input$restaurants_prev_page, {
    rv$page <- "breakfast_brunch"  
  })
  
  observeEvent(input$restaurants_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$restaurants_next_page, {
    rv$page <- "coffee_shop"  
  })
  
  observeEvent(input$restaurants_more_page, {
    rv$page <- "more"
  })
  
  
  observeEvent(input$coffee_prev_page, {
    rv$page <- "restaurants"  
  })
  
  observeEvent(input$coffee_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$coffee_next_page, {
    rv$page <- "cheap_eats"  
  })
  
  observeEvent(input$coffee_more_page, {
    rv$page <- "more"
  })
  
  
  
  observeEvent(input$cheapeats_prev_page, {
    rv$page <- "coffee_shop"  
  })
  
  observeEvent(input$cheapeats_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$cheapeats_next_page, {
    rv$page <- "best_bars"  
  })
  
  observeEvent(input$cheapeats_more_page, {
    rv$page <- "more"
  })
  
  
  
  observeEvent(input$bestbars_prev_page, {
    rv$page <- "cheap_eats"  
  })
  
  observeEvent(input$bestbars_back_page, {
    rv$page <- "must_visit_main"
  })
  
  observeEvent(input$bestbars_next_page, {
    rv$page <- "breakfast_brunch"  
  })
  
  observeEvent(input$bestbars_more_page, {
    rv$page <- "more"
  })
  
  
  observeEvent(rv$page, {
    session$onFlushed(function() {
      runjs("window.scrollTo(0,0);")
    }, once = TRUE)
  })
  
  
  ################## more_data ####################
  
  
  restaurants <- read.csv("data/Restaurants.csv", stringsAsFactors = FALSE) %>%
    filter(!str_detect(Categories, "Coffee & Bar"))
  coffeeshop <- read.csv("data/coffeeshop.csv", stringsAsFactors = FALSE)
  brunch <- read.csv("data/brunch.csv", stringsAsFactors = FALSE)
  breakfast <- read.csv("data/breakfast.csv", stringsAsFactors = FALSE)
  foortop <- read.csv("data/foortop.csv", stringsAsFactors = FALSE)
  cheapeat <- read.csv("data/cheapeat.csv", stringsAsFactors = FALSE)
  
  restaurants$Type <- "Restaurant"
  coffeeshop$Type <- "Coffee Shop"
  brunch$Type <- "Brunch"
  breakfast$Type <- "Breakfast"
  foortop$Type <- "Footop"
  cheapeat$Type <- "Cheapeat"
  
  combined_data <- bind_rows(restaurants, coffeeshop, brunch, breakfast, foortop, cheapeat)
  shuffled_data <- combined_data %>% sample_frac(1)
  
  classify_cuisine <- function(cuisine) {
    if (is.na(cuisine)) {
      return("Other")
    } else if (str_detect(cuisine, "American restaurant|Hamburger restaurant|Diner|Fried chicken takeaway|Fast food restaurant|Southern restaurant \\(US\\)")) {
      return("American")
    } else if (str_detect(cuisine, "Chinese restaurant|Chinese takeaway|Dim sum restaurant|Dumpling restaurant|Chinese noodle restaurant|Shanghainese restaurant|Cantonese restaurant")) {
      return("Chinese")
    } else if (str_detect(cuisine, "Vietnamese restaurant|Pho restaurant|Banh mi")) {
      return("Vietnamese")
    } else if (str_detect(cuisine, "Korean restaurant|Korean barbecue restaurant|Korean rib restaurant|Korean beef restaurant")) {
      return("Korean")
    } else if (str_detect(cuisine, "Japanese restaurant|Sushi restaurant|Ramen restaurant|Yakitori|Authentic Japanese restaurant")) {
      return("Japanese")
    } else if (str_detect(cuisine, "Indonesian restaurant|Malaysian restaurant|Burmese restaurant|Southeast Asian restaurant")) {
      return("Southeast Asian")
    } else if (str_detect(cuisine, "Thai restaurant")) {
      return("Thai")
    } else if (str_detect(cuisine, "Bangladeshi restaurant|Hyderabadi restaurant|Indian restaurant|Pakistani restaurant|Biryani restaurant|North Indian restaurant|Punjabi restaurant|Modern Indian restaurant|Indian takeaway|South Asian restaurant")) {
      return("Indian/Pakistani")
    } else if (str_detect(cuisine, "Italian restaurant|Pizza restaurant|Pizza delivery|Pizza takeaway|Pasta shop|Neapolitan restaurant|Northern Italian restaurant")) {
      return("Italian")
    } else if (str_detect(cuisine, "Mexican restaurant|Burrito restaurant|Taco restaurant")) {
      return("Mexican")
    } else if (str_detect(cuisine, "Greek restaurant|Turkish restaurant|Lebanese restaurant|Mediterranean restaurant")) {
      return("Mediterranean")
    } else if (str_detect(cuisine, "Persian restaurant|Middle Eastern restaurant")) {
      return("Middle Eastern")
    } else if (str_detect(cuisine, "Modern Australian restaurant|Australian restaurant")) {
      return("Australian")
    } else {
      return("Other")
    }
  }
  
  if(!"Categories" %in% colnames(combined_data)){
    stop("The dataset is missing the 'Categories' column for cuisine classification.")
  }
  
  combined_data$Cuisine <- sapply(combined_data$Categories, classify_cuisine)
  combined_data <- combined_data %>%
    distinct(Name, Fulladdress, Cuisine, .keep_all = TRUE)  
  
  unique_cuisines <- sort(unique(combined_data$Cuisine))
  unique_cuisines <- unique_cuisines[unique_cuisines != "Other"]
  unique_cuisines <- c(unique_cuisines, "Other")
  
  
  filtered_data <- reactive({
    data <- if (input$type_filter == "All") {
      shuffled_data
    } else {
      combined_data %>% filter(Type == input$type_filter)
    }
    
    if (input$type_filter == "Restaurant" && input$cuisine_filter != "All") {
      if("Cuisine" %in% colnames(data)){
        data <- data %>% filter(Cuisine == input$cuisine_filter)
      } else {
        showNotification("Cuisine column is missing in the data.", type = "error")
      }
    }
    
    if (input$search != "") {
      search_term <- tolower(input$search)
      data <- data %>% filter(
        str_detect(tolower(Name), fixed(search_term)) |
          (if("Cuisine" %in% colnames(data)) str_detect(tolower(Cuisine), fixed(search_term)) else FALSE) |
          str_detect(tolower(Description), fixed(search_term))
      )
    }
    
    
    data <- data %>% distinct(Name, Fulladdress,  .keep_all = TRUE)
    
    data
  })
  
  
  
  total_pages <- reactive({
    items_per_page <- as.numeric(input$items_per_page)
    data <- filtered_data()
    if (items_per_page > 0) {
      ceiling(nrow(data) / items_per_page)
    } else {
      1
    }
  })
  
  
  observeEvent(input$items_per_page, {
    rv$current_page <- 1
  })
  
  observeEvent(input$type_filter, {
    rv$current_page <- 1
    if (input$type_filter == "Restaurant") {
      enable("cuisine_filter")
    } else {
      updateSelectInput(session, "cuisine_filter", selected = "All")
      disable("cuisine_filter")
    }
  })
  
  observeEvent(input$search, {
    rv$current_page <- 1
  })
  
  observeEvent(input$page_input, {
    if (!is.na(input$page_input) && input$page_input >= 1 && input$page_input <= total_pages()) {
      rv$current_page <- input$page_input
    } else {
      showNotification("Invalid page number!", type = "error")
    }
  })
  
  
  observeEvent(input$goto_page, {
    if (!is.na(input$goto_page) && input$goto_page >= 1 && input$goto_page <= total_pages()) {
      rv$current_page <- input$goto_page
    }
  })
  
  
  output$pagination_controls <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    total_pages_num <- total_pages()
    current_page <- rv$current_page
    
    page_buttons <- tagList()
    if (total_pages_num <= 5) {
      for (page in 1:total_pages_num) {
        page_buttons <- tagAppendChild(
          page_buttons,
          tags$button(
            page, 
            id = paste0("page_", page), 
            class = ifelse(page == current_page, "btn custom-btn", "btn btn-default"), 
            onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", page)
          )
        )
      }
    } else {
      if (current_page <= 3) {
        for (page in 1:4) {
          page_buttons <- tagAppendChild(
            page_buttons,
            tags$button(
              page, 
              id = paste0("page_", page), 
              class = ifelse(page == current_page, "btn custom-btn", "btn btn-default"), 
              onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", page)
            )
          )
        }
        page_buttons <- tagAppendChild(page_buttons, tags$span("..."))
        page_buttons <- tagAppendChild(
          page_buttons,
          tags$button(
            total_pages_num, 
            id = paste0("page_", total_pages_num), 
            class = "btn btn-default", 
            onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", total_pages_num)
          )
        )
      } else if (current_page > total_pages_num - 3) {
        page_buttons <- tagAppendChild(
          page_buttons,
          tags$button(
            1, 
            id = "page_1", 
            class = "btn btn-default", 
            onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", 1)
          )
        )
        page_buttons <- tagAppendChild(page_buttons, tags$span("..."))
        for (page in (total_pages_num - 3):total_pages_num) {
          page_buttons <- tagAppendChild(
            page_buttons,
            tags$button(
              page, 
              id = paste0("page_", page), 
              class = ifelse(page == current_page, "btn custom-btn", "btn btn-default"), 
              onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", page)
            )
          )
        }
      } else {
        page_buttons <- tagAppendChild(
          page_buttons,
          tags$button(
            1, 
            id = "page_1", 
            class = "btn btn-default", 
            onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", 1)
          )
        )
        page_buttons <- tagAppendChild(page_buttons, tags$span("..."))
        for (page in (current_page - 1):(current_page + 1)) {
          page_buttons <- tagAppendChild(
            page_buttons,
            tags$button(
              page, 
              id = paste0("page_", page), 
              class = ifelse(page == current_page, "btn custom-btn", "btn btn-default"), 
              onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", page)
            )
          )
        }
        page_buttons <- tagAppendChild(page_buttons, tags$span("..."))
        page_buttons <- tagAppendChild(
          page_buttons,
          tags$button(
            total_pages_num, 
            id = paste0("page_", total_pages_num), 
            class = "btn btn-default", 
            onclick = sprintf("Shiny.setInputValue('goto_page', %d, {priority: 'event'})", total_pages_num)
          )
        )
      }
    }
    
    tagList(
      tags$div(
        class = "more-pagination",  
        actionButton("prev_page", "Previous", class = "btn custom-btn"),
        page_buttons,
        actionButton("next_page", "Next", class = "btn custom-btn")
      )
    )
  })
  
  
  observeEvent(input$prev_page, {
    rv$current_page <- max(1, rv$current_page - 1)
  })
  
  observeEvent(input$next_page, {
    rv$current_page <- min(total_pages(), rv$current_page + 1)
  })
  
  
  output$restaurant_cards <- renderUI({
    data <- filtered_data()
    items_per_page <- as.numeric(input$items_per_page)
    start <- (rv$current_page - 1) * items_per_page + 1
    end <- min(start + items_per_page - 1, nrow(data))
    data_subset <- data[start:end, ]
    
    if(nrow(data_subset) == 0){
      return(tags$div("No results found.", style = "font-size: 18px; color: red;"))
    }
    
    card_list <- lapply(1:nrow(data_subset), function(i) {
      row <- data_subset[i, ]
      
      
      tags$div(class = "more-restaurant-card",
               tags$div(
                 tags$img(src = row$Featured.Image, alt = row$Name)
               ),
               tags$div(class = "card-content",  
                        tags$h5(paste0(i + start - 1, ". ", row$Name)),
                        tags$div(
                          tags$img(src = "https://img.icons8.com/ios-filled/50/000000/marker.png", 
                                   class = "marker-icon"),
                          tags$a(row$Fulladdress, href = row$Google.Maps.URL, target = "_blank")
                        ),
                        tags$p(paste("Rating:", row$Average.Rating, "| Reviews:", row$Review.Count), 
                               class = "rating-reviews"),
                        tags$p(row$Description),
                        tags$p(paste("Opening Hours:", str_replace_all(row$Opening.Hours, "[\\[\\]]", ""))),
                        tags$p(paste("Price Range:", row$Price)),
                        if (!is.na(row$Website) && row$Website != "") {
                          tags$a("Visit Website", href = row$Website, target = "_blank")
                        }
               )
      )
      
    })
    tagList(card_list)
  })
  
 
  output$main_content <- renderUI({
    if (rv$page == "must_visit_main") {
      tagList(
        fluidRow(
          h1("The Best Restaurants in Melbourne Right Now"),
          p("Melbourne has long held the title of Australia's most exciting food capital. Sure, we've seen Sydney creeping 
             up the ranks and many top chefs moving to regional towns in recent years. But Melbourne's best restaurants continue 
             to hold their own. From restaurants in Melbourne considered some of the best in the world to trendy must visit venues 
             one imagines is where celebrities dine to family owned new neighbourhood gems that epitomise the city's effortless cool. 
             Melbourne — and wider Victoria — is known for its food."),
          p("We've scoured the city of Melbourne and its suburbs for the best places to eat, drink and celebrate Melbourne's 
             longstanding restaurant culture.")
        ),
        
        div(style = "display: flex; justify-content: center;",
            slickROutput("carousel", width = "90%")
        )
      )
    } else if (rv$page == "restaurants") {
      div(class = "page-content",
          h2("5 OF MELBOURNE'S BEST RESTAURANTS", class = "detail-title"),
          h3("From stylish and sophisticated to inspired and experimental, Melbourne's renowned restaurants are in a league of their own.", class = "detail-subtitle"),
          h3("By Lana Bogunovich", class = "detail-author"),
          p("With its internationally renowned chefs, multi-cultural flavours and a region abundant with incredible ingredients, Melbourne's reputation as Australia's culinary capital remains firm. A food lover's visit to Melbourne would be incomplete without dining at one of its famous restaurants, so here are just a few that are sure to satisfy the most discerning epicurean."),
          
          div(class = "restaurant-section",
              h3("FARMER'S DAUGHTERS", class = "restaurant-name"),
              img(src = "res_top1.png", class = "restaurant-image"),
              p("Farmer's Daughters, Melbourne, Victoria © Farmer's Daughters, Thom Rigney"),
              p("Where: 95 Exhibition Street, Melbourne"),
              HTML('<p><a href="https://www.farmersdaughters.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Farmer\'s Daughters</a> is a celebration of Gippsland\'s seasons and the bounty they produce. The seasonally-driven menus are designed to showcase regional ingredients, wine and spirits, producers and stories, from the coast to the mountains. Dishes are created in the open campfire kitchen, cooked over charcoal and wood to bring out the rustic country flavours of each ingredient. The venue also features a deli on the ground floor and a greenhouse-inspired rooftop bar, lush with native plants and herbs used in many of the cocktails and dishes.</p>')
          ),
          
          div(class = "card",
              img(src = "top_res1_small.png"),
              div(class = "card-content",
                  h4("Farmer's Daughters", class = "card-title"),
                  p("Eat and Drink • Restaurant and Cafe", class = "card-subtitle"),
                  a("Learn more", href = "https://www.farmersdaughters.com.au/", target = "_blank", class = "card-link")
              )
          ),
          
          div(class = "restaurant-section",
              h3("CHIN CHIN", class = "restaurant-name"),
              img(src = "res_top2.png", class = "restaurant-image"),
              p("South East Asian cuisine at Chin Chin, Melbourne, Victoria © LUCAS Restaurants"),
              p("Where: 125 Flinders Lane, Melbourne"),
              HTML('<p> This eclectic South East Asian diner is one of Melbourne\'s most popular restaurants, serving up expressive and experimental Thai-influenced dishes that are as dynamic as the restaurant\'s rambunctious ambience of upbeat music and contemporary art-adorned walls. Dishes at <a href="https://www.chinchin.melbourne/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Chin Chin</a> are made to share, with a varied menu of zesty salads, spicy curries, punchy stir fries, smoky barbecued meats and plenty of vegetarian options. Arrive early, leave your name at the door, then go have a drink at GoGo Bar underneath the restaurant and you\'ll get a call when your table is available.</p>')
          ),
          
          div(class = "card",
              img(src = "top_res2_small.png"),
              div(class = "card-content",
                  h4("Chin Chin", class = "card-title"),
                  p("Modern Thai Fusion • Vibrant Atmosphere", class = "card-subtitle"),
                  a("Learn more", href = "https://www.chinchin.melbourne/", target = "_blank", class = "card-link")
              )
          )
          ,
          
          
          div(class = "restaurant-section with-tip",
              div(class = "restaurant-content",
                  h3("GIMLET", class = "restaurant-name"),
                  img(src = "res_top3.png", class = "restaurant-image"),
                  p("Gimlet, Melbourne, Victoria © Trader House Restaurants, Sharyn Cairns"),
                  p("Where: 33 Russell St, Melbourne"),
                  HTML('<p>Housed inside the beautiful heritage building Cavendish House, <a href="https://gimlet.melbourne/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Gimlet</a> oozes classic European charm. The ultra-high ceilings, dark wood floors, plush booths, black marble bar, and Art Deco columns and light-fittings set a glamorous yet comfortable scene, making this cocktail bar and restaurant ideally suited to any occasion. The Mediterranean-style menu features fresh seafood, seasonal vegetables, and wood-fire roasted meats, while the bar serves up fresh and inventive cocktails. For the ultimate indulgence, treat yourself to the decadent Caviar service.</p>'),
                  HTML('<p style="color: #FF6D00; font-weight: bold; background-color: #FFF3E0; padding: 5px; border-radius: 4px;">Top tip: If you\'re looking for a late-night bite, Gimlet does a supper menu on Friday and Saturday nights from 10pm until 1am.</p>')
              )
          ),
          
          
          div(class = "card",
              img(src = "top_res3_small.png"),
              div(class = "card-content",
                  h4("Gimlet", class = "card-title"),
                  p("Elegant European Dining • Classic Cocktails", class = "card-subtitle"),
                  a("Learn more", href = "https://gimlet.melbourne/", target="_blank", class = "card-link")
              )
          ),
          
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("ATTICA", class = "restaurant-name"),
                  img(src = "res_top4.png", class = "restaurant-image"),
                  p("Grilled marron with desert lime at Attica, Melbourne, Victoria © Attica"),
                  p("Where: 74 Glen Eira Rd, Ripponlea"),
                  HTML('<p>Helmed by internationally-acclaimed Australian chef Ben Shewry, <a href="https://www.attica.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Attica</a> has regularly graced the covetable World\'s 50 Best Restaurants list, making it one of the top dining destinations in the world. It offers a fine dining experience with an adventurous yet relaxed approach, favouring native Australian ingredients. The multi-course tasting menu highlights rare and unique ingredients such as finger limes, bunya nuts, marron and murrnong, also known as yam daisy. Each course is designed to reflect the stories and landscapes of the local region, instilling a sense of place into every dish. The drinks list is just as exciting, featuring innovative cocktails and wine from Australia and beyond.</p>'),
                  HTML('<p style="color: #FF6D00; font-weight: bold; background-color: #FFF3E0; padding: 5px; border-radius: 4px;">When to go: Reservation availability is released on the first Wednesday of each month for the following three months, so plan ahead.</p>')
              )
          )
          ,
          
          div(class = "card",
              img(src = "top_res4_small.png"),
              div(class = "card-content",
                  h4("Attica", class = "card-title"),
                  p("Innovative Cuisine • Australian Spirit", class = "card-subtitle"),
                  a("Learn more", href = "https://www.attica.com.au/", target="_blank", class = "card-link")
              )
          ),
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("FLORENTINO", class = "restaurant-name"),
                  img(src = "res_top5.png", class = "restaurant-image"),
                  p("Florentino, Melbourne, Victoria © Grossi Restaurants"),
                  p("Where: 80 Bourke St, Melbourne"),
                  HTML('<p>Since opening in 1928, <a href="https://www.florentino.com.au/florentino/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Florentino</a> has had a significant impact on the Melbourne dining scene and remains one of its most renowned restaurants. Helmed by esteemed chef and owner Guy Grossi, Florentino\'s cuisine is proud of its Italian and Australian cultures, showcasing the best locally sourced ingredients through classic Italian dishes. The menu is designed to offer a choice of three or five courses, and there\'s also a discerning wine list to match, spanning Italy, Australia and France, so you\'ll want to allow plenty of time to enjoy this fine dining experience.</p>')
              )
          ),
          
          div(class = "card",
              img(src = "top_res5_small.png"),
              div(class = "card-content",
                  h4("Florentino", class = "card-title"),
                  p("Traditional Elegance • Modern Touch", class = "card-subtitle"),
                  a("Learn more", href = "https://www.florentino.com.au/florentino/", target="_blank", class = "card-link")
              )
          ),
          
          div(style = "display: flex; justify-content: center; gap: 60px; margin-top: 20px;",
              actionButton("restaurants_prev_page", "Previous Page", class = "back-button"),
              actionButton("restaurants_back_page", "Return", class = "back-button"),
              actionButton("restaurants_next_page", "Next Page", class = "back-button"),
              actionButton("restaurants_more_page", "More", class = "back-button")
          )
      )
    }
    
    
    else if (rv$page == "coffee_shop") {
      div(class = "page-content",
          h2("MELBOURNE'S 5 BEST COFFEE SHOPS", class = "detail-title"),
          h3("Melbourne is known globally for its coffee obsession. It offers more than 2,000 cafés as well as some of the world's best baristas. Here are some of the best.", class = "detail-subtitle"),
          h3("By Sue Gough Henly", class = "detail-author"),
          p("Melbourne's love affair with coffee started with the arrival of Italian and Greek immigrants after World War II and has evolved into an art form. The city's coffee entrepreneurs trawl the world's great coffee regions in search of single origin coffees with which to tantalise their sophisticated customers. And it's not just about espressos and flat whites (similar to lattes), but also pour over, siphon and cold-drip coffee styles."),
          
          div(class = "restaurant-section",
              h3("CODE BLACK", class = "restaurant-name"),
              # Embed Instagram post using iframe
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/p/CB9FumYlLiE/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: 15-17 Weston St, Brunswick"),
              p("The back of the Code Black menu simply reads in small font:\"The Sublime and The Ridiculous\", which basically sums this place up. Sublime in the quality of its fancy stone and metal fit-out and ridiculous in the excellence of its coffee. Code Black is simultaneously a laboratory, a workshop and a hub for connoisseurs of the dark art and science of coffee. There's always a house blend for black and another for milk coffees and a rotating single origin for both black and white.")
          ),
          
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("ST ALi", class = "restaurant-name"),
                  img(src = "cof_top2.png", class = "restaurant-image"),
                  p("ST. ALi, Melbourne, Victoria © Visit Victoria"),
                  p("Where: 12-18 Yarra Pl, South Melbourne"),
                  HTML('<p><a href="https://stali.com.au/pages/st-ali-south-melbourne" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">ST ALi</a> is an entire precinct dedicated to coffee, and the engine room of renaissance coffee guy Salvatore Malatesta. With its dumpster-chic decor down a graffiti daubed laneway, ST ALi is one of Melbourne\'s main experimental coffee brewers, micro roasters, green bean traders and country of origin specialists. It is one of the best places to try subtle cold-drip coffee, with its low acidity and bitterness, which goes down a treat with a tasty breakfast or lunch.</p>')
              )
          ),
          div(class = "card",
              img(src = "cof_top2_small.png"),
              div(class = "card-content",
                  h4("ST ALi", class = "card-title"),
                  p("Innovative Brews • Premium Blends", class = "card-subtitle"),
                  a("Learn more", href = "https://stali.com.au/pages/st-ali-south-melbourne", target="_blank", class = "card-link")
              )
          ),
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("SEVEN SEEDS", class = "restaurant-name"),
                  img(src = "cof_top3.png", class = "restaurant-image"),
                  p("Seven Seeds, Carlton, Victoria © Seven Seeds"),
                  p("Where: 114 Berkeley St, Carlton"),
                  HTML('<p><a href="https://sevenseeds.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Seven Seeds</a> (which takes its name from the seven fertile seeds of coffee that the Sufi Baba Budan smuggled out of Yemen and into India during the 17th century) is a small micro-roaster in Carlton that sources seasonal coffee from around the world. It is owned by coffee visionaries Mark Dundon and Bridget Amor, the original owners of ST ALi. Its quirky on-site café still makes some of Melbourne\'s best coffee and offers a small menu of simple food.</p>')
              )
          ),
          div(class = "card",
              img(src = "cof_top3_small.png"),
              div(class = "card-content",
                  h4("Seven Seeds", class = "card-title"),
                  p("Sourcing Excellence • Game-Changing Brews", class = "card-subtitle"),
                  a("Learn more", href = "https://sevenseeds.com.au/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("INDUSTRY BEANS", class = "restaurant-name"),
                  img(src = "cof_top4.png", class = "restaurant-image"),
                  p("Industry Beans, Melbourne, Victoria © Josie Withers Photography"),
                  p("Where: 3/62 Rose St, Fitzroy"),
                  HTML('<p><a href="https://industrybeans.com/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Industry Beans</a> is housed in an open plan, award-winning warehouse conversion among Fitzroy\'s edgy street art. It is a coffee roastery, restaurant, and brew bar with an inspired seasonal brunch menu. The menu includes almost-too-pretty-to-eat dishes such as rosewater compressed watermelon with lemon myrtle panna cotta, bee pollen curd, and wattleseed granola, all designed to showcase the roastery\'s beans.</p>')
              )
          ),
          div(class = "card",
              img(src = "cof_top4_small.png"),
              div(class = "card-content",
                  h4("Industry Beans", class = "card-title"),
                  p("Innovative Coffee Craftsmanship • Unique Brunch Experience", class = "card-subtitle"),
                  a("Learn more", href = "https://industrybeans.com/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(class = "restaurant-section",
              div(class = "restaurant-content",
                  h3("DUKES COFFEE ROASTERS", class = "restaurant-name"),
                  img(src = "cof_top5.png", class = "restaurant-image"),
                  p("Dukes Coffee Roasters, Melbourne, Victoria © Visit Victoria"),
                  p("Where: 247 Flinders Lane, Melbourne"),
                  HTML('<p><a href="https://www.dukescoffee.com.au/?v=3a1ed7090bfa" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Dukes Coffee Roasters</a> is located at Ross House on Flinders Lane, Melbourne. This intimate light wood-panelled café sources the best seasonal ethically traded coffee beans from individual farms or small cooperatives worldwide. The café offers both single origin and blended beans for espressos and filter coffee, along with a variety of brewing gear for coffee enthusiasts.</p>')
              )
          ),
          div(class = "card",
              img(src = "cof_top5_small.png"),
              div(class = "card-content",
                  h4("Dukes Coffee Roasters", class = "card-title"),
                  p("Organic Coffee Craft • Sustainable Roasting", class = "card-subtitle"),
                  a("Learn more", href = "https://www.dukescoffee.com.au/?v=3a1ed7090bfa", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(style = "display: flex; justify-content: center; gap: 60px; margin-top: 20px;",
              actionButton("coffee_prev_page", "Previous Page", class = "back-button"),
              actionButton("coffee_back_page", "Return", class = "back-button"),
              actionButton("coffee_next_page", "Next Page", class = "back-button"),
              actionButton("coffee_more_page", "More", class = "back-button")
          )
      )
    }
    
    
    
    else if (rv$page == "cheap_eats") {
      tagList(
        div(class = "page-content",
            h2("5 OF MELBOURNE'S BEST CHEAP EATS", class = "detail-title"),
            h3("These cheap and cheerful Melbourne restaurants serve up quality food that won't break the bank.", class = "detail-subtitle"),
            h3("By Ellie Schneider", class = "detail-author"),
            p("Melbourne is renowned for its dining culture, and for those travelling on a budget there is plenty of good food that's also friendly on the wallet. We've scouted the streets of Melbourne to bring you a few of our favourite, fuss-free eateries."),
            
            
            div(class = "restaurant-section",
                h3("HANOI HANNAH EXPRESS LANE", class = "restaurant-name"),
                img(src = "cheap_top1.png", class = "restaurant-image"),
                p("Hanoi Hannah, Melbourne, Victoria © Josie Withers Photography"),
                p("Where: Windsor"),
                HTML('<p>Avoid the queues at Hanoi Hannah and head to its no-frills <a href="https://expresslane.hanoihannah.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">eatery next door</a>. The space is dedicated to Hanoi Hannah\'s takeaway operations, but a tiny communal table indoors lets you watch the chefs whip up their hawker-style Vietnamese fare. You can order favourites from the restaurant menu such as vermicelli salad with char grilled chicken or tender beef pho, or try something different such as pork belly banh mi or lemongrass beef stir fry.</p>'),
            ),
            div(class = "card",
                img(src = "cheap_top1_small.png"),  
                div(class = "card-content",
                    h4("Hanoi Hannah Express Lane", class = "card-title"),
                    p("Vietnamese Hawker-Style Fare • Takeaway Focused", class = "card-subtitle"),
                    a("Learn more", href = "https://expresslane.hanoihannah.com.au/", target="_blank", class = "card-link")
                )
            ),
            
            
            
            div(class = "restaurant-section",
                h3("MISS KATIE'S CRAB SHACK", class = "restaurant-name"),
                div(style = "display: flex; justify-content: center; margin-top: 20px;",
                    tags$iframe(
                      src = "https://www.instagram.com/p/B-J4vQfDz3E/embed", 
                      width = "400", 
                      height = "480", 
                      frameborder = "0", 
                      scrolling = "no", 
                      allowtransparency = "true"
                    )
                ),
                p("Where: Fitzroy"),
                HTML('<p>Having done the rounds of Melbourne and settling in Fitzroy, <a href="https://www.misskatiescrabshack.com/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Miss Katie\'s Crab Shack</a> has become the go-to spot for crustacean cravings. The roving restaurant, headed up by chef Katie Marron, offers an authentic take on southern soul food. Think pots of prawns, fried chicken and waffles, fish tacos, and a build-your-own Louisiana-style boil in Old Bay seasoning. Bibs are supplied in case things get messy.</p>')
            ),
            
            
            
            div(class = "restaurant-section",
                h3("TOKYO TINA", class = "restaurant-name"),
                img(src = "cheap_top3.png", class = "restaurant-image"),  
                p("Tokyo Tina, Windsor, Victoria © Visit Victoria"),
                p("Where: Windsor"),
                HTML('<p>One of the busiest restaurants in Windsor, don\'t be put off by the crumbling walls and peeling posters – she\'s a diamond in the rough. The menu at <a href="https://tokyotina.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Tokyo Tina</a> features favourites such as gyoza and bao, but not always as you might expect. While you might not call it fusion, the chefs here certainly take influence from different cuisines around the world – and they aren\'t afraid to try something new.</p>')
            ),
            div(class = "card",
                img(src = "cheap_top3_small.png"), 
                div(class = "card-content",
                    h4("Tokyo Tina", class = "card-title"),
                    p("Japanese Cuisine • Creative & Diverse", class = "card-subtitle"),
                    a("Learn more", href = "https://tokyotina.com.au/", target="_blank", class = "card-link")
                )
            ),
            
            
            
            
            div(class = "restaurant-section",
                h3("MIZNON", class = "restaurant-name"),
                
                div(style = "display: flex; justify-content: center; margin-top: 20px;",
                    tags$iframe(
                      src = "https://www.instagram.com/reel/C_NShCgS-_b/embed", 
                      width = "400", 
                      height = "480", 
                      frameborder = "0", 
                      scrolling = "no", 
                      allowtransparency = "true"
                    )
                ),
                p("Where: City centre"),
                HTML('<p>With Australia\'s largest Greek population, Melbourne is no stranger to a pita. But thanks to the arrival of Israeli eatery <a href="https://www.miznonaustralia.com/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Miznon</a>, the city has been introduced to a new brand of Mediterranean street food. Minon means "kiosk" in Hebrew, and this casual space is super festive and fast-paced. Most food comes served in a paper bag and pitas are stuffed with a range of different ingredients, such as lamb rib (pull the bones out and watch the flesh fall away), steak and egg, ratatouille and golden fish. Don\'t leave without trying the tatami, a coffee soaked layered biscuit cake dusted with cocoa.</p>')
            ),
            
            
            
            div(class = "restaurant-section",
                h3("8BIT", class = "restaurant-name"),
                img(src = "cheap_top5.png", class = "restaurant-image"), 
                p("8 Bit Burgers, Swanston Street, Melbourne, Victoria © Josie Withers"),
                p("Where: City centre"),
                HTML('<p>Burger-loving locals need not head far to get their fix. <a href="https://www.eat8bit.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">8bit</a>, which started out in the westside suburb of Footscray, has set up shop smack bang in the middle of the city centre on the corner of Swanston and Little Bourke streets. Order the towering Double Dragon (double beef, double cheese and double bacon) or the Golden Axe (crispy fried chicken, sriracha mayo and slaw), with a side of crispy onion rings or beer battered fries. To finish, try the salted caramel milkshake.</p>'),
            ), 
            
            div(class = "card",
                img(src = "cheap_top5_small.png"),  
                div(class = "card-content",
                    h4("8bit", class = "card-title"),
                    p("Classic Burgers • Retro Vibes", class = "card-subtitle"),
                    a("Learn more", href = "https://www.eat8bit.com.au/", target="_blank", class = "card-link")
                )
            ),
            
            div(style = "display: flex; justify-content: center; gap: 60px; margin-top: 20px;",
                actionButton("cheapeats_prev_page", "Previous Page", class = "back-button"),
                actionButton("cheapeats_back_page", "Return", class = "back-button"),
                actionButton("cheapeats_next_page", "Next Page", class = "back-button"),
                actionButton("cheapeats_more_page", "More", class = "back-button")
            )
            
        )
      )
      
    }
    
    
    
    else if (rv$page == "best_bars") {
      div(class = "page-content",
          h2("TOP ROOFTOP BARS IN MELBOURNE", class = "detail-title"),
          h3("Head to the top floor of these unique venues for some of the hottest food, coldest drinks and the best views of Melbourne.", class = "detail-subtitle"),
          h3("By Allie Metz", class = "detail-author"),
          p("Melbourne is known for its vibrant small bar scene. While you\'ll find an ample selection of moody underground haunts to explore, why not take advantage of the fresh air and skyline vistas on offer at these chic rooftop venues.")
          ,
          
          
          div(class = "restaurant-section",
              h3("GOOD HEAVENS", class = "restaurant-name"),
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/reel/C_pauV2zy4d/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: level 2/79 Bourke St, Melbourne"),
              HTML('<p>It\'s Palm Beach meets southern BBQ at <a href="https://www.goodheavens.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Good Heavens</a>, where the 80s inspired drinks are brightly-hued and the snacks come straight from the smoker at Fancy Hank\'s downstairs. The drinks menu also features a range of craft brews and an all-Aussie wine list, playing the perfect companion to the indulgent burgers, brisket and cheesy sides being served up.</p>')
          )
          ,
          
          
          
          
          div(class = "restaurant-section",
              h3("ROOFTOP BAR", class = "restaurant-name"),
              img(src = "rooftop_bar1.png", class = "restaurant-image"),  
              p("Rooftop Bar at Curtin House, Melbourne, Victoria © Visit Victoria"),
              p("Where: Curtin House, level 7/252 Swanston St, Melbourne"),
              HTML('<p style="color: gray;">Don\'t let the seven flights of stairs stand in your way (the elevator works sometimes), as this is easily one of Melbourne\'s best known and most loved rooftop bars. Located at <a href="https://curtinhouse.com/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Curtin House</a> on Swanston Street, right in the middle of the city, you\'ll get unbeatable views of Melbourne\'s skyline as you sit back in one of the deck chairs. The Rooftop Burger Shack will keep you fed and in summer the venue plays host to an outdoor cinema.</p>'),
              HTML('<p style="color: #FF6D00; font-weight: bold; background-color: #FFF3E0; padding: 5px; border-radius: 4px;">Top tip: Be sure to book ahead for the <a href="https://rooftopcinema.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Rooftop Cinema</a> (operating weekends from December to March), as tickets are limited.</p>')
              
          ),
          div(class = "card",
              img(src = "rooftop_bar1_small.png"), 
              div(class = "card-content",
                  h4("Rooftop Bar", class = "card-title"),
                  p("Skyline Vistas • Open-Air Cinema", class = "card-subtitle"),
                  a("Learn more", href = "https://curtinhouse.com/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(class = "restaurant-section",
              h3("LOOP ROOF", class = "restaurant-name"),
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/p/C6cXAp3yTfJ/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: 3/23 Meyers Pl, Melbourne"),
              HTML('<p style="color: gray;">Fully equipped to keep you comfortable in the fickle Melbourne weather (think retractable awnings, misters and radiant heating), <a href="https://www.looprooftopbar.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Loop Roof</a> is a city centre escape where you\'ll be delighted by the funky décor, 90s hip hop and quirky cocktails. The food is top notch as well, evidenced by the snaking line up out the door. Get there early to ensure you don\'t get stuck waiting.</p>')
          ),
          
          
          
          
          div(class = "restaurant-section",
              h3("THE ROOFTOP AT QT HOTEL", class = "restaurant-name"),
              img(src = "rooftop_bar3.png", class = "restaurant-image"),  
              p("The Rooftop at QT Hotel, Melbourne, Victoria © Tourism Australia"),
              p("Where: 133 Russell St, Melbourne"),
              HTML('<p style="color: gray;">Whether or not you\'ve made this funky hotel your home while in Melbourne, <a href="https://www.qthotels.com/melbourne/eat-drink/rooftop-at-qt/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">The Rooftop bar at the QT Hotel</a> is certainly worth a visit. The mid-century décor is as elegant as the cocktails and the indoor/outdoor split makes it a perfect spot to lounge any time of year. Head up there for drinks, snacks and incredible views of Melbourne\'s glittering skyline.</p>')
          ),
          
          div(class = "card",
              img(src = "rooftop_bar3_small.png"), 
              div(class = "card-content",
                  h4("The Rooftop at QT Hotel", class = "card-title"),
                  p("Mid-Century Elegance • Perfect Lounge Spot", class = "card-subtitle"),
                  a("Learn more", href = "https://www.qthotels.com/melbourne/eat-drink/rooftop-at-qt/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(class = "restaurant-section",
              h3("THE EMERSON ROOFTOP", class = "restaurant-name"),
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/p/C79DTsVPYd5/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: 141-145 Commercial Rd, South Yarra"),
              HTML('<p>With a retractable roof and ample heating, <a href="https://theemerson.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">The Emerson Rooftop</a> in South Yarra is perfect no matter the weather. Stop here for everything from a refreshing drink in the afternoon sun (grab a day bed and settle in) through to a great casual dinner. If you end up staying on, head downstairs and hit the dance floor for the perfect way to finish off your night.</p>')
          ),
          
          
          
          div(style = "display: flex; justify-content: center; gap: 60px; margin-top: 20px;",
              actionButton("bestbars_prev_page", "Previous page", class = "back-button"),
              actionButton("bestbars_back_page", "Return", class = "back-button"),
              actionButton("bestbars_next_page", "Next page", class = "back-button"),
              actionButton("bestbars_more_page", "More", class = "back-button")
          )
          
      )
    }
    
    
    
    else if (rv$page == "breakfast_brunch") {
      div(class = "page-content",
          h2("MELBOURNE'S BEST BREAKFAST AND BRUNCH", class = "detail-title"),
          h3("Make the most important meal of the day the best one at these top-notch Melbourne cafés.", class = "detail-subtitle"),
          h3("By Ellie Schneider", class = "detail-author"),
          p("It's a city renowned for its café culture and Melburnians are spoilt for choice when it comes to their morning meal. Here are a few Melbourne breakfast and brunch spots that are worth getting out of bed for.")
          ,
          
          
          div(class = "restaurant-section",
              h3("THE KETTLE BLACK", class = "restaurant-name"),
              img(src = "break_top1.png", class = "restaurant-image"),  
              p("The Kettle Black, Melbourne, Victoria © Visit Victoria"),
              p("Where: 50 Albert Rd, South Melbourne"),
              HTML('<p>This could quite possibly be the most photographed café in Melbourne. Housed within a converted Victorian terrace on Albert Road, <a href="https://thekettleblack.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">The Kettle Black</a> serves sophisticated breakfast fare almost too pretty to eat. Think seasonal ingredients integrated perfectly into both sweet and savoury dishes.</p>')
          ),
          div(class = "card",
              img(src = "break_top1_small.png"), 
              div(class = "card-content",
                  h4("The Kettle Black", class = "card-title"),
                  p("Distinctive Design • Gourmet Breakfast Experience", class = "card-subtitle"),
                  a("Learn more", href = "https://thekettleblack.com.au/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          div(class = "restaurant-section",
              h3("TWO BIRDS ONE STONE", class = "restaurant-name"),
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/reel/DA7TNthxOhN/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: 12 Claremont St, South Yarra"),
              HTML('<p>Just behind Chapel Street in South Yarra, <a href="https://www.twobirdsonestonecafe.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Two Birds One Stone</a> has become a popular port of call for hungry Melburnians. Its bright, airy space and cosy booth seating mean you could easily hang around all day. And with food this impressive, you may just want to. Your caffeine hit is available in a variety of forms: espresso, Aeropress, siphon, pour-over and cold drip.</p>')
          ),
          
          
          
          div(class = "restaurant-section",
              h3("FEAST OF MERIT", class = "restaurant-name"),
              img(src = "break_top3.png", class = "restaurant-image"),
              p("Where: 117 Swan St, Richmond"),
              HTML('<p>The exterior of <a href="https://feastofmerit.com/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Feast of Merit</a> is splashed with vibrant murals, and inside you\'ll find a menu that keeps the excitement rolling. Dishes here are inspired by Middle Eastern spices, so you\'ll find dishes infused with the flavours of sumac, za\'atar, smoky paprika, and more. With bright interior gardens, this is one café that will transport the tastebuds.</p>')
          ),
          
          div(class = "card",
              img(src = "break_top3_small.png"), 
              div(class = "card-content",
                  h4("Feast of Merit", class = "card-title"),
                  p("Colorful Murals • Middle Eastern Flavors", class = "card-subtitle"),
                  a("Learn more", href = "https://feastofmerit.com/", target="_blank", class = "card-link")
              )
          ),
          
          
          
          
          
          div(class = "restaurant-section",
              h3("TOP PADDOCK", class = "restaurant-name"),
              div(style = "display: flex; justify-content: center; margin-top: 20px;",
                  tags$iframe(
                    src = "https://www.instagram.com/reel/Cf8XAhvj8uX/embed", 
                    width = "400", 
                    height = "480", 
                    frameborder = "0", 
                    scrolling = "no", 
                    allowtransparency = "true"
                  )
              ),
              p("Where: 658 Church St, Richmond"),
              HTML('<p>Sitting down to breakfast at <a href="https://darlinggroup.com.au/top-paddock/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Top Paddock</a> has become a Melbourne must, so be prepared to wait. Do so and you\'ll be treated to classic dishes like eggs benedict as well as surprising newcomers like a breakfast salad. Coffee in hand and surrounded by greenery, timber and glass, a trip to Top Paddock will be a very Melbourne morning indeed.</p>')
          ),
          
          
          div(class = "restaurant-section",
              h3("AUCTION ROOMS", class = "restaurant-name"),
              img(src = "break_top5.png", class = "restaurant-image"),  
              p("Auction Rooms Cafe, Melbourne, Victoria © Visit Victoria"),
              p("Where: 103-107 Errol St, North Melbourne"),
              HTML('<p>The old WB Ellis auction house on Errol Street provides the bones for one of Melbourne\'s most lauded cafés, <a href="https://www.auctionroomscafe.com.au/" target="_blank" style="text-decoration: none; border-bottom: 2px solid green;">Auction Rooms</a>. It has garnered a reputation for its cool fit-out and creative breakfast fare. If that\'s not enough, the coffee is impeccable, made from beans that come from its own roastery down the road.</p>')
          ),
          
          div(class = "card",
              img(src = "break_top5_small.png"),  
              div(class = "card-content",
                  h4("Auction Rooms", class = "card-title"),
                  p("Iconic Setting • Creative Breakfast Fare", class = "card-subtitle"),
                  a("Learn more", href = "https://www.auctionroomscafe.com.au/", target="_blank", class = "card-link")
              )
          ),
          
          
          div(style = "display: flex; justify-content: center; gap: 60px; margin-top: 20px;",
              actionButton("brunch_prev_page", "Previous page", class = "back-button"),
              actionButton("brunch_back_page", "Return", class = "back-button"),
              actionButton("brunch_next_page", "Next page", class = "back-button"),
              actionButton("brunch_more_page", "More", class = "back-button")
          )
          
      )
    }
    
    
    
    
    
    else if (rv$page == "more") {
      tagList(
        div(class = "more-page-container",
            
            div(class = "more-page-content",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type_filter", "Establishment Type (Select one, or none for all):", 
                                 choices = c("All", unique(combined_data$Type)), selected = "All"),
                    selectInput("cuisine_filter", "Restaurant Cuisine Type:", 
                                choices = c("All", unique_cuisines), selected = "All", multiple = FALSE),
                    selectInput("items_per_page", "Show entries:", choices = c(5, 10, 15, 20), selected = 5),
                    textInput("search", "Search:", ""),
                    numericInput("page_input", "Go to page:", value = 1, min = 1, step = 1),
                    tags$p("Note: Data loading might take some time. You can select only one type at a time. If no type is selected, all will be displayed.", class = "more-note")
                  ),
                  mainPanel(
                    uiOutput("restaurant_cards")
                  )
                )
            ),
            
            div(class = "more-page-footer",
                actionButton("return_page", "Return", class = "btn custom-btn btn-return"),
                uiOutput("pagination_controls")
            )
        )
      )
    }
    
    
  })
  
  
  
  output$carousel <- renderSlickR({
    slides <- list(
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'restaurants', {priority: 'event'})",
               tags$img(src = "Restaurant.png"),
               tags$div(class = "slide-text", "MELBOURNE'S BEST RESTAURANTS")
      ),
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'coffee_shop', {priority: 'event'})",
               tags$img(src = "coffee_shop.png"),
               tags$div(class = "slide-text", "TOP COFFEE SHOPS")
      ),
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'cheap_eats', {priority: 'event'})",
               tags$img(src = "cheap_food.png"),
               tags$div(class = "slide-text", "CHEAP EATS IN MELBOURNE")
      ),
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'best_bars', {priority: 'event'})",
               tags$img(src = "bar.png"),
               tags$div(class = "slide-text", "BEST ROOFTOP BARS")
      ),
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'breakfast_brunch', {priority: 'event'})",
               tags$img(src = "brunch.png"),
               tags$div(class = "slide-text", "BREAKFAST AND BRUNCH SPOTS")
      ),
      tags$div(class = "slide-container",
               onclick = "Shiny.setInputValue('page', 'more', {priority: 'event'})",
               tags$img(src = "more.png"),
               tags$div(class = "slide-text", "MORE CONTENT")
      )
    )
    
    slickR(slides) + 
      settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 1000)
  })

  

  
  # Initialize reactive value to store clicked locations
  clicked_locations <- reactiveVal(list())
  enable_click_mode <- reactiveVal(FALSE)  # Track point generation mode
  
  # Filtered data based on building type selection
  filtered_datas <- reactive({
    if (length(input$buildingType) == 0) {
      return(all_data[FALSE, ])
    } else {
      all_data %>% filter(Type %in% input$buildingType)
    }
  })
  
  # Reactive value to store the search result coordinates and types
  search_result <- reactiveVal(NULL)
  search_types <- reactiveVal(NULL)
  
  # Observe the search button and update the dropdown for multiple matches
  observeEvent(input$search, {
    search_name <- input$searchPlace
    if (search_name != "") {
      matched_rows <- all_data %>% filter(grepl(search_name, Name, ignore.case = TRUE))
      
      if (nrow(matched_rows) > 1) {
        output$matchSelection <- renderUI({
          selectInput("selectedMatch", "Select a matching place:", choices = matched_rows$Name)
        })
      } else if (nrow(matched_rows) == 1) {
        search_result(list(lat = matched_rows$Latitude, lng = matched_rows$Longitude))
        search_types(unique(matched_rows$Type))
        
        leafletProxy("map") %>%
          setView(lng = matched_rows$Longitude, lat = matched_rows$Latitude, zoom = 15) %>%
          clearGroup("search") %>%
          addCircles(lng = matched_rows$Longitude, lat = matched_rows$Latitude, 
                     color = "red", radius = 50, group = "search")
      } else {
        output$matchSelection <- renderUI({ NULL })
        search_types(NULL)
      }
    }
  })
  
  # Observe when user selects a match from the dropdown
  observeEvent(input$selectedMatch, {
    selected_place <- input$selectedMatch
    if (!is.null(selected_place) && selected_place != "") {
      matched_row <- all_data %>% filter(Name == selected_place)
      if (nrow(matched_row) > 0) {
        search_result(list(lat = matched_row$Latitude, lng = matched_row$Longitude))
        search_types(unique(matched_row$Type))
        
        leafletProxy("map") %>%
          setView(lng = matched_row$Longitude, lat = matched_row$Latitude, zoom = 15) %>%
          clearGroup("search") %>%
          addCircles(lng = matched_row$Longitude, lat = matched_row$Latitude, 
                     color = "red", radius = 50, group = "search")
      }
    }
  })
  
  # Observe the reset button to reset the map view and clear search highlights
  observeEvent(input$reset, {
    # Clear the search input box
    updateTextInput(session, "searchPlace", value = "")
    
    # Clear map related variables
    search_result(NULL)
    search_types(NULL)
    output$matchSelection <- renderUI({ NULL })
    
    # Reset Selections
    updatePickerInput(session, "buildingType", selected = character(0)) 
    updatePickerInput(session, "tramRoute", selected = character(0))    
    
    # Reset the map view to the default position and zoom
    leafletProxy("map") %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
      clearGroup("search") %>%    
      clearMarkers() %>%           
      clearShapes()                
    
    # Hide information panel
    output$placeDetails <- renderUI({ NULL })
    hide("info-panel")
    
  })
  
  
  
  
  
  # Render the leaflet map with Mapbox style
  observeEvent(input$tabs, {
    if (input$tabs == "Map") {
  output$map <- renderLeaflet({
    leaflet(all_data) %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri WorldStreetMap") %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap HOT") %>%
      addTiles(urlTemplate = mapboxStyleURL, group = "Mapbox Custom") %>%
      
      # Set the view to be displayed initially
      setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
      
      # Add a layer control and make sure "OpenStreetMap HOT" is the default basemap
      addLayersControl(
        baseGroups = c("Esri WorldStreetMap", "OpenStreetMap HOT", "Mapbox Custom"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  }
  })
  
  # Clear selection and reset map
  observeEvent(input$clear_selection, {
    clicked_locations(list())  
    enable_click_mode(FALSE)  
    
    leafletProxy("map") %>%
      clearMarkers()  # Clear the map marker
  })
  
  # Observe filtered data and update map markers with custom icons
  observe({
    current_types <- search_types()
    
    map_data <- if (!is.null(current_types)) {
      all_data %>% filter(Type %in% current_types)
    } else {
      filtered_datas()
    }
    
    proxy <- leafletProxy("map", data = map_data) %>%
      clearMarkers() %>%
      clearGroup("cluster")
    
    if (nrow(map_data) > 0) {
      proxy <- proxy %>%
        addMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~icons(
            iconUrl = Icon,
            iconWidth = 30, iconHeight = 30,
            iconAnchorX = 15, iconAnchorY = 15
          ),
          layerId = ~Name,
          group = "cluster",
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 17)
        )
    }
    
    proxy %>%
      clearTiles() %>%
      addTiles(urlTemplate = mapboxStyleURL, group = "Custom Map") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri World Street Map") %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap HOT") %>%
      addLayersControl(
        baseGroups = c("Esri World Street Map", "Custom Map", "OpenStreetMap HOT"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Observe the "Enable Point Generation" button click event
  observeEvent(input$enable_click_mode, {
    enable_click_mode(TRUE)  # Enable point generation mode
    
    # Show modal to inform the user to click two points
    showModal(modalDialog(
      title = "Point Generation Mode",
      "Please click on two locations on the map to generate points for navigation.",
      easyClose = TRUE
    ))
  })
  
  # General click processing function, processing the logic of generating points
  handle_map_click <- function(lat, lng, popup_text, type = "map_click") {
    if (enable_click_mode()) {  # Only processed when point generation mode is enabled
      if (length(clicked_locations()) < 2) { # Limit to two click locations
        clicked_locations(c(clicked_locations(), list(c(lat = lat, lng = lng))))
        
        # Add markers to the map
        leafletProxy("map") %>%
          addMarkers(lng = lng, lat = lat, popup = popup_text)
        
        # If two points are selected, a navigation confirmation box pops up
        if (length(clicked_locations()) == 2) {
          showModal(modalDialog(
            title = "Navigate to Google Maps?",
            "You have selected two locations. Do you want to navigate using Google Maps?",
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_navigation", "Navigate")
            )
          ))
        }
      }
    }
  }
  
  # Listen for blank map click events
  observeEvent(input$map_click, {
    handle_map_click(input$map_click$lat, input$map_click$lng, paste("Map Click: ", input$map_click$lat, ", ", input$map_click$lng), "Map Click")
  })
  
  # Listen for map marker click events and generate points on the markers
  observeEvent(input$map_marker_click, {
    # Generate a point using the marker's latitude and longitude
    handle_map_click(input$map_marker_click$lat, input$map_marker_click$lng, paste("Marker Click: ", input$map_marker_click$lat, ", ", input$map_marker_click$lng), "Marker Click")
  })
  
  # Listen for the "Navigate to Google Maps" confirmation button click event
  observeEvent(input$confirm_navigation, {
    locations <- clicked_locations()
    if (length(locations) == 2) {  # Make sure two locations are clicked
      origin <- paste(locations[[1]][["lat"]], locations[[1]][["lng"]], sep = ",")
      destination <- paste(locations[[2]][["lat"]], locations[[2]][["lng"]], sep = ",")
      navigation_url <- paste0("https://www.google.com/maps/dir/?api=1&origin=", origin, "&destination=", destination)
      
      # Open the navigation in a new window and remove the modal
      browseURL(navigation_url)
      removeModal()  
      
      # Clear the selected position
      clicked_locations(list())
    }
  })
  
  
  # Clear the selection and remove markers
  observeEvent(input$clear_selection, {
    clicked_locations(list())  # Clear stored clicked locations
    enable_click_mode(FALSE)  # Disable point generation mode
    
    # Clear markers from the map
    leafletProxy("map") %>%
      clearMarkers()
  })
  
  # Observe map_marker_click event for clicking on markers
  observeEvent(input$map_marker_click, {
    if (!enable_click_mode()) {  
      click <- input$map_marker_click
      if (!is.null(click$id)) {
        selected_row <- all_data %>% filter(Name == click$id)
        if (nrow(selected_row) > 0) {
          output$placeDetails <- renderUI({
            HTML(paste0(
              "<h4>", selected_row$Name, "</h4>",
              "<p><strong>Type:</strong> ", selected_row$Type, "</p>",
              "<p><strong>About:</strong> ", selected_row$About, "</p>",
              "<p><strong>Price:</strong> ", selected_row$Price, "</p>",
              "<p><strong>Website:</strong> <a href='", selected_row$Website, "' target='_blank'>",
              selected_row$Website, "</a></p>",
              "<p><strong>Open Time:</strong> ", selected_row$OpenTime, "</p>",
              "<img src='", selected_row$Image, "' width='100%'>"
            ))
          })
          show("info-panel")
        }
      }
    }
  })
  
  observe({
    # Make sure input$tramRoute is processed
    if (is.null(input$tramRoute) || length(input$tramRoute) == 0) {
      # When no route is selected, clear the marker on the map
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearGroup("polylines")
      
    } else {
      # When a route is selected, filter and display the corresponding stations
      selected_route_data <- tram_stops %>%
        filter(ROUTEUSSP %in% input$tramRoute)
      
      # Use the nearest neighbor algorithm to sort sites by distance
      ordered_route_data <- selected_route_data[1, , drop = FALSE] 
      remaining_stops <- selected_route_data[-1, , drop = FALSE]   
      max_distance_threshold <- 500
      
      while (nrow(remaining_stops) > 0) {
        last_stop <- ordered_route_data[nrow(ordered_route_data), ]
        
        # Calculate the distance from the current point to all other remaining points
        distances <- distm(c(last_stop$LONGITUDE, last_stop$LATITUDE), 
                           cbind(remaining_stops$LONGITUDE, remaining_stops$LATITUDE))
        
        # Find the point with the smallest distance
        nearest_idx <- which.min(distances)
        
        # If the distance of the nearest point exceeds the threshold, we still need to connect the point to ensure that all points are connected
        if (distances[nearest_idx] > max_distance_threshold) {
        }
        
        ordered_route_data <- rbind(ordered_route_data, remaining_stops[nearest_idx, ])
        remaining_stops <- remaining_stops[-nearest_idx, ]
      }
      
      
      # Show site on map
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearGroup("polylines") %>%
        addCircleMarkers(
          data = selected_route_data,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 5,
          color = "green",
          label = ~STOP_NAME,
          popup = ~paste("Stop ID:", STOP_ID, "<br>",
                         "Stop Name:", STOP_NAME, "<br>",
                         "Zone:", TICKETZONE, "<br>",
                         "Routes:", ROUTEUSSP)
        ) %>%
        # Add route lines and connect stations
        addPolylines(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          data = ordered_route_data,
          color = "blue", # Change line color as needed
          weight = 4,
          group = "polylines"
        )
    }
  })
  observeEvent(input$show_free_tram_zone, {
    # Convert `free_tram_zone_stops` to sf object
    free_tram_zone_sf <- st_as_sf(free_tram_zone_stops, coords = c("LONGITUDE", "LATITUDE"), crs = 3111)  
    free_tram_zone_sf <- st_transform(free_tram_zone_sf, crs = 4326)  
    
    # Calculate the convex hull, the minimum closed area
    convex_hull <- st_convex_hull(st_union(free_tram_zone_sf))
    
    # Display the stations and convex hull of the Free Tram Zone
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = free_tram_zone_stops,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 3,
        color = "blue",
        label = ~STOP_NAME,
        popup = ~paste("Stop ID:", STOP_ID, "<br>",
                       "Stop Name:", STOP_NAME, "<br>",
                       "Zone:", TICKETZONE)
      ) %>%
      # Add a convex hull polygon to connect all sites to form a closed area
      addPolygons(
        data = convex_hull,
        color = "red",
        weight = 2,
        fillColor = "blue",
        fillOpacity = 0.2
      )
  })
  
  # Listen for the "Cancel" button and clear the display of the Free Tram Zone
  observeEvent(input$hide_free_tram_zone, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
  })
  
  
  # Use input$map_click to detect clicks on the map and hide info-panel when clicking outside markers
  observeEvent(input$map_click, {
    if (!enable_click_mode()) {
      hide("info-panel")
    }
  })
  
  # Get and display real-time weather information
  get_weather_info <- function() {
    url <- paste0("https://api.openweathermap.org/data/2.5/weather?lat=-37.8136&lon=144.9631&appid=", api_key)
    response <- httr::GET(url)
    
    if (httr::status_code(response) == 200) {
      weather_data <- httr::content(response, "parsed")
      return(weather_data)
    } else {
      message <- paste("API request failed with status code:", httr::status_code(response))
      print(message)
      return(NULL)
    }
  }
  
  output$current_weather <- renderUI({
    weather_data <- get_weather_info()
    
    if (is.null(weather_data)) {
      HTML("Unable to retrieve real-time weather information.<br>Please try again later.")
    } else {
      weather_type <- weather_data$weather[[1]]$main
      weather_description <- weather_data$weather[[1]]$description
      visibility <- weather_data$visibility
      temperature <- weather_data$main$temp - 273.15
      feels_like <- weather_data$main$feels_like - 273.15
      humidity <- weather_data$main$humidity
      wind_speed <- weather_data$wind$speed
      
      weather_info <- paste(
        "Location: Melbourne",
        paste("Weather Type:", weather_type),
        paste("Description:", weather_description),
        paste("Temperature:", round(temperature, 1), "°C"),
        paste("Feels Like:", round(feels_like, 1), "°C"),
        paste("Humidity:", humidity, "%"),
        paste("Visibility:", visibility, "m"),
        paste("Wind Speed:", wind_speed, "m/s"),
        sep = "<br>"
      )
      HTML(weather_info)
    }
  })
  
  
  
  # Current page for "Must to do"
  currentPage <- reactiveVal(1)
  
  # Total pages for "Must to do"
  totalPages <- ceiling(nrow(data)/9)
  
  # Pagination controls for "Must to do"
  observeEvent(input$next_page, {
    if (currentPage() < totalPages) {
      currentPage(currentPage() + 1)
    }
  })
  
  observeEvent(input$prev_page, {
    if (currentPage() > 1) {
      currentPage(currentPage() - 1)
    }
  })
  
  # Display current page info for "Must to do"
  output$page_info <- renderText({
    paste("Page", currentPage(), "of", totalPages)
  })
  
  # Render attractions for "Must to do"
  output$attractions <- renderUI({
    page <- currentPage()
    startIndex <- (page - 1) * 9 + 1
    endIndex <- min(page * 9, nrow(data))
    attractionsToShow <- data[startIndex:endIndex, ]

    fluidRow(
      lapply(1:nrow(attractionsToShow), function(i) {
        
        # URL
        url <- attractionsToShow$Google.Maps.URL[i]
        
        column(
          width = 4, 
          div(
            class = "card", 
            style = "margin-bottom: 20px;",
            
            # Image container
            div(
              style = "width: 100%; height: 300px; overflow: hidden;",
              img(
                src = attractionsToShow$Featured.Image[i],
                alt = attractionsToShow$Name[i],
                style = "width: 100%; height: 100%; object-fit: cover;"
              )
            ),
            
            # Title and rating
            div(
              h4(attractionsToShow$Name[i], style = "margin: 10px 0;"),
              p(
                paste("Rating:", ifelse(!is.na(attractionsToShow$Average.Rating[i]) && attractionsToShow$Average.Rating[i] != "", 
                                        attractionsToShow$Average.Rating[i], "No rating available"))
              ),
              
              # Description
              p(
                attractionsToShow$Description[i], 
                style = "color: #777;"
              ),
              
              # View Details button
              tags$a(
                href = url,
                target = "_blank",
                class = "btn btn-success",
                style = "color: white;",
                "View Details"
              )
            ),
            style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;"
          )
        )
      })
    )
  })
  
  # Current page for "Things to do"
  thingsCurrentPage <- reactiveVal(1)
  
  # Total pages for "Things to do"
  thingsTotalPages <- ceiling(nrow(data2)/10)
  
  # Pagination controls for "Things to do"
  observeEvent(input$things_next_page, {
    if (thingsCurrentPage() < thingsTotalPages) {
      thingsCurrentPage(thingsCurrentPage() + 1)
    }
  })
  
  observeEvent(input$things_prev_page, {
    if (thingsCurrentPage() > 1) {
      thingsCurrentPage(thingsCurrentPage() - 1)
    }
  })
  
  # Display current page info for "Things to do"
  output$things_page_info <- renderText({
    paste("Page", thingsCurrentPage(), "of", thingsTotalPages)
  })
  
  # Render attractions for "Things to do"
  output$things_to_do_attractions <- renderUI({
    page <- thingsCurrentPage()
    startIndex <- (page - 1) * 10 + 1
    endIndex <- min(page * 10, nrow(data2))
    attractionsToShow <- data2[startIndex:endIndex, ]
    
    # Create card layout for each attraction
    attraction_ui_list <- lapply(1:nrow(attractionsToShow), function(i) {
      attraction <- attractionsToShow[i, ]
      
      tags$div(
        class = "card card-horizontal",
        tags$div(
          class = "card-img",
          img(
            src = attraction$Featured.Image,
            alt = attraction$Name
          )
        ),
        tags$div(
          class = "card-body",
          h4(class = "card-title", attraction$Name),
          p(class = "card-text", attraction$Description),
          p(class = "card-text", strong("Address: "), attraction$Fulladdress),
          tags$a(
            href = attraction$Google.Maps.URL,
            target = "_blank",
            class = "btn btn-primary",
            "View Details"
          )
        )
      )
    })
    
    # Return UI elements
    do.call(tagList, attraction_ui_list)
  })
  
  # Initialize review indices
  reviewIndexMust <- reactiveVal(1)
  reviewIndexThings <- reactiveVal(1)
  totalReviews <- nrow(reviews)
  
  # Next button for "Must to do" reviews
  observeEvent(input$next_review_must, {
    newIndex <- reviewIndexMust() + 1
    if (newIndex > totalReviews) {
      newIndex <- 1
    }
    reviewIndexMust(newIndex)
  })
  
  # Render reviews for "Must to do"
  output$review_must <- renderUI({
    index <- reviewIndexMust()
    review <- reviews[index, ]
    
    tags$div(
      style = "margin-bottom: 20px;",
      img(src = review$Author.Image, style = "width: 50px; height: 50px; border-radius: 50%;"),
      h4(review$Author),
      p(strong("Rating: "), review$Review.Rating),
      p(strong("Date: "), review$Date),
      div(
        style = "height: 200px; overflow: auto;",
        p(review$Review.Text)
      ),
      tags$a(
        href = review$Review.URL,
        target = "_blank",
        "View Full Review"
      )
    )
  })
  
  # Next button for "Things to do" reviews
  observeEvent(input$next_review_things, {
    newIndex <- reviewIndexThings() + 1
    if (newIndex > totalReviews) {
      newIndex <- 1
    }
    reviewIndexThings(newIndex)
  })
  
  # Render reviews for "Things to do"
  output$review_things <- renderUI({
    index <- reviewIndexThings()
    review <- reviews[index, ]
    
    tags$div(
      style = "margin-bottom: 20px;",
      img(src = review$Author.Image, style = "width: 50px; height: 50px; border-radius: 50%;"),
      h4(review$Author),
      p(strong("Rating: "), review$Review.Rating),
      p(strong("Date: "), review$Date),
      div(
        style = "height: 200px; overflow: auto;",
        p(review$Review.Text)
      ),
      tags$a(
        href = review$Review.URL,
        target = "_blank",
        "View Full Review"
      )
    )
  })
  
  # -----------------------------------
  # Entertainment pagination and filtering
  
  # Reactive expression for filtered entertainment data
  filtered_entertainment_data <- reactive({
    if (is.null(input$entertainment_categories) || length(input$entertainment_categories) == 0) {
      # Return empty data frame if no categories are selected
      entertainment_data[0, ]
    } else {
      # Create a regex pattern from selected categories
      selected_categories <- input$entertainment_categories
      pattern <- paste(selected_categories, collapse = "|")
      
      # Filter data where 'Categories' contains any of the selected categories
      entertainment_data[grepl(pattern, entertainment_data$Categories, ignore.case = TRUE), ]
    }
  })
  
  # Reactive value for total pages based on filtered data
  entertainmentTotalPages <- reactive({
    total <- ceiling(nrow(filtered_entertainment_data()) / 12)
    if (total == 0) total <- 1
    total
  })
  
  # Current page for entertainment
  entertainmentCurrentPage <- reactiveVal(1)
  
  # Observe changes in filtered data to reset current page
  observeEvent(filtered_entertainment_data(), {
    entertainmentCurrentPage(1)
  })
  
  # Pagination controls for entertainment
  observeEvent(input$entertainment_next_page, {
    if (entertainmentCurrentPage() < entertainmentTotalPages()) {
      entertainmentCurrentPage(entertainmentCurrentPage() + 1)
    }
  })
  
  observeEvent(input$entertainment_prev_page, {
    if (entertainmentCurrentPage() > 1) {
      entertainmentCurrentPage(entertainmentCurrentPage() - 1)
    }
  })
  
  # Display current page info for entertainment
  output$entertainment_page_info <- renderText({
    paste("Page", entertainmentCurrentPage(), "of", entertainmentTotalPages())
  })
  
  # Render entertainment facilities
  output$entertainment_facilities <- renderUI({
    page <- entertainmentCurrentPage()
    filtered_data <- filtered_entertainment_data()
    startIndex <- (page - 1) * 12 + 1
    endIndex <- min(page * 12, nrow(filtered_data))
    facilitiesToShow <- filtered_data[startIndex:endIndex, ]
    
    # Check if there are facilities to show
    if (nrow(facilitiesToShow) == 0) {
      return(h4("No entertainment facilities found for the selected categories."))
    }
    
    # Create card layout for each facility
    facility_ui_list <- lapply(1:nrow(facilitiesToShow), function(i) {
      facility <- facilitiesToShow[i, ]
      
      tags$div(
        class = "entertainment-card",
        tags$div(
          class = "card-img",
          img(
            src = facility$Featured.Image,
            alt = facility$Name
          )
        ),
        tags$div(
          class = "card-body",
          h4(class = "card-title", facility$Name),
          p(class = "card-text", strong("Address: "), facility$Fulladdress),
          p(class = "card-text", strong("Category: "), facility$Categories),
          p(class = "card-text", strong("Phone: "), facility$Phone),
          p(class = "card-text", strong("Rating: "), facility$Average.Rating),
          tags$a(
            href = facility$Google.Maps.URL,
            target = "_blank",
            class = "btn btn-primary",
            "View Details"
          )
        )
      )
    })
    
    # Return UI elements
    do.call(tagList, facility_ui_list)
  })
  }


shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
