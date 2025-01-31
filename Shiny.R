library(shiny)
library(shinythemes)
library(dplyr)
library(sf)
library(rnaturalearth)
library(leaflet)
library(plotly)
library(DT)
library(RColorBrewer)
library(colorspace)
library(viridis)
library(ggplot2)
library(readr)        
library(ggalluvial)  


country_mapping <- c(
  "UKR" = "Ukraine",
  "KGZ" = "Kyrgyzstan",
  "NGA" = "Nigeria",
  "BLR" = "Belarus",
  "MEX" = "Mexico",
  "USA" = "United States of America",
  "SLE" = "Sierra Leone",
  "KEN" = "Kenya",
  "PHL" = "Philippines",
  "TKM" = "Turkmenistan",
  "MMR" = "Myanmar",
  "UZB" = "Uzbekistan",
  "BGD" = "Bangladesh",
  "ETH" = "Ethiopia",
  "IDN" = "Indonesia",
  "LKA" = "Sri Lanka",
  "KHM" = "Cambodia",
  "CHN" = "China",
  "KAZ" = "Kazakhstan",
  "GTM" = "Guatemala",
  "BGR" = "Bulgaria",
  "HND" = "Honduras",
  "JAM" = "Jamaica",
  "THA" = "Thailand",
  "HTI" = "Haiti",
  "TJK" = "Tajikistan",
  "GHA" = "Ghana",
  "BDI" = "Burundi",
  "COL" = "Colombia",
  "NPL" = "Nepal",
  "SLV" = "El Salvador",
  "VNM" = "Vietnam",
  "ERI" = "Eritrea",
  "IND" = "India",
  "UGA" = "Uganda",
  "SDN" = "Sudan",
  "CIV" = "Ivory Coast",
  "KOR" = "South Korea",
  "ROU" = "Romania",
  "SOM" = "Somalia",
  "AFG" = "Afghanistan",
  "BOL" = "Bolivia",
  "BRA" = "Brazil",
  "MDA" = "Moldova",
  "RUS" = "Russia",
  "DOM" = "Dominican Republic",
  "VEN" = "Venezuela",
  "HUN" = "Hungary",
  "SRB" = "Serbia",
  "ALB" = "Albania",
  "LAO" = "Laos",
  "COD" = "Congo (Kinshasa)",
  "NER" = "Niger",
  "MDG" = "Madagascar"
)




world <- ne_countries(scale = "medium", returnclass = "sf")

user_data <- read_csv("CTDC_global_synthetic_data_v2024.csv")

sankey_data <- read_csv("CTDC_VPsynthetic_condensed.csv")

### Data processing ###

# ----------------------
# Code1: Maps,list and line chart
# ----------------------


required_columns_code1 <- c("citizenship", "yearOfRegistration")
missing_columns_code1 <- setdiff(required_columns_code1, colnames(user_data))
if (length(missing_columns_code1) > 0) {
  stop(paste("Code1 The required data is missing the following columns:", paste(missing_columns_code1, collapse = ", ")))
}


user_data_code1 <- user_data %>%
  mutate(citizenship_mapped = recode(citizenship, !!!country_mapping))


user_data_filtered_code1 <- user_data_code1 %>%
  filter(!is.na(citizenship_mapped), citizenship_mapped != "")


citizenship_counts_map <- user_data_filtered_code1 %>%
  group_by(citizenship_mapped) %>%
  summarise(total_cases = n(), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  filter(citizenship_mapped %in% world$admin)


world_data <- world %>%
  left_join(citizenship_counts_map, by = c("admin" = "citizenship_mapped"))


bins <- c(0, 500, 1000, 1500, 2000, 5000, 10000, 15000, max(world_data$total_cases, na.rm = TRUE))
custom_palette <- viridis(length(bins) - 1, option = "C")
pal <- colorBin(
  palette = custom_palette,
  domain = world_data$total_cases,
  bins = bins,
  na.color = "lightgray"
)


all_countries <- unique(citizenship_counts_map$citizenship_mapped)
n_countries <- length(all_countries)
country_colors <- setNames(qualitative_hcl(n_countries, palette = "Dynamic"), all_countries)
line_types <- c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot")
country_line_types <- setNames(rep(line_types, length.out = n_countries), all_countries)

# ----------------------
# Code2: pie Chart
# ----------------------


required_columns_code3 <- c(
  "citizenship", "yearOfRegistration", "gender", "ageBroad", "CountryOfExploitation",
  "traffickMonths", "isForcedLabour", "isSexualExploit", "isOtherExploit",
  "meansDebtBondageEarnings", "meansThreats", "meansAbusePsyPhySex",
  "meansFalsePromises", "meansDrugsAlcohol", "meansDenyBasicNeeds",
  "meansExcessiveWorkHours", "meansWithholdDocs",
  "recruiterRelationIntimatePartner", "recruiterRelationFriend",
  "recruiterRelationFamily", "recruiterRelationOther",
  "typeOfLabourAgriculture", "typeOfLabourConstruction",
  "typeOfLabourDomesticWork", "typeOfLabourHospitality",
  "typeOfSexProstitution", "typeOfSexPornography"
)
missing_columns_code3 <- setdiff(required_columns_code3, colnames(user_data))
if (length(missing_columns_code3) > 0) {
  stop(paste("The data required by Code2 is missing the following columns:", paste(missing_columns_code3, collapse = ", ")))
}




data_clean <- user_data %>%
  mutate(citizenship = recode(citizenship, !!!country_mapping)) %>%
  mutate(
    gender = ifelse(is.na(gender) | gender == "", "Unknown", gender),
    gender = factor(gender, levels = c("Man", "Woman", "Trans/Transgender/NonConforming", "Unknown"))
  )


gender_counts <- data_clean %>%
  filter(gender != "Unknown") %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = 'drop')

total_known <- sum(gender_counts$count)

if (total_known == 0) {
  stop("No known gender data was available for reassignment.")
}

gender_counts <- gender_counts %>%
  mutate(proportion = count / sum(count))


set.seed(123)  

unknown_count <- sum(data_clean$gender == "Unknown")

if (unknown_count > 0) {
  assigned_genders <- sample(
    gender_counts$gender,
    size = unknown_count,
    replace = TRUE,
    prob = gender_counts$proportion
  )
  
  data_clean$gender[data_clean$gender == "Unknown"] <- as.character(assigned_genders)
}


data_clean$gender <- factor(data_clean$gender, levels = c("Man", "Woman", "Trans/Transgender/NonConforming"))


data_clean_code3 <- data_clean %>%
  mutate(
    ageBroad = ifelse(is.na(ageBroad) | ageBroad == "", "Unspecified", ageBroad),
    ageBroad = factor(ageBroad, levels = c("0--8", "09--17", "18--20", "21--23",
                                           "24--26", "27--29", "30--38", "39--47", "48+", "Unspecified"))
  )


specified_age <- data_clean_code3 %>% filter(ageBroad != "Unspecified")
unspecified_age <- data_clean_code3 %>% filter(ageBroad == "Unspecified")


age_props <- specified_age %>%
  group_by(gender, ageBroad) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(gender) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()


assigned_age <- unspecified_age %>%
  group_by(gender) %>%
  group_modify(~ {
    current_gender <- .y$gender
    props <- age_props %>% filter(gender == current_gender)
    if (nrow(props) == 0) {
      
      mutate(.x, ageBroad = "Unspecified")
    } else {
      
      props <- props %>%
        mutate(proportion = proportion / sum(proportion))
      
      mutate(.x, ageBroad = sample(
        props$ageBroad,
        size = n(),
        replace = TRUE,
        prob = props$proportion
      ))
    }
  }) %>%
  ungroup()


final_data_code3 <- specified_age %>%
  bind_rows(assigned_age)

human_traffic_data_code3 <- final_data_code3 %>%
  group_by(yearOfRegistration, citizenship, gender, ageBroad) %>%
  summarise(total_cases = n(), .groups = 'drop')







### Define the content of the information Hub page ###


Hub_Guide <- tabPanel(
  "Guide",
  fluidPage(
    
    
    h2(HTML("<b>Guide</b>"), style = "text-align:center"),
    br(),
    span(
      h4("The global human trafficking visualization platform shows global trends in human trafficking, gender and age distribution of victims, types of exploitation, and trafficking flows between regions. Through maps and line graphs, users can view the number of trafficking cases in each country and its changing trends over time. The age and gender distribution pie charts reveal that victims are mainly concentrated in the 21-38 age group, with women accounting for the highest proportion. The platform also shows the distribution of types of exploitation (such as forced labor and sexual exploitation) and means of control, and uses Sankey diagrams to visualize trafficking flows between regions around the world. Users can use filtering options to gain in-depth insights into the characteristics of human trafficking in specific countries, time periods, and genders.",
         style = "font-weight:normal; padding: 10px; margin: 0 50px; line-height: 2;"),
      style = "margin-left:50px; margin-right:50px;"
    ),
    
    br(),
    br(),
    
    
    fluidRow(
      column(6, tags$img(src = "guide1.png", width = "100%")),  
      column(6, 
             div(
               style = "height: 400px; display: flex; align-items: center; justify-content: center; text-align: center;",
               h4("Press this button to switch between different views.",
                  style = "font-weight:normal; padding: 10px; margin: 10px; text-align: center; line-height: 1.8;")
             )
      )
    ),
    
    br(),
    
    
    fluidRow(
      column(6, tags$img(src = "guide2.png", width = "100%")), 
      column(6, 
             div(
               style = "height: 400px; display: flex; align-items: center; justify-content: center; text-align: center;",
               h4(HTML("This map is for illustration purposes only. The boundaries and names shown, and the designations used on this map do not imply official endorsement or acceptance by the International Organization for Migration."),
                  style = "font-weight:normal; padding: 10px; margin: 10px; text-align: left; line-height: 1.8;")
             )
      )
    ),
    
    br(),
    
    
    fluidRow(
      column(6, tags$img(src = "guide3.png", width = "100%")),  
      column(6, 
             div(
               style = "height: 400px; display: flex; align-items: center; justify-content: center; text-align: center;",
               h4(HTML("Users can navigate through maps, graphs, and list views to analyze global human trafficking cases and trends over time.<br><br>
               Steps to use the filters:
               <ol>
                 <li>Select a country from the dropdown menu. You can either choose a specific country or view all countries.</li>
                 <li>Use the year slider to select a specific year or a range of years to explore human trafficking data over time.</li>
                 <li>Select one or more genders to filter the data by gender categories, including options for Male, Female, and Trans/Non-binary.</li>
               </ol>"),
                  style = "font-weight:normal; padding: 10px; margin: 10px; text-align: left; line-height: 1.8;")
             )
      )
    ),
    
    br(),
    
    
    div(
      style = "text-align:center; font-weight:bold; padding-top: 20px; font-size:24px; color:red;",
      h4("All visualization images take time to load, please be patient")
    )
  )
)







Hub_data <- tabPanel(
  "Data & Reference",
  br(),
  h2(HTML("<b>Data</b>"), style = "text-align:center"),
  br(),
  span(
    h4(
      "The application utilizes two synthetic human trafficking datasets from 2002 to 2022. The first dataset provides detailed information on victim demographics, such as gender, age group, and citizenship, as well as the countries where exploitation occurs. It also includes the types of exploitation and the control methods used on victims. The second dataset focuses on additional victim-related data, including trafficking duration, recruiter relations, and sectors of labor exploitation.",
      style = "border: 1px solid #ddd; padding: 20px; margin-left: auto; margin-right: auto; width: 80%; line-height: 1.5; font-weight: normal; text-align: justify;"
    )
  ),
  br(),
  br(),
  a("Click here to download the data.", href = "https://www.ctdatacollaborative.org/page/dashboards-datasets", style = "text-align:center; display:block;"),
  br(),
  br(),
  
  h2(HTML("<b>Reference</b>"), style = "text-align:center"),
  br(),
  div(
    style = "text-align: center;",
    h4(
      "The following resources provided ideas and R code for this application:",
      style = "border: 1px solid #ddd; padding: 20px; margin-left: auto; margin-right: auto; width: 80%; line-height: 1.5; font-weight: normal; text-align: justify;"
    ),
    br(),
    a("Sankey Diagrams with ggplot2 and ggsankey", href = "https://r-charts.com/flow/sankey-diagram-ggplot2/#ggsankey"),
    br(),
    a("Plotly R Graphing Library", href = "https://plotly.com/r/"),
    br(),
    a("How to insert pictures in shinyapp", href = "https://rstudio.github.io/shiny/reference/renderImage.html"),
    br(),
    a("How Banks Can Help Stop Human Trafficking", href = "https://feedzai.com/blog/what-can-banks-do-to-stop-human-trafficking-plenty/"),
    br(),
    a("UNODC on Human Trafficking and Crime", href = "https://www.unodc.org/unodc/en/human-trafficking/crime.html")
  )
)







Hub_FQA <- tabPanel(
  "FQA",
  br(),
  br(),
  
  h4("Want to know more about human trafficking? Please copy the following link and open it in your browser", 
     style = "font-weight:normal; text-align:center; margin-bottom: 20px;"),
  
  div(
    style = "text-align:center;",
    tags$img(src = "FQA.png", width = "50%")  
  ),
  
  br(),
  
  div(
    style = "text-align:center; font-size: 16px;",
    "link： https://www.unodc.org/unodc/en/human-trafficking/faqs.html"
  )
)










#### Defining the UI ####


ui <- navbarPage(
  theme = shinytheme("united"),
  "Global Human Trafficking Cases Dashboard",
  
  tags$style(HTML("
  
  .navbar {
    background-color: #000000 !important;  
  }
  
  
  .navbar .navbar-nav li a {
    background-color: #000000 !important;  
    color: white !important;  
  }

  
  .navbar .navbar-nav li.active a {
    background-color: #000000 !important;  
    color: white !important;  
  }
")),
  
  
  
  
  
  tabPanel("Home",
           fluidPage(
             div(
               style = "position: relative; text-align: center;",
               
               tags$img(src = "1.png", width = "100%"),
               
               absolutePanel(
                 top = "50%", left = "50%", width = "100%",
                 style = "transform: translate(-50%, -50%); color: white; font-size: 40px; font-weight: bold;",
                 HTML("<p>You have been sold.</p><p style='font-size: 24px;'>Human trafficking is a large-scale and complex crime, with many forms.</p>")
               )
             ),
             
             fluidRow(
               column(12, br())
             ),
             
             fluidRow(
               column(12, 
                      tags$img(src = "2.png", width = "100%")  
               )
             )
           )
  )
  ,
  
  
  
  
  
  tabPanel("Overview",
           fluidPage(
             titlePanel("Global Human Trafficking Cases Map"),
             
             tags$style(type = "text/css", "
.leaflet-container {
  background: white !important;
}
"),
             
             fluidRow(
               column(width = 6,
                      tabsetPanel(
                        tabPanel("Map", leafletOutput("map", height = "700px")),
                        tabPanel("List", DTOutput("country_list", height = "700px"))
                      )
               ),
               
               column(width = 6,
                      
                      div(style = "margin-top: -50px;",  
                          h4("Click on a country on the map or in the list to view the change in human trafficking cases over time.")
                      ),
                      
                      uiOutput("current_selection"),
                      
                      actionButton("clear_selection", "Clear Country Selection", style = "margin-bottom: 20px; margin-top: 10px;"),
                      
                      plotlyOutput("line_chart", height = "600px", width = "100%")
               )
             )
             
             
           )
  ),
  
  
  tabPanel("Demographics",
           fluidPage(
             titlePanel("Age and Gender Distribution of Human Trafficking Cases"),
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Options"),
                 selectInput(
                   'country_code3',
                   label = 'Select Country',
                   choices = c('All', sort(unique(human_traffic_data_code3$citizenship))),
                   selected = 'All',
                   multiple = FALSE
                 ),
                 sliderInput(
                   'year_range_code3',
                   label = 'Select Year',
                   min = min(human_traffic_data_code3$yearOfRegistration, na.rm = TRUE),
                   max = max(human_traffic_data_code3$yearOfRegistration, na.rm = TRUE),
                   value = max(human_traffic_data_code3$yearOfRegistration, na.rm = TRUE),
                   step = 1,
                   sep = "",
                   animate = TRUE
                 ),
                 checkboxGroupInput(
                   'gender_code3',
                   'Select Gender',
                   choices = levels(human_traffic_data_code3$gender),
                   selected = levels(human_traffic_data_code3$gender)
                 )
               ),
               mainPanel(
                 plotlyOutput('traffic_pie_chart', height = "600px", width = "100%")
               )
             )
           )
  ),
  
  
  tabPanel("Analysis",
           fluidPage(
             titlePanel("Trafficking and Exploitation Distribution"),
             
             fluidRow(
               column(4,
                      selectInput(
                        'country_analysis',
                        label = 'Select Country',
                        choices = c('All', sort(unique(final_data_code3$citizenship))),
                        selected = 'All',
                        multiple = FALSE
                      )
               ),
               column(4,
                      sliderInput(
                        'year_range_analysis',
                        label = 'Select Year',
                        min = min(final_data_code3$yearOfRegistration, na.rm = TRUE),
                        max = max(final_data_code3$yearOfRegistration, na.rm = TRUE),
                        value = c(min(final_data_code3$yearOfRegistration, na.rm = TRUE), max(final_data_code3$yearOfRegistration, na.rm = TRUE)),
                        step = 1,
                        sep = "",
                        ticks = TRUE
                      )
               ),
               column(4,
                      checkboxGroupInput(
                        'gender_analysis',
                        'Select Gender',
                        choices = levels(final_data_code3$gender),
                        selected = levels(final_data_code3$gender)
                      )
               )
             ),
             
             fluidRow(
               
               column(4,
                      plotOutput("donutPlotTrafficking", height = "300px"),
                      plotOutput("donutPlotExploitation", height = "300px")
               ),
               
               column(4,
                      plotOutput("donutPlotMeansOfControl", height = "300px"),
                      plotOutput("donutPlotRecruiterRelation", height = "300px")
               ),
               
               column(4,
                      plotOutput("donutPlotLabourType", height = "300px"),
                      plotOutput("donutPlotSexType", height = "300px")
               )
             )
           )
  ),
  
  
  tabPanel("Flow",
           fluidPage(
             titlePanel("Human Trafficking Flow Visualization"),
             
             fluidRow(
               column(6, 
                      radioButtons(
                        "gender_flow",
                        "Select Gender:",
                        choices = c("Male", "Female"),
                        selected = "Male",
                        inline = TRUE  
                      )
               ),
               column(6,
                      checkboxGroupInput(
                        "exploitType_flow",
                        "Select Exploitation Types:",
                        choices = c("Forced Labour", "Sexual Exploit", "Other"),
                        selected = c("Forced Labour", "Sexual Exploit", "Other"),
                        inline = TRUE  
                      )
               )
             ),
             
             fluidRow(
               column(12, 
                      p("Note: Due to the relatively low number of human trafficking cases from the Americas/Oceania region, these cases are displayed separately in the bar chart on the right to avoid being overshadowed by the global data. This allows for clearer analysis of the exploitation type distribution in this region.")
               )
             ),
             
             fluidRow(
               column(12, plotOutput("sankeyPlot_flow", height = "600px"))  
             ),
             
             fluidRow(
               column(6, plotOutput("barPlot_flow", height = "500px")),  
               column(6, plotOutput("rosePlot_flow", height = "500px"))  
             )
           )
  ),
  
  navbarMenu("Information Hub", 
             Hub_Guide,
             Hub_data,
             Hub_FQA
  )
)








#### Defining server  ####

server <- function(input, output, session) {
  
  
  selected <- reactiveValues(countries = list())
  
  ### Code1 map,list,line chart ###
  
  
  output$map <- renderLeaflet({
    leaflet(world_data) %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(total_cases),
        color = "#444444",
        weight = 1,
        fillOpacity = 0.8,
        layerId = ~admin,
        popup = ~paste(
          "Country code:", iso_a3, "<br>",
          "Country name:", admin, "<br>",
          "Total cases:", total_cases
        ),
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = ~total_cases,
        opacity = 0.7,
        title = "Total Cases",
        position = "bottomleft",
        labFormat = labelFormat(suffix = "")
      )
  })
  
  
  proxy <- dataTableProxy("country_list")
  
  
  output$country_list <- renderDT({
    datatable(
      citizenship_counts_map,
      colnames = c("Citizenship", "Count"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 15, 20),
        autoWidth = TRUE
      ),
      rownames = FALSE,
      selection = 'multiple'
    )
  })
  
  observeEvent(input$country_list_rows_selected, {
    selected_rows <- input$country_list_rows_selected
    if (length(selected_rows) > 0) {
      
      selected_countries_from_table <- citizenship_counts_map[selected_rows, "citizenship_mapped"]
      
      selected$countries <- unique(c(selected$countries, selected_countries_from_table))
      
      proxy %>% selectRows(NULL)
    }
  })
  
  
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!(clicked_country %in% selected$countries)) {
      selected$countries <- c(selected$countries, clicked_country)
    }
  })
  
  
  observeEvent(input$clear_selection, {
    selected$countries <- list()
  })
  
  
  output$line_chart <- renderPlotly({
    selected_countries <- selected$countries
    
    
    if (length(selected_countries) == 0) {
      chart_data <- user_data_filtered_code1 %>%
        group_by(yearOfRegistration) %>%
        summarise(total_cases = n(), .groups = "drop") %>%
        arrange(yearOfRegistration)
      
      
      if (nrow(chart_data) == 0) {
        return(plot_ly(type = 'scatter', mode = 'markers') %>%
                 layout(
                   title = "No global data available.",
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       x = 0.5,
                       y = 0.5,
                       text = "No global data available.",
                       showarrow = FALSE,
                       xref = "paper",
                       yref = "paper"
                     )
                   )
                 ))
      }
      
      return(plot_ly(chart_data, 
                     x = ~yearOfRegistration, 
                     y = ~total_cases, 
                     type = 'scatter', 
                     mode = 'lines+markers', 
                     name = "Global", 
                     line = list(color = 'black', dash = 'solid')) %>%
               layout(
                 title = "Total Human Trafficking Cases Over Time",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Number of Cases"),
                 hovermode = "closest",
                 showlegend = TRUE
               ))
    }
    
    
    filtered_data <- user_data_filtered_code1 %>%
      filter(citizenship_mapped %in% selected_countries)
    
    
    if (nrow(filtered_data) == 0) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>%
               layout(
                 title = "No data available for selected country.",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     x = 0.5,
                     y = 0.5,
                     text = "No data available for selected country.",
                     showarrow = FALSE,
                     xref = "paper",
                     yref = "paper"
                   )
                 )
               ))
    }
    
    
    chart_data <- filtered_data %>%
      group_by(yearOfRegistration, citizenship_mapped) %>%
      summarise(total_cases = n(), .groups = "drop") %>%
      arrange(yearOfRegistration)
    
    
    p <- plot_ly()
    
    for (country in unique(chart_data$citizenship_mapped)) {
      country_data <- chart_data %>% filter(citizenship_mapped == country)
      
      p <- p %>%
        add_trace(
          x = country_data$yearOfRegistration,
          y = country_data$total_cases,
          type = 'scatter',
          mode = 'lines+markers',
          name = country,
          line = list(width = 2, color = country_colors[country], dash = country_line_types[country]),
          marker = list(size = 6, color = country_colors[country]),
          hovertemplate = paste(
            "Country: ", country, "<br>",
            "Year: %{x}<br>",
            "Total Cases: %{y}<extra></extra>"
          )
        )
    }
    
    p <- p %>% layout(
      title = "Human Trafficking Cases Over Time",
      xaxis = list(title = "Year", dtick = 1, showgrid = FALSE),
      yaxis = list(title = "Number of Cases", showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      showlegend = TRUE
    )
    
    p
  })
  
  
  output$current_selection <- renderUI({
    selected_countries <- selected$countries
    
    if (length(selected_countries) == 0) {
      country_text <- "Global"
    } else {
      country_text <- paste(selected_countries, collapse = ", ")
    }
    
    HTML(paste0("<strong>Currently Selected Countries:</strong> ", country_text))
  })
  
  
  
  
  
  
  ### Code2 pie chart ###
  
  
  output$traffic_pie_chart <- renderPlotly({
    
    
    filtered_data_code3 <- human_traffic_data_code3 %>%
      filter(
        yearOfRegistration == input$year_range_code3,
        gender %in% input$gender_code3,
        (input$country_code3 == 'All' | citizenship == input$country_code3)
      )
    
    
    if (nrow(filtered_data_code3) == 0) {
      return(plot_ly() %>%
               layout(
                 title = "No Data",
                 annotations = list(
                   list(
                     text = "No data available for the selected criteria.",
                     x = 0.5, y = 0.5, showarrow = FALSE,
                     font = list(size = 20)
                   )
                 )
               )
      )
    }
    
    
    age_data_code3 <- filtered_data_code3 %>%
      group_by(ageBroad) %>%
      summarise(
        age_total_cases = sum(total_cases),
        .groups = 'drop'
      )
    
    
    gender_info_code3 <- filtered_data_code3 %>%
      group_by(ageBroad, gender) %>%
      summarise(
        total_cases = sum(total_cases),
        .groups = 'drop'
      ) %>%
      group_by(ageBroad) %>%
      mutate(
        percentage = round(total_cases / sum(total_cases) * 100, 2)
      ) %>%
      summarise(
        gender_info = paste(gender, "(Cases:", total_cases, ", Percentage:", percentage, "%)", collapse = "<br>"),
        .groups = 'drop'
      )
    
    
    pie_data_code3 <- left_join(age_data_code3, gender_info_code3, by = "ageBroad") %>%
      mutate(ageBroad = factor(ageBroad, levels = c("0--8", "09--17", "18--20", "21--23",
                                                    "24--26", "27--29", "30--38", "39--47", "48+"))) %>%
      arrange(ageBroad)
    
    
    num_age_groups_code3 <- length(unique(pie_data_code3$ageBroad))
    max_colors_code3 <- brewer.pal.info["Set3", "maxcolors"]
    if (num_age_groups_code3 > max_colors_code3) {
      colors_code3 <- colorRampPalette(brewer.pal(max_colors_code3, "Set3"))(num_age_groups_code3)
    } else {
      colors_code3 <- brewer.pal(num_age_groups_code3, "Set3")
    }
    
    
    selected_country_code3 <- ifelse(input$country_code3 == "All", "Global", input$country_code3)
    plot_title_code3 <- paste("Age and Gender Distribution of Human Trafficking Cases in", input$year_range_code3, "(", selected_country_code3, ")", sep = " ")
    
    
    plot_ly(
      data = pie_data_code3,
      labels = ~ageBroad,
      values = ~age_total_cases,
      type = 'pie',
      sort = FALSE,
      textinfo = 'none',
      hoverinfo = 'text',
      text = ~paste("Age Group:", ageBroad, "<br>", gender_info),
      marker = list(
        colors = colors_code3,
        line = list(color = '#FFFFFF', width = 2)
      ),
      pull = 0.05
    ) %>%
      layout(
        title = plot_title_code3,
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          x = 0.5, 
          xanchor = "center", 
          y = -0.3,  
          font = list(size = 16)
        ),
        margin = list(t = 50, l = 50, r = 50, b = 150), 
        annotations = list(
          list(
            text = "Data filtered: 'year' and 'ageBroad' where null values were removed.",
            x = 0.5, 
            y = -0.2,  
            showarrow = FALSE,
            xref = 'paper', 
            yref = 'paper',
            xanchor = 'center',
            font = list(size = 12, color = 'grey')
          )
        )
      )
  })
  
  ### code3 Donut chart ###
  
  
  create_donut_plot <- function(data, aes_fill, title_label, fill_label, label_threshold = 0.02) {
    if (nrow(data) == 0) {
      ggplot() + 
        theme_void() + 
        ggtitle(paste(title_label, "\n(No Data)")) +
        theme(plot.title = element_text(size = 16, face = "bold"))
    } else {
      
      data <- data %>%
        mutate(proportion = Count / sum(Count)) %>%
        arrange(desc(proportion))
      
      
      data <- data %>%
        mutate(category = ifelse(proportion >= label_threshold, !!sym(aes_fill), "Others"))
      
      
      data <- data %>%
        group_by(category) %>%
        summarise(Count = sum(Count), .groups = 'drop') %>%
        mutate(proportion = Count / sum(Count)) %>%
        arrange(desc(proportion))
      
      
      data <- data %>%
        mutate(label = paste0(round(100 * proportion, 1), "%"))
      
      ggplot(data, aes(x = 2, y = Count, fill = category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = label), 
                  position = position_stack(vjust = 0.5), size = 4) +
        labs(fill = fill_label, title = paste(title_label, "Distribution")) +
        scale_fill_brewer(palette = "Set3") +  
        theme_void() +  
        theme(
          legend.position = "right",
          legend.text = element_text(size = 14),       
          legend.title = element_text(size = 16),      
          plot.title = element_text(size = 16, face = "bold")  
        ) +
        xlim(0.5, 2.5)  
    }
  }
  
  
  filtered_data_analysis <- reactive({
    data_filtered <- final_data_code3
    
    
    if (input$country_analysis != 'All') {
      data_filtered <- data_filtered %>% filter(citizenship == input$country_analysis)
    }
    
    
    data_filtered <- data_filtered %>%
      filter(yearOfRegistration >= input$year_range_analysis[1],
             yearOfRegistration <= input$year_range_analysis[2])
    
    
    if (!is.null(input$gender_analysis)) {
      data_filtered <- data_filtered %>% filter(gender %in% input$gender_analysis)
    } else {
      
      data_filtered <- data_filtered[0, ]
    }
    
    return(data_filtered)
  })
  
  
  output$donutPlotTrafficking <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    valid_values <- c("0--12 (0-1 yr)", "13--24 (1-2 yrs)", "25+ (2+ yrs)")
    data_traf <- data_filtered %>%
      filter(traffickMonths %in% valid_values)
    
    if (nrow(data_traf) == 0) {
      
      ggplot() + 
        theme_void() + 
        ggtitle("Trafficking Duration Distribution\n(No Data)") +
        theme(plot.title = element_text(size = 16, face = "bold"))
    } else {
      
      traffick_counts <- data_traf %>%
        group_by(traffickMonths) %>%
        summarise(Count = n(), .groups = 'drop')
      
      
      create_donut_plot(traffick_counts, "traffickMonths", "Trafficking Duration", "Trafficking Duration", label_threshold = 0.02)
    }
  })
  
  
  output$donutPlotExploitation <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    exploitation_data <- data.frame(
      Type = c("Forced Labour", "Sexual Exploitation", "Other Exploitation"),
      Count = c(
        sum(data_filtered$isForcedLabour, na.rm = TRUE),
        sum(data_filtered$isSexualExploit, na.rm = TRUE),
        sum(data_filtered$isOtherExploit, na.rm = TRUE)
      )
    )
    
    
    exploitation_data <- exploitation_data %>% filter(Count > 0)
    
    
    create_donut_plot(exploitation_data, "Type", "Exploitation Type", "Exploitation Type", label_threshold = 0.02)
  })
  
  
  output$donutPlotMeansOfControl <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    means_data <- data.frame(
      MeansOfControl = c("Debt Bondage", "Threats", "Abuse (Psy/Phy/Sex)", "False Promises",
                         "Drugs/Alcohol", "Deny Basic Needs", "Excessive Work Hours", "Withhold Documents"),
      Count = c(
        sum(data_filtered$meansDebtBondageEarnings, na.rm = TRUE),
        sum(data_filtered$meansThreats, na.rm = TRUE),
        sum(data_filtered$meansAbusePsyPhySex, na.rm = TRUE),
        sum(data_filtered$meansFalsePromises, na.rm = TRUE),
        sum(data_filtered$meansDrugsAlcohol, na.rm = TRUE),
        sum(data_filtered$meansDenyBasicNeeds, na.rm = TRUE),
        sum(data_filtered$meansExcessiveWorkHours, na.rm = TRUE),
        sum(data_filtered$meansWithholdDocs, na.rm = TRUE)
      )
    )
    
    
    means_data <- means_data %>% filter(Count > 0)
    
    
    create_donut_plot(means_data, "MeansOfControl", "Means of Control", "Means of Control", label_threshold = 0.02)
  })
  
  
  output$donutPlotRecruiterRelation <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    recruiter_data <- data.frame(
      RecruiterRelation = c("Intimate Partner", "Friend", "Family", "Other"),
      Count = c(
        sum(data_filtered$recruiterRelationIntimatePartner, na.rm = TRUE),
        sum(data_filtered$recruiterRelationFriend, na.rm = TRUE),
        sum(data_filtered$recruiterRelationFamily, na.rm = TRUE),
        sum(data_filtered$recruiterRelationOther, na.rm = TRUE)
      )
    )
    
    
    recruiter_data <- recruiter_data %>% filter(Count > 0)
    
    
    create_donut_plot(recruiter_data, "RecruiterRelation", "Recruiter Relation", "Recruiter Relation", label_threshold = 0.02)
  })
  
  
  output$donutPlotLabourType <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    labour_data <- data.frame(
      LabourType = c("Agriculture", "Construction", "Domestic Work", "Hospitality"),
      Count = c(
        sum(data_filtered$typeOfLabourAgriculture, na.rm = TRUE),
        sum(data_filtered$typeOfLabourConstruction, na.rm = TRUE),
        sum(data_filtered$typeOfLabourDomesticWork, na.rm = TRUE),
        sum(data_filtered$typeOfLabourHospitality, na.rm = TRUE)
      )
    )
    
    
    labour_data <- labour_data %>% filter(Count > 0)
    
    
    create_donut_plot(labour_data, "LabourType", "Labour Type", "Labour Type", label_threshold = 0.02)
  })
  
  
  output$donutPlotSexType <- renderPlot({
    
    
    data_filtered <- filtered_data_analysis()
    
    
    sex_data <- data.frame(
      SexType = c("Prostitution", "Pornography"),
      Count = c(
        sum(data_filtered$typeOfSexProstitution, na.rm = TRUE),
        sum(data_filtered$typeOfSexPornography, na.rm = TRUE)
      )
    )
    
    
    sex_data <- sex_data %>% filter(Count > 0)
    
    
    create_donut_plot(sex_data, "SexType", "Sex Exploitation Type", "Sex Exploitation Type", label_threshold = 0.02)
  })
  
  
  
  
  
  
  ### code 4 Sankey diagram and Rose diagram ###
  
  
  df_cleaned_flow <- sankey_data %>%
    filter(!is.na(gender)) %>%
    mutate(
      ExploitType = case_when(
        isForcedLabour == 1 ~ "Forced Labour",
        isSexualExploit == 1 ~ "Sexual Exploit",
        TRUE ~ "Other"
      )
    )
  
  
  filtered_df_flow <- reactive({
    df_cleaned_flow %>%
      filter(
        ExploitType %in% input$exploitType_flow,
        gender == input$gender_flow
      )
  })
  
  
  df_simplified_flow <- reactive({
    filtered_df_flow() %>%
      select(UN_COO_Region, UN_COE_Region, isForcedLabour, isSexualExploit) %>%
      mutate(
        UN_COO_Region = trimws(UN_COO_Region),
        UN_COE_Region = trimws(UN_COE_Region)
      ) %>%
      filter(
        UN_COO_Region != "", UN_COO_Region != "NULL",
        UN_COE_Region != "", UN_COE_Region != "NULL",
        UN_COO_Region != "Americas/Oceania",  
        UN_COE_Region != "Americas/Oceania"   
      ) %>%
      mutate(
        ExploitType = case_when(
          isForcedLabour == 1 ~ "Forced Labour",
          isSexualExploit == 1 ~ "Sexual Exploit",
          TRUE ~ "Other"
        )
      ) %>%
      group_by(UN_COO_Region, ExploitType, UN_COE_Region) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  
  output$sankeyPlot_flow <- renderPlot({
    df_clean_flow <- df_simplified_flow()
    
    if(nrow(df_clean_flow) == 0){
      ggplot() + 
        theme_void() + 
        ggtitle("No Data for Selected Exploitation Types") +
        theme(plot.title = element_text(size = 16, face = "bold"))
    } else {
      ggplot(df_clean_flow, 
             aes(axis1 = UN_COO_Region, axis2 = ExploitType, axis3 = UN_COE_Region, y = count)) +
        geom_alluvium(aes(fill = ExploitType), alpha = 0.8) +
        geom_stratum(width = 1/3) +
        geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 5, vjust = 0.5) +
        scale_x_discrete(limits = c("Source Region", "Exploitation Type", "Destination Region"), expand = c(0.15, 0.05)) +
        theme_minimal() +
        labs(title = "Human Trafficking Flow (By Region and Exploitation Type)", x = NULL, y = "Number of Cases") +
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              axis.text.x = element_text(size = 16))
    }
  })
  
  
  df_americas_flow <- reactive({
    filtered_df_flow() %>%
      filter(UN_COO_Region == "Americas/Oceania" | UN_COE_Region == "Americas/Oceania") %>%
      group_by(ExploitType) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  
  output$barPlot_flow <- renderPlot({
    df_filtered_flow <- df_americas_flow()
    
    if(nrow(df_filtered_flow) == 0){
      ggplot() + 
        theme_void() + 
        ggtitle("No Data for Selected Exploitation Types in Americas/Oceania") +
        theme(plot.title = element_text(size = 16, face = "bold"))
    } else {
      ggplot(df_filtered_flow, aes(x = ExploitType, y = count, fill = ExploitType)) +
        geom_bar(stat = "identity", width = 0.7) +
        theme_minimal() +
        labs(title = "Exploitation Type Distribution in Americas/Oceania", x = "Exploitation Type", y = "Number of Cases") +
        theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position = "none",
              axis.text.x = element_text(size = 16))
    }
  })
  
  
  df_rose_flow <- reactive({
    filtered_df_flow() %>%
      filter(!is.na(IP_ageBroad)) %>%
      mutate(
        IP_ageBroad = case_when(
          IP_ageBroad %in% c("0--29", "0-29") ~ "0-29",
          IP_ageBroad %in% c("30--38", "30-38") ~ "30-38",
          IP_ageBroad %in% c("39--47", "39-47") ~ "39-47",
          IP_ageBroad %in% c("48+", "48+") ~ "48+",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(IP_ageBroad))
  })
  
  
  output$rosePlot_flow <- renderPlot({
    
    df_filtered_flow <- df_rose_flow() %>%
      group_by(IP_ageBroad, ExploitType) %>%
      summarise(count = sum(n()), .groups = 'drop') 
    
    if(nrow(df_filtered_flow) == 0){
      ggplot() + 
        theme_void() + 
        ggtitle("Rose Chart: Age Group and Exploit Type (No Data)") +
        theme(plot.title = element_text(size = 16, face = "bold"))
    } else {
      ggplot(df_filtered_flow, aes(x = IP_ageBroad, y = count, fill = ExploitType)) +
        geom_bar(stat = "identity") + 
        coord_polar(theta = "x") +
        theme_minimal() +
        labs(
          title = paste("Rose Chart: Age Group and Exploit Type (", input$gender_flow, ")", sep = ""),
          x = "Age Group",
          y = "",  
          fill = "Exploit Type"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),  
          axis.text.x = element_text(size = 14),  
          axis.text.y = element_blank(),  
          axis.ticks.y = element_blank(),  
          panel.grid.minor = element_blank(),  
          legend.position = "none"  
        ) +
        
        annotate("text", x = 1.5, y = 0, label = "0", size = 4, color = "black", angle = 45) +
        annotate("text", x = 1.5, y = 100, label = "100", size = 4, color = "black", angle = 45) +
        annotate("text", x = 1.5, y = 200, label = "200", size = 4, color = "black", angle = 45) +
        annotate("text", x = 1.5, y = 300, label = "300", size = 4, color = "black", angle = 45) +
        annotate("text", x = 1.5, y = 400, label = "400", size = 4, color = "black", angle = 45) +
        annotate("text", x = 1.5, y = 500, label = "500", size = 4, color = "black", angle = 45) 
    }
  })
  
}


shinyApp(ui = ui, server = server)

