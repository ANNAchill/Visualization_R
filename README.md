# Melbourne Exploration Guide 🌏

This is an interactive **Shiny web application** designed to help users explore Melbourne, Australia. It includes **maps, data visualizations, restaurant recommendations, hotel listings, entertainment spots, and public transport information**.

## 📌 Features

- 🗺 **Interactive Map**: Explore attractions, transport, and other points of interest.
- 🏨 **Hotel Listings & Top 5 Hotels**: Find accommodations with details like amenities, pricing, and locations.
- 🍽 **Food Guide**: Browse must-visit restaurants, coffee shops, and dining categories.
- 🎭 **Entertainment Spots**: Discover bars, parks, casinos, and more fun places.
- 🚋 **Public Transport & Free Tram Zone**: Identify tram routes and stops within Melbourne's free transport zone.
- 📊 **Data Integration**: Combines multiple datasets (CSV, shapefiles) for a dynamic user experience.

## 📁 Files & Structure

- **`final_version.R`**: The main Shiny application script.
- **`data/`**: Folder containing all required datasets, including:
  - `bar.csv`, `museum.csv`, `food.csv`, `park.csv`, `hotel.csv` (Location-based data)
  - `PTV_METRO_TRAM_STOP.shp` (Tram stop GIS data)
- **`www/`**: Stores images/icons used in the application.
- **`tableau-in-shiny-v1.2.R`**: Tableau integration script for interactive data visualization.

## 🛠 Installation & Setup

### 1️⃣ Install Required Packages
Before running the app, install the following R packages:

```r
install.packages(c("shiny", "shinythemes", "readxl", "leaflet", "dplyr", "ggplot2",
                   "plotly", "sf", "shinydashboard", "shinyWidgets", "DT", "tidyr",
                   "geosphere", "shinyjs", "GGally", "htmltools", "scales", "fmsb"))
```

### 2️⃣ Run the Shiny App
Ensure you are in the correct working directory, then run:

```r
shiny::runApp("final_version.R")
```

The application will open in your default web browser.

## 🔍 Data Sources
- **PTV (Public Transport Victoria)**
- **Open Data Melbourne**
- **TripAdvisor & Google Maps API** (for attraction recommendations)

## 📜 License
This project is for educational and research purposes. 
