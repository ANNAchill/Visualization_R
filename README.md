# 🌍 Interactive Shiny Dashboards Collection

This repository contains **two interactive Shiny applications** that provide insights into **Melbourne exploration** and **global human trafficking data**. Both dashboards are built using R's **Shiny framework**, with dynamic filtering, interactive maps, and visual analytics.

## 📌 Project Overview

### 1️⃣ Melbourne Exploration Guide 🏙️  
A **Shiny web application** that helps users explore Melbourne’s top attractions, public transport, dining options, and entertainment venues.

#### 🔹 Key Features:
- 🗺 **Interactive Map** – Explore Melbourne’s landmarks, hotels, and public transport routes.
- 🏨 **Hotel Listings** – Find the best accommodations with detailed filters.
- 🍽 **Food & Dining** – Discover restaurants and coffee shops in the city.
- 🎭 **Entertainment Spots** – Browse museums, casinos, bars, and parks.
- 🚋 **Public Transport Info** – View tram stops and routes, including free tram zones.

#### 🔹 Quick Start:
```r
shiny::runApp("final_version.R")
```

#### 🔹 Data Sources:
- **Public Transport Victoria (PTV)**
- **Open Data Melbourne**
- **TripAdvisor & Google Maps API**

---

### 2️⃣ Global Human Trafficking Dashboard 🚨  
A **Shiny web application** that visualizes **global human trafficking** trends using interactive maps and data-driven insights.

#### 🔹 Key Features:
- 🌎 **Global Trafficking Map** – Visualize human trafficking cases by country.
- 📊 **Trend Analysis** – Analyze trends in human trafficking over time.
- 👥 **Demographic Breakdown** – Explore victim gender, age, and exploitation types.
- 🔀 **Sankey Flow Diagrams** – Show trafficking flows between different regions.
- 🔎 **Data Filtering** – Select country, year, and gender to explore specific patterns.

#### 🔹 Quick Start:
```r
shiny::runApp("Shiny.R")
```

#### 🔹 Data Sources:
- **Counter-Trafficking Data Collaborative (CTDC)**
- **UNODC Human Trafficking Reports**

---

## 📸 Project Previews
### Melbourne Exploration Dashboard 🏙️


### Human Trafficking Dashboard 🚨


---

## 🛠 Installation Guide
Both dashboards require the following **R packages**:
```r
install.packages(c("shiny", "shinythemes", "leaflet", "dplyr", "ggplot2", "plotly",
                   "sf", "shinydashboard", "shinyWidgets", "DT", "geosphere",
                   "shinyjs", "htmltools", "scales", "fmsb", "readr", "ggalluvial"))
```

To run a specific app:
```r
shiny::runApp("final_version.R")  # Melbourne Guide
shiny::runApp("Shiny.R")  # Human Trafficking Dashboard
```

---

## 📜 License
This project is for educational and research purposes.
