# Global Human Trafficking Visualization Dashboard

This project is a **Shiny** web application that visualizes global human trafficking trends using interactive maps, charts, and graphs. It leverages multiple datasets to display insights on trafficking cases, demographics, and exploitation types.

## Features

- **Interactive Maps**: View human trafficking cases worldwide, color-coded by case count.
- **Line Charts**: Explore trends in human trafficking cases over time.
- **Pie Charts**: Analyze the gender and age distribution of victims.
- **Sankey Diagrams**: Visualize trafficking flows between different world regions.
- **Data Filtering**: Select country, year, and gender to explore specific trafficking patterns.

## Files

- **`Shiny.R`**: The main R script containing the Shiny app code.
- **`CTDC_global_synthetic_data_v2024.csv`**: A dataset with detailed information on trafficking victims.
- **`CTDC_VPsynthetic_condensed.csv`**: A dataset focusing on trafficking duration, recruiter relations, and exploitation types.

## Installation & Setup

### Prerequisites
Ensure you have **R** and **RStudio** installed. Then, install the required R packages:

```r
install.packages(c("shiny", "shinythemes", "dplyr", "sf", "rnaturalearth", "leaflet", "plotly", 
                   "DT", "RColorBrewer", "colorspace", "viridis", "ggplot2", "readr", "ggalluvial"))
```

### Running the App
1. Clone this repository or download the `Shiny.R` file.
2. Place the required datasets (`.csv` files) in the same directory.
3. Open `Shiny.R` in **RStudio** and run:

```r
shiny::runApp("Shiny.R")
```

The application will launch in your default web browser.

## How It Works

- The app loads **synthetic human trafficking data** and processes it using **dplyr**.
- It visualizes data using **Leaflet (maps)**, **Plotly (interactive graphs)**, and **ggplot2 (static charts)**.
- Users can filter cases by **country, year, and gender** to gain detailed insights.
- A **guide section** explains how to navigate and use different views.

## Data Sources

- **Counter-Trafficking Data Collaborative (CTDC)**
  - [Download Data](https://www.ctdatacollaborative.org/page/dashboards-datasets)
- **UNODC Human Trafficking Reports**
  - [More Information](https://www.unodc.org/unodc/en/human-trafficking/crime.html)

## License
This project is open-source and provided for research and educational purposes.
