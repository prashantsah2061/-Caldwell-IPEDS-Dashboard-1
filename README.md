# Caldwell IPEDS Dashboard

An interactive **R Shiny dashboard** for exploring enrollment, graduation, retention, and institutional-efficiency data from the Integrated Postsecondary Education Data System (IPEDS).

## Features

* Compare university enrollment from 2010–2024
* Filter institutions by level, control, and year
* Explore 12-month enrollment by demographics
* Compare undergraduate and graduate enrollment
* Analyze full-time equivalent (FTE) enrollment
* Compare Caldwell University with peer institutions
* Explore graduation and retention rates
* Analyze institutional efficiency, expenses, revenue, staffing, and degree productivity
* View interactive charts, KPI cards, and data tables
* Download filtered data as CSV files

## Technologies Used

* R
* Shiny
* tidyverse
* ggplot2
* DT
* scales
* janitor
* data.table

## Project Structure

```text
├── app.R            # Starts the Shiny application
├── ui.R             # Dashboard user interface
├── mod_ipedsr.R     # Server logic and dashboard modules
├── functions.R      # Data loading and helper functions
├── data/            # IPEDS datasets used by the application
└── www/             # Images and other web assets
```

## How to Run the App

### 1. Clone the repository

```bash
git clone <repository-url>
cd Caldwell-IPEDS-Dashboard
```

### 2. Install the required R packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "ggplot2",
  "scales",
  "DT",
  "janitor",
  "data.table"
))
```

### 3. Run the application

```r
shiny::runApp()
```

You can also open `app.R` in RStudio and click **Run App**.

## Data Source

The dashboard uses publicly available higher-education data from the **Integrated Postsecondary Education Data System (IPEDS)**.

## Purpose

This project was developed to make institutional data easier to explore and compare through interactive visualizations. It can support enrollment analysis, peer comparison, retention analysis, graduation-rate analysis, and institutional planning.

## Author

**Prashant Sah**

Computer Science student at Caldwell University
