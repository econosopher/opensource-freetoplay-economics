# Open Source F2P Economics Dashboard

This project is a Shiny dashboard for analyzing and visualizing financial data for publicly traded video game companies. It has been refactored to use a modern, modular, and robust architecture.

## Features

- **Live Financial Data**: Fetches up-to-date financial statements and stock prices from the [SimFin API](https://simfin.com/).
- **Modular Architecture**: Built with a fully modular Shiny design, making the code clean, maintainable, and scalable.
- **Reproducible Environment**: Uses the `{renv}` package to ensure a consistent and reproducible R environment.
- **Interactive Visualizations**: Leverages `{plotly}` for dynamic charts and `{gt}` for beautiful, presentation-ready tables.

## Setup and Installation

### 1. API Key

This application requires a **free** API key from SimFin.

- Go to [https://app.simfin.com/login](https://app.simfin.com/login) to register and get your API key.
- Create a file named `.Renviron` in the root of this project directory.
- Add the following line to the `.Renviron` file, replacing `YOUR_API_KEY_HERE` with the key you obtained:

```
SIMFIN_API_KEY="YOUR_API_KEY_HERE"
```

### 2. R Environment

This project uses `{renv}` to manage dependencies. When you first open this project in RStudio (or an R console), `renv` should prompt you to restore the project library from the `renv.lock` file.

- Type `renv::restore()` in the R console and press Enter.
- This will install all the required R packages into a project-specific library.

### 3. Running the Application

Once the setup is complete, you can run the Shiny application:

- Open the `dashboard/dashboard.R` file.
- Execute the code, for example by clicking "Run App" in RStudio.

## Project Structure

- `R/`: Contains all the modular Shiny code. Each `mod_*.R` file represents a self-contained component of the dashboard (e.g., a tab). The `simfin_data.R` file handles all data retrieval from the SimFin API.
- `dashboard/`: The main entry point for the Shiny application.
  - `dashboard.R`: The top-level script that assembles the UI and server from the modules.
  - `ui_elements/`: Contains the UI definitions for the main layout (sidebar, body) and pointers to the module UIs.
- `dev/`: Contains non-application scripts, such as exploratory code.
- `renv/`: The `renv` project library (managed by `renv`, do not edit manually).
- `renv.lock`: The lockfile specifying all package dependencies.
- `.Renviron`: Local environment variables (you must create this file for your API key).