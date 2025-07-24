FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

# Copy app files
COPY . .

# Install R packages (including DT and others your app needs)
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'DT', \
    'plotly', \
    'dplyr', \
    'readr', \
    'htmltools', \
    'shinycssloaders', \
    'openxlsx', \
    'scales', \
    'lubridate' \
), repos='https://cloud.r-project.org')"

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]
