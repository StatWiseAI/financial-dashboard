FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

# Copy app files
COPY app.R ./
COPY custom.css ./www/
COPY *.R ./

# Install basic packages
RUN R -e "install.packages(c('shiny', 'shinydashboard'), repos='https://cloud.r-project.org')"

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]
