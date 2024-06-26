# Base R Shiny image
FROM rocker/shiny:latest

# Make a directory in the container 
RUN mkdir /home/shiny-app
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    dotnet-runtime-8.0 \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \ 
    libtiff5-dev \
    libjpeg-dev
RUN apt-get update && apt-get upgrade -y && apt-get clean

# Copy the Shiny app code
COPY . /home/shiny-app/

# Install R dependencies
RUN Rscript /home/shiny-app/install_dependencies.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R

