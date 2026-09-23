# =====================================================================
# Dockerfile
# Image de production pour le Public Health Survey Analytics Dashboard
# Base : image Rocker officielle avec R et les paquets systeme
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

FROM rocker/shiny:4.4.1

LABEL maintainer="Joseph ATEBA <atebajoseph047@gmail.com>"
LABEL description="Public Health Survey Analytics Dashboard (R Shiny)"

# Dependances systeme necessaires a la compilation des paquets R
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Paquets R
RUN R -e "install.packages(c( \
    'shiny', 'shinydashboard', 'shinydashboardPlus', 'shinyWidgets', \
    'shinyjs', 'shinycssloaders', 'fresh', 'bslib', 'fontawesome', \
    'dplyr', 'tidyr', 'readr', 'tibble', 'purrr', 'stringr', 'forcats', \
    'ggplot2', 'plotly', 'scales', 'corrplot', 'GGally', \
    'DT', 'knitr', 'kableExtra', \
    'broom', 'car', \
    'survival', 'survminer', \
    'survey', \
    'naniar', 'visdat', \
    'rmarkdown', 'haven', 'foreign' \
    ), repos = 'https://cloud.r-project.org', Ncpus = 4)"

# Copie de l'application
COPY . /srv/shiny-server/app/

WORKDIR /srv/shiny-server/app

# Droits d'acces
RUN chown -R shiny:shiny /srv/shiny-server/app

# Port expose par shiny-server
EXPOSE 3838

USER shiny

CMD ["/usr/bin/shiny-server"]
