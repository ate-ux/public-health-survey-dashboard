# =====================================================================
# dependances.R
# Installation des dependances du projet
# Usage : Rscript dependances.R
# =====================================================================

paquets <- c(
  # Application et interface
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
  "shinyjs", "shinycssloaders", "fresh", "bslib", "fontawesome",
  # Donnees
  "dplyr", "tidyr", "readr", "tibble", "purrr", "stringr", "forcats",
  # Visualisation
  "ggplot2", "plotly", "scales", "corrplot", "GGally",
  # Tableaux
  "DT", "knitr", "kableExtra",
  # Statistiques
  "broom", "car",
  # Survie
  "survival", "survminer",
  # Plan de sondage
  "survey",
  # Diagnostic
  "naniar", "visdat",
  # Rapports
  "rmarkdown",
  # Import de formats externes
  "haven", "foreign"
)

installer_si_absent <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Installation de ", p, "...")
    install.packages(p, repos = "https://cloud.r-project.org")
  } else {
    message(p, " est deja installe.")
  }
}

invisible(lapply(paquets, installer_si_absent))
message("\nToutes les dependances sont installees.")
