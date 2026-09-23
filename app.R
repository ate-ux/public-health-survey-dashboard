# =====================================================================
# app.R
# Public Health Survey Analytics Dashboard
# Tableau de bord d'analyse d'enquete en sante publique (R Shiny)
#
# Auteur  : Joseph ATEBA, Data Scientist
# Contact : atebajoseph047@gmail.com | +237 670 211 522
# Donnees : NHANES 2017-2018 (National Health and Nutrition Examination
#           Survey, centres CDC, domaine public)
# =====================================================================

# ---------------------------------------------------------------------
# Librairies
# ---------------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(fresh)
  library(scales)
})

# ---------------------------------------------------------------------
# Modules internes
# ---------------------------------------------------------------------
source("R/data_cleaning.R")
source("R/descriptive_analysis.R")
source("R/regression.R")
source("R/survival.R")
source("R/tests_avances.R")
source("R/survey_analysis.R")
source("R/report_generation.R")

# ---------------------------------------------------------------------
# Palette et theme
# ---------------------------------------------------------------------
PALETTE <- list(
  primaire   = "#1f6f8b",
  secondaire = "#e07a5f",
  accent     = "#3d5a80",
  vert       = "#2a9d8f",
  fond       = "#f4f7f9",
  texte      = "#22333b"
)

theme_sante <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", colour = PALETTE$texte, size = base_size + 1),
      plot.subtitle = ggplot2::element_text(colour = "#5c6b73"),
      axis.title   = ggplot2::element_text(colour = PALETTE$texte),
      axis.text    = ggplot2::element_text(colour = "#455a64"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e3eaee"),
      legend.title = ggplot2::element_text(face = "bold")
    )
}

# ---------------------------------------------------------------------
# Jeu de donnees par defaut
# ---------------------------------------------------------------------
CHEMIN_DONNEES <- "data/nhanes_survey.csv"
CHEMIN_CODEBOOK <- "data/codebook.csv"

donnees_defaut <- function() {
  if (file.exists(CHEMIN_DONNEES)) charger_csv(CHEMIN_DONNEES) else NULL
}
codebook_defaut <- function() charger_codebook(CHEMIN_CODEBOOK)

# ---------------------------------------------------------------------
# Interface utilisateur
# ---------------------------------------------------------------------
entete <- dashboardHeader(
  title = tagList(
    span(class = "logo-mini", "PH"),
    span(class = "logo-lg", "Public Health Survey Analytics")
  ),
  titleWidth = 340
)

barre_laterale <- dashboardSidebar(
  width = 340,
  sidebarMenu(
    id = "menu",
    menuItem("Tableau de bord", tabName = "dashboard", icon = icon("gauge-high")),
    menuItem("Exploration des donnees", tabName = "exploration", icon = icon("magnifying-glass-chart")),
    menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-column")),
    menuItem("Statistiques descriptives", tabName = "descriptives", icon = icon("calculator")),
    menuItem("Analyse bivariee", tabName = "bivarie", icon = icon("table-cells-large")),
    menuItem("Regression logistique", tabName = "regression", icon = icon("chart-line")),
    menuItem("Tests statistiques", tabName = "tests", icon = icon("flask-vial")),
    menuItem("Analyse de survie", tabName = "survie", icon = icon("heart-pulse")),
    menuItem("Diagnostic des donnees", tabName = "diagnostic", icon = icon("stethoscope")),
    menuItem("Tableau filtrable", tabName = "tableau", icon = icon("table")),
    menuItem("Plan de sondage", tabName = "plansondage", icon = icon("layer-group")),
    menuItem("Import et export", tabName = "importexport", icon = icon("file-arrow-up"))
  ),
  hr(),
  div(style = "padding: 0 16px 16px 16px;",
      h5("Source des donnees", style = "color:#cfd8dc;"),
      radioButtons("source_donnees", NULL,
                   choices = c("NHANES 2017-2018 (integre)" = "integre",
                               "Importer un CSV" = "csv"),
                   selected = "integre"),
      conditionalPanel(
        condition = "input.source_donnees == 'csv'",
        fileInput("fichier_csv", "Fichier CSV", accept = ".csv"),
        checkboxInput("entete_csv", "Premiere ligne = entetes", value = TRUE),
        selectInput("separateur_csv", "Separateur",
                    choices = c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t"),
                    selected = ",")
      ),
      hr(),
      actionButton("reinitialiser", "Reinitialiser les filtres",
                   icon = icon("rotate-left"), width = "100%",
                   class = "btn-default")
  )
)

corps <- dashboardBody(
  use_theme(
    create_theme(
      adminlte_global(
        content_bg = PALETTE$fond,
        box_bg = "#ffffff",
        info_box_bg = "#ffffff"
      ),
      adminlte_sidebar(
        width = "340px",
        dark_bg = "#22333b",
        dark_hover_bg = "#1f6f8b",
        dark_color = "#cfd8dc"
      ),
      adminlte_vars(
        "body-bg" = PALETTE$fond,
        "font-size-base" = "14px"
      )
    )
  ),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),

  # ===================================================================
  # Onglet Tableau de bord
  # ===================================================================
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("vb_obs", width = 3),
        valueBoxOutput("vb_vars", width = 3),
        valueBoxOutput("vb_manquant", width = 3),
        valueBoxOutput("vb_complet", width = 3)
      ),
      fluidRow(
        box(
          title = "Filtres rapides du tableau de bord", width = 12,
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            column(3, uiOutput("filtre_age_dash")),
            column(3, uiOutput("filtre_sexe_dash")),
            column(3, uiOutput("filtre_bmi_dash")),
            column(3, uiOutput("filtre_ethnie_dash"))
          )
        )
      ),
      fluidRow(
        box(title = "Indicateurs cles de sante",
            width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("dash_kpi_bar", height = 330))),
        box(title = "Repartition par sexe et categorie d'IMC",
            width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("dash_bmi_sexe", height = 330)))
      ),
      fluidRow(
        box(title = "Prevalence des facteurs de risque",
            width = 7, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("dash_prevalence"))),
        box(title = "Profil de l'echantillon filtre",
            width = 5, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("dash_profil")))
      )
    ),

    # ===================================================================
    # Onglet Exploration
    # ===================================================================
    tabItem(
      tabName = "exploration",
      fluidRow(
        box(title = "Structure du jeu de donnees", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("expl_structure")))
      ),
      fluidRow(
        box(title = "Distribution des types de variables", width = 5,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("expl_types", height = 320))),
        box(title = "Apercu des premieres lignes", width = 7,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("expl_apercu")))
      ),
      fluidRow(
        box(title = "Etude d'une variable", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, uiOutput("expl_var_select")),
              column(8, withSpinner(uiOutput("expl_var_resume")))
            ),
            withSpinner(DTOutput("expl_var_table")))
      )
    ),

    # ===================================================================
    # Onglet Visualisations
    # ===================================================================
    tabItem(
      tabName = "visualisations",
      fluidRow(
        box(title = "Parametres du graphique", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("viz_type", "Type de graphique",
                                    choices = c("Histogramme" = "hist",
                                                "Boite a moustaches" = "box",
                                                "Diagramme en barres" = "bar",
                                                "Camembert" = "pie",
                                                "Nuage de points" = "scatter",
                                                "Barres croisees" = "croisees",
                                                "Moyennes avec intervalle de confiance" = "moyennes",
                                                "Matrice de correlation" = "corr",
                                                "Valeurs manquantes" = "manquants"))),
              column(4, uiOutput("viz_var1")),
              column(4, uiOutput("viz_var2")),
              column(4, uiOutput("viz_groupe")),
              column(4, checkboxInput("viz_plotly", "Rendre interactif (plotly)", value = TRUE)),
              column(4, numericInput("viz_bins", "Nombre de classes (histogramme)", 30, 5, 100))
            )
        )
      ),
      fluidRow(
        box(title = "Graphique", width = 12, status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("viz_sortie")))
      )
    ),

    # ===================================================================
    # Onglet Statistiques descriptives
    # ===================================================================
    tabItem(
      tabName = "descriptives",
      fluidRow(
        box(title = "Selection des variables", width = 12,
            status = "primary", solidHeader = TRUE,
            uiOutput("desc_var_select"))
      ),
      fluidRow(
        box(title = "Variables quantitatives", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("desc_continu")))
      ),
      fluidRow(
        box(title = "Frequences des variables qualitatives", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("desc_discret")))
      ),
      fluidRow(
        box(title = "Synthese automatique", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("desc_synthese")))
      )
    ),

    # ===================================================================
    # Onglet Analyse bivariee
    # ===================================================================
    tabItem(
      tabName = "bivarie",
      fluidRow(
        box(title = "Choix des variables", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6, uiOutput("biv_var1")),
              column(6, uiOutput("biv_var2"))
            )
        )
      ),
      fluidRow(
        box(title = "Tableau croise", width = 6,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("biv_table"))),
        box(title = "Graphique", width = 6,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("biv_graph", height = 360)))
      ),
      fluidRow(
        box(title = "Tests statistiques et interpretation", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("biv_tests")))
      )
    ),

    # ===================================================================
    # Onglet Regression logistique
    # ===================================================================
    tabItem(
      tabName = "regression",
      fluidRow(
        box(title = "Parametres du modele", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, uiOutput("reg_outcome")),
              column(4, uiOutput("reg_predicteurs")),
              column(4,
                     sliderInput("reg_seuil", "Seuil de classification", 0.05, 0.95, 0.5, 0.05),
                     checkboxInput("reg_stepwise", "Selection pas a pas (AIC)", value = FALSE))
            ),
            actionButton("reg_lancer", "Ajuster le modele",
                         icon = icon("play"), class = "btn-primary")
        )
      ),
      fluidRow(
        box(title = "Coefficients et rapports de cotes", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("reg_coefs")))
      ),
      fluidRow(
        valueBoxOutput("reg_n", width = 3),
        valueBoxOutput("reg_events", width = 3),
        valueBoxOutput("reg_auc", width = 3),
        valueBoxOutput("reg_nagelkerke", width = 3)
      ),
      fluidRow(
        box(title = "Courbe ROC", width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("reg_roc", height = 360))),
        box(title = "Qualite d'ajustement", width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("reg_qualite")))
      ),
      fluidRow(
        box(title = "Matrice de confusion", width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("reg_confusion"))),
        box(title = "Distribution des probabilites predites", width = 6,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("reg_proba", height = 320)))
      ),
      fluidRow(
        box(title = "Diagnostic du modele", width = 12,
            status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            fluidRow(
              column(6, withSpinner(plotlyOutput("reg_residus", height = 320))),
              column(6, withSpinner(plotlyOutput("reg_cook", height = 320)))
            ),
            h4("Multicolinearite (VIF)"),
            withSpinner(DTOutput("reg_vif")),
            h4("Points influents"),
            withSpinner(DTOutput("reg_influents")))
      ),
      fluidRow(
        box(title = "Interpretation automatique", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("reg_interpretation")))
      )
    ),

    # ===================================================================
    # Onglet Tests statistiques
    # ===================================================================
    tabItem(
      tabName = "tests",
      fluidRow(
        box(title = "Batterie de tests", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6, uiOutput("test_var1")),
              column(6, uiOutput("test_var2"))
            ),
            actionButton("test_lancer", "Lancer les tests",
                         icon = icon("flask-vial"), class = "btn-primary"),
            helpText("Les tests proposes s'adaptent automatiquement aux types des deux variables choisies.")
        )
      ),
      fluidRow(
        box(title = "Resultats des tests", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("test_resultats")))
      ),
      fluidRow(
        box(title = "Graphique associe", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("test_graph", height = 380)))
      )
    ),

    # ===================================================================
    # Onglet Analyse de survie
    # ===================================================================
    tabItem(
      tabName = "survie",
      fluidRow(
        box(title = "Parametres de l'analyse de survie", width = 12,
            status = "primary", solidHeader = TRUE,
            helpText("Cette analyse estime le temps jusqu'a un evenement a partir d'une variable de suivi quantitative et d'une variable binaire d'evenement."),
            fluidRow(
              column(4, uiOutput("surv_temps")),
              column(4, uiOutput("surv_evenement")),
              column(4, uiOutput("surv_groupe"))
            ),
            actionButton("surv_lancer", "Estimer la survie",
                         icon = icon("heart-pulse"), class = "btn-primary")
        )
      ),
      fluidRow(
        box(title = "Survie mediane", width = 5, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("surv_mediane"))),
        box(title = "Test du log-rank", width = 7, status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("surv_logrank")))
      ),
      fluidRow(
        box(title = "Courbes de survie Kaplan-Meier", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("surv_courbe", height = 450)))
      ),
      fluidRow(
        box(title = "Modele de Cox", width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("surv_cox"))),
        box(title = "Hypothese des risques proportionnels", width = 6,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("surv_ph")))
      )
    ),

    # ===================================================================
    # Onglet Diagnostic des donnees
    # ===================================================================
    tabItem(
      tabName = "diagnostic",
      fluidRow(
        box(title = "Resume de la qualite", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("diag_resume")))
      ),
      fluidRow(
        box(title = "Completude par variable", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("diag_completude")))
      ),
      fluidRow(
        box(title = "Valeurs aberrantes (regle de Tukey, 1.5 x IQR)", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("diag_k", "Coefficient k",
                                    choices = c("1.5 (standard)" = 1.5,
                                                "2.0" = 2, "3.0 (extreme)" = 3),
                                    selected = 1.5)),
              column(3, br(), downloadButton("export_classement", "Exporter la qualite"))
            ),
            withSpinner(DTOutput("diag_outliers")))
      ),
      fluidRow(
        box(title = "Controle de coherence interne", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("diag_coherence")))
      ),
      fluidRow(
        box(title = "Valeurs extremes observees", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("diag_extremes")))
      ),
      fluidRow(
        box(title = "Traitement des valeurs manquantes", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("diag_strategie", "Strategie",
                                    choices = c("Aucune" = "aucune",
                                                "Supprimer les lignes incompletes" = "supprimer",
                                                "Imputer par la mediane" = "imputer_mediane",
                                                "Imputer par le mode" = "imputer_mode"))),
              column(4, br(), actionButton("diag_appliquer", "Appliquer",
                                           icon = icon("broom"), class = "btn-warning")),
              column(4, br(), textOutput("diag_message"))
            ))
      )
    ),

    # ===================================================================
    # Onglet Tableau filtrable
    # ===================================================================
    tabItem(
      tabName = "tableau",
      fluidRow(
        box(title = "Filtres", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, uiOutput("tab_filtre_cat")),
              column(3, uiOutput("tab_filtre_cat_val")),
              column(3, uiOutput("tab_filtre_num")),
              column(3, uiOutput("tab_filtre_num_range"))
            ),
            fluidRow(
              column(6, uiOutput("tab_colonnes"))
            )
        )
      ),
      fluidRow(
        box(title = "Donnees filtrees", width = 12, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("tab_tableau")))
      )
    ),

    # ===================================================================
    # Onglet Import et export
    # ===================================================================
    # ===================================================================
    # Onglet Plan de sondage
    # ===================================================================
    tabItem(
      tabName = "plansondage",
      fluidRow(
        box(title = "Plan de sondage complexe NHANES", width = 12,
            status = "primary", solidHeader = TRUE,
            p("Les enquetes nationales reposent sur un echantillonnage stratifie et
               en grappes. Ignorer les poids, strates et unites primaires fausse les
               prevalences et sous-estime les intervalles de confiance."),
            withSpinner(uiOutput("plan_resume"))
        )
      ),
      fluidRow(
        box(title = "Parametres", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6, uiOutput("plan_poids")),
              column(6, uiOutput("plan_variables"))
            ),
            actionButton("plan_lancer", "Calculer les estimations ponderees",
                         icon = icon("play"), class = "btn-primary")
        )
      ),
      fluidRow(
        box(title = "Prevalences : brute et ponderee", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("plan_prevalences")))
      ),
      fluidRow(
        box(title = "Comparaison graphique", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("plan_graph_prevalences", height = 420)))
      ),
      fluidRow(
        box(title = "Moyennes et medianes ponderees", width = 12,
            status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("plan_moyennes")))
      ),
      fluidRow(
        box(title = "Effet de plan (design effect)", width = 12,
            status = "primary", solidHeader = TRUE,
            p("Un effet de plan superieur a 1 indique que l'echantillonnage complexe
               reduit la precision par rapport a un echantillon aleatoire simple de
               meme taille."),
            withSpinner(DTOutput("plan_effets")))
      ),
      fluidRow(
        box(title = "Prevalence par domaine", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, uiOutput("plan_dom_variable")),
              column(4, uiOutput("plan_dom_groupe")),
              column(4, br(), br(),
                     actionButton("plan_dom_lancer", "Calculer",
                                  icon = icon("play"), class = "btn-primary"))
            ),
            withSpinner(DTOutput("plan_domaine")),
            br(),
            withSpinner(plotlyOutput("plan_graph_domaine", height = 400)))
      ),
      fluidRow(
        box(title = "Tests d'association tenant compte du plan", width = 12,
            status = "primary", solidHeader = TRUE,
            p("Le chi-deux de Rao-Scott corrige la statistique usuelle par l'effet
               de plan. Le test F de Rao-Scott est prefere lorsque le nombre
               d'unites primaires est limite."),
            withSpinner(DTOutput("plan_rao_scott")))
      ),
      fluidRow(
        box(title = "Regression logistique ponderee", width = 12,
            status = "primary", solidHeader = TRUE,
            p("Modele estime par quasi-vraisemblance, avec erreurs standards
               tenant compte des strates et des grappes."),
            fluidRow(
              column(4, uiOutput("plan_reg_outcome")),
              column(4, uiOutput("plan_reg_predicteurs")),
              column(4, br(), br(),
                     actionButton("plan_reg_lancer", "Ajuster le modele",
                                  icon = icon("play"), class = "btn-primary"))
            ),
            withSpinner(DTOutput("plan_reg_coefs")),
            br(),
            withSpinner(uiOutput("plan_reg_qualite")))
      ),
      fluidRow(
        box(title = "Pourquoi ponderer", width = 12,
            status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            h4("Le plan de sondage NHANES"),
            p("NHANES selectionne les participants en deux etapes. Des comtes
               geographiques sont d'abord tires, puis des menages et des personnes
               sont selectionnes dans chacun. Chaque repondant recoit un poids qui
               indique combien de personnes de la population il represente."),
            h4("Les trois elements"),
            tags$ul(
              tags$li(strong("Poids (WTMEC2YR) : "), "inverse de la probabilite de
                      selection, ajuste pour la non-reponse. La somme des poids
                      estime la population americaine civile non institutionnalisee."),
              tags$li(strong("Strates (SDMVSTRA) : "), "groupes formes pour garantir
                      une representation geographique. La variance se calcule entre
                      strates."),
              tags$li(strong("Unites primaires (SDMVPSU) : "), "comtes tires dans
                      chaque strate. Elles capturent la correlation entre personnes
                      d'un meme lieu.")
            ),
            h4("Ce que cela change"),
            p("Avec ces donnees, l'ecart entre estimation brute et ponderee atteint
               plus de 6 points pour l'hypertension. L'effet de plan depasse 2 pour
               plusieurs indicateurs, ce qui signifie que l'intervalle de confiance
               reel est plus de deux fois plus large que celui d'un echantillon
               aleatoire simple. Ne pas en tenir compte revient a surestimer la
               precision des resultats."),
            h4("Limites"),
            p("La variance est estimee avec le plan fourni. Les regles de degres de
               liberte reposent sur le nombre d'unites primaires moins le nombre de
               strates, soit ici 15 strates. Pour une publication, completez par
               l'analyse de sous-populations avec les commandes dediees du paquet
               survey, afin de controler finement les domaines d'estimation.")
        )
      )
    ),

    tabItem(
      tabName = "importexport",
      fluidRow(
        box(title = "Import de donnees", width = 6, status = "primary", solidHeader = TRUE,
            p("Pour importer vos propres donnees, choisissez \"Importer un CSV\" dans le panneau de gauche."),
            tags$ul(
              tags$li("Premiere ligne : noms des colonnes"),
              tags$li("Valeurs manquantes acceptees : vide, NA, N/A, 999, 9999"),
              tags$li("Encodage recommande : UTF-8"),
              tags$li("Les variables binaires doivent etre codees 0/1 pour la regression")
            ),
            uiOutput("imp_resume")
        ),
        box(title = "Export des resultats", width = 6, status = "primary", solidHeader = TRUE,
            p("Exportez les donnees filtrees, le dictionnaire ou un rapport d'analyse complet."),
            downloadButton("export_donnees", "Donnees filtrees (CSV)", class = "btn-primary"),
            br(), br(),
            downloadButton("export_codebook", "Dictionnaire des variables (CSV)"),
            br(), br(),
            downloadButton("export_rapport_md", "Rapport d'analyse (Markdown)"),
            br(), br(),
            downloadButton("export_rapport_html", "Rapport d'analyse (HTML)"),
            br(), br(),
            downloadButton("export_resume_stats", "Statistiques descriptives (CSV)")
        )
      ),
      fluidRow(
        box(title = "A propos de l'application", width = 12,
            status = "primary", solidHeader = TRUE,
            h4("Public Health Survey Analytics Dashboard"),
            p("Application d'analyse d'enquete en sante publique developpee avec R Shiny."),
            tags$ul(
              tags$li("Donnees de demonstration : NHANES 2017-2018 (CDC, domaine public)"),
              tags$li("Import de fichiers CSV personnalises"),
              tags$li("Statistiques descriptives, analyse bivariee, regression logistique, tests avances et analyse de survie"),
              tags$li("Export des resultats et rapport d'analyse automatise")
            ),
            hr(),
            p(strong("Auteur : "), "Joseph ATEBA, Data Scientist"),
            p(strong("Contact : "), "atebajoseph047@gmail.com | +237 670 211 522")
        )
      )
    )
  )
)

ui <- dashboardPage(
  skin = "blue",
  entete,
  barre_laterale,
  corps
)

# ---------------------------------------------------------------------
# Logique serveur (definie dans R/server.R)
# ---------------------------------------------------------------------
# source() place la fonction serveur dans l'environnement appelant. On la
# rattache explicitement a l'environnement de l'application pour qu'elle
# accede aux fonctions definies dans ce fichier et dans les modules.
.env_app <- environment()
source("R/server.R")
environment(server) <- .env_app

# ---------------------------------------------------------------------
# Lancement
# ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
