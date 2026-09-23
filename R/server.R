# =====================================================================
# R/server.R
# Logique serveur de l'application Shiny
# Ce fichier est source par app.R.
# Projet : Public Health Survey Analytics Dashboard
# Auteur  : Joseph ATEBA, Data Scientist
# =====================================================================

server <- function(input, output, session) {

  # -------------------------------------------------------------------
  # Donnees reactives
  # -------------------------------------------------------------------

  # Donnees actives (integre ou importees)
  donnees_actives <- reactiveVal(donnees_defaut())
  codebook_actif  <- reactiveVal(codebook_defaut())
  # Version de travail apres nettoyage
  donnees_travail <- reactiveVal(NULL)
  # Message de diagnostic
  message_diag <- reactiveVal("")

  observe({
    # Charger le CSV importe
    if (identical(input$source_donnees, "csv")) {
      req(input$fichier_csv)
      sep <- input$separateur_csv
      d <- tryCatch(
        charger_csv(input$fichier_csv$datapath, sep = sep),
        error = function(e) NULL
      )
      if (is.null(d)) {
        showNotification("Le fichier n'a pas pu etre lu.", type = "error")
        return()
      }
      donnees_actives(d)
      codebook_actif(NULL)
    } else {
      donnees_actives(donnees_defaut())
      codebook_actif(codebook_defaut())
    }
  })

  # Donnees de base (avant filtres globaux)
  observeEvent(donnees_actives(), {
    donnees_travail(donnees_actives())
    message_diag("")
  })

  # Reinitialiser les filtres
  observeEvent(input$reinitialiser, {
    donnees_travail(donnees_actives())
    message_diag("Aucun traitement applique.")
    showNotification("Les donnees ont ete reinitialisees.", type = "message")
  })

  # Fonction utilitaire : donnees courantes
  df_courant <- reactive({
    d <- donnees_travail()
    req(d)
    d
  })

  types_courants <- reactive({
    detecter_types(df_courant())
  })

  vars_continues <- reactive({
    t <- types_courants()
    names(t)[t == "continue"]
  })

  vars_discretes <- reactive({
    t <- types_courants()
    names(t)[t %in% c("binaire", "categorielle")]
  })

  vars_binaires <- reactive({
    d <- df_courant()
    t <- types_courants()
    cand <- names(t)[t == "binaire"]
    # Ne garder que les binaires reellement codees 0/1 pour la regression
    cand[vapply(cand, function(v) {
      x <- na.omit(d[[v]])
      length(x) > 0 && all(x %in% c(0, 1))
    }, logical(1))]
  })

  # -------------------------------------------------------------------
  # Tableau de bord
  # -------------------------------------------------------------------

  output$vb_obs <- renderValueBox({
    d <- df_courant()
    valueBox(format(nrow(d), big.mark = " "), "Observations",
             icon = icon("users"), color = "aqua")
  })

  output$vb_vars <- renderValueBox({
    d <- df_courant()
    valueBox(ncol(d), "Variables",
             icon = icon("list"), color = "blue")
  })

  output$vb_manquant <- renderValueBox({
    d <- df_courant()
    pct <- round(mean(is.na(d)) * 100, 2)
    valueBox(paste0(pct, "%"), "Valeurs manquantes",
             icon = icon("circle-exclamation"), color = if (pct > 20) "red" else "green")
  })

  output$vb_complet <- renderValueBox({
    d <- df_courant()
    n <- sum(colSums(is.na(d)) == 0)
    valueBox(n, "Colonnes completes",
             icon = icon("circle-check"), color = "green")
  })

  # Filtres du tableau de bord
  output$filtre_age_dash <- renderUI({
    d <- df_courant()
    if (!"RIDAGEYR" %in% names(d)) return(NULL)
    sliderInput("dash_age", "Age",
                min = floor(min(d$RIDAGEYR, na.rm = TRUE)),
                max = ceiling(max(d$RIDAGEYR, na.rm = TRUE)),
                value = range(d$RIDAGEYR, na.rm = TRUE), step = 1)
  })

  output$filtre_sexe_dash <- renderUI({
    d <- df_courant()
    if (!"RIAGENDR" %in% names(d)) return(NULL)
    pickerInput("dash_sexe", "Sexe",
                choices = sort(unique(na.omit(d$RIAGENDR))),
                selected = sort(unique(na.omit(d$RIAGENDR))),
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })

  output$filtre_bmi_dash <- renderUI({
    d <- df_courant()
    if (!"BMI_cat" %in% names(d)) return(NULL)
    pickerInput("dash_bmi", "Categorie d'IMC",
                choices = levels(droplevels(as.factor(d$BMI_cat))),
                selected = levels(droplevels(as.factor(d$BMI_cat))),
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })

  output$filtre_ethnie_dash <- renderUI({
    d <- df_courant()
    if (!"RIDRETH1" %in% names(d)) return(NULL)
    pickerInput("dash_ethnie", "Origine ethnique",
                choices = sort(unique(na.omit(d$RIDRETH1))),
                selected = sort(unique(na.omit(d$RIDRETH1))),
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })

  # Donnees filtrees du tableau de bord
  df_dash <- reactive({
    d <- df_courant()
    if (!is.null(input$dash_age) && "RIDAGEYR" %in% names(d))
      d <- d[d$RIDAGEYR >= input$dash_age[1] & d$RIDAGEYR <= input$dash_age[2], ]
    if (!is.null(input$dash_sexe) && "RIAGENDR" %in% names(d))
      d <- d[d$RIAGENDR %in% input$dash_sexe, ]
    if (!is.null(input$dash_bmi) && "BMI_cat" %in% names(d))
      d <- d[as.character(d$BMI_cat) %in% input$dash_bmi, ]
    if (!is.null(input$dash_ethnie) && "RIDRETH1" %in% names(d))
      d <- d[d$RIDRETH1 %in% input$dash_ethnie, ]
    d
  })

  output$dash_kpi_bar <- renderPlotly({
    d <- df_dash()
    kpis <- list()
    if ("hypertension" %in% names(d))
      kpis[["Hypertension"]] <- mean(d$hypertension == 1, na.rm = TRUE) * 100
    if ("diabetes" %in% names(d))
      kpis[["Diabete"]] <- mean(d$diabetes == 1, na.rm = TRUE) * 100
    if ("current_smoker" %in% names(d))
      kpis[["Tabagisme actuel"]] <- mean(d$current_smoker == 1, na.rm = TRUE) * 100
    if ("BMI_cat" %in% names(d))
      kpis[["Obesite (IMC >= 30)"]] <- mean(as.character(d$BMI_cat) == "Obésité", na.rm = TRUE) * 100
    if ("DPQ_total" %in% names(d))
      kpis[["Symptomes depressifs (PHQ-9 >= 10)"]] <- mean(d$DPQ_total >= 10, na.rm = TRUE) * 100
    if ("HIQ011" %in% names(d))
      kpis[["Sans assurance sante"]] <- mean(d$HIQ011 == 2, na.rm = TRUE) * 100
    if (length(kpis) == 0) return(plotly_empty())
    kd <- data.frame(Indicateur = names(kpis), Preval = round(unlist(kpis), 1))
    kd$Indicateur <- factor(kd$Indicateur, levels = kd$Indicateur[order(kd$Preval)])
    p <- ggplot(kd, aes(x = Preval, y = Indicateur, fill = Indicateur)) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = paste0(Preval, "%")), hjust = -0.15, size = 3.4) +
      scale_fill_manual(values = rep(PALETTE$primaire, nrow(kd))) +
      labs(x = "Prevalence (%)", y = "", title = "Prevalence des indicateurs (echantillon filtre)") +
      theme_sante() + theme(legend.position = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2)))
    ggplotly(p, tooltip = "x") %>% config(displayModeBar = FALSE)
  })

  output$dash_bmi_sexe <- renderPlotly({
    d <- df_dash()
    if (!all(c("BMI_cat", "RIAGENDR") %in% names(d))) return(plotly_empty())
    tab <- as.data.frame(table(Categorie = d$BMI_cat, Sexe = d$RIAGENDR))
    tab <- tab[tab$Freq > 0, ]
    tab$Sexe <- factor(tab$Sexe, labels = c("Homme", "Femme")[seq_len(nlevels(as.factor(tab$Sexe)))])
    p <- ggplot(tab, aes(x = Categorie, y = Freq, fill = Sexe)) +
      geom_col(position = "dodge", alpha = 0.9) +
      scale_fill_manual(values = c(PALETTE$primaire, PALETTE$secondaire)) +
      labs(x = "Categorie d'IMC", y = "Effectif", fill = "Sexe",
           title = "Repartition de l'IMC selon le sexe") +
      theme_sante()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })

  output$dash_prevalence <- renderDT({
    d <- df_dash()
    vars <- intersect(c("hypertension", "diabetes", "current_smoker",
                        "MCQ160B", "MCQ160C", "MCQ160E", "MCQ160F", "MCQ220"),
                      names(d))
    if (length(vars) == 0) return(datatable(data.frame(Message = "Aucun indicateur disponible")))
    libelles <- c(hypertension = "Hypertension autorapportee",
                  diabetes = "Diabete autorapporte",
                  current_smoker = "Fumeur actuel",
                  MCQ160B = "Insuffisance cardiaque",
                  MCQ160C = "Maladie coronarienne",
                  MCQ160E = "Crise cardiaque",
                  MCQ160F = "Accident vasculaire cerebral",
                  MCQ220 = "Cancer")
    res <- do.call(rbind, lapply(vars, function(v) {
      x <- d[[v]]
      n_val <- sum(!is.na(x))
      # gerer les codages 1/2 et 0/1
      cas <- if (all(na.omit(x) %in% c(0, 1))) sum(x == 1, na.rm = TRUE) else sum(x == 1, na.rm = TRUE)
      data.frame(
        Indicateur = libelles[[v]],
        N_valide = n_val,
        Cas = cas,
        Prevalence_pct = round(cas / n_val * 100, 2),
        IC95_bas = round((cas / n_val - 1.96 * sqrt((cas / n_val) * (1 - cas / n_val) / n_val)) * 100, 2),
        IC95_haut = round((cas / n_val + 1.96 * sqrt((cas / n_val) * (1 - cas / n_val) / n_val)) * 100, 2),
        stringsAsFactors = FALSE
      )
    }))
    datatable(res, rownames = FALSE,
              options = list(dom = "t", pageLength = 10, autoWidth = TRUE)) %>%
      formatStyle("Prevalence_pct", background = styleColorBar(res$Prevalence_pct, "#cfe4ec"),
                  backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$dash_profil <- renderDT({
    d <- df_dash()
    lignes <- list(
      Observations = nrow(d),
      `Age median` = round(median(d$RIDAGEYR, na.rm = TRUE), 1),
      `Part de femmes (%)` = round(mean(d$RIAGENDR == 2, na.rm = TRUE) * 100, 1),
      `IMC moyen` = round(mean(d$BMXBMI, na.rm = TRUE), 1),
      `Tension systolique moyenne` = round(mean(d$SBP_mean, na.rm = TRUE), 1),
      `Score PHQ-9 moyen` = round(mean(d$DPQ_total, na.rm = TRUE), 1)
    )
    res <- data.frame(Caracteristique = names(lignes),
                      Valeur = unlist(lapply(lignes, function(x) as.character(x))),
                      row.names = NULL, stringsAsFactors = FALSE)
    datatable(res, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })

  # -------------------------------------------------------------------
  # Exploration
  # -------------------------------------------------------------------

  output$expl_structure <- renderDT({
    d <- df_courant(); t <- types_courants()
    cb <- codebook_actif()
    res <- data.frame(
      Variable = names(d),
      Classe_R = vapply(d, function(x) class(x)[1], character(1)),
      Type_detecte = unname(t),
      N_valides = vapply(d, function(x) sum(!is.na(x)), numeric(1)),
      N_uniques = vapply(d, function(x) length(unique(na.omit(x))), numeric(1)),
      stringsAsFactors = FALSE
    )
    if (!is.null(cb)) {
      res$Libelle <- cb$libelle[match(res$Variable, cb$variable)]
      res <- res[, c("Variable", "Libelle", "Classe_R", "Type_detecte", "N_valides", "N_uniques")]
    }
    datatable(res, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$expl_types <- renderPlotly({
    t <- types_courants()
    td <- as.data.frame(table(Type = t))
    p <- ggplot(td, aes(x = reorder(Type, -Freq), y = Freq, fill = Type)) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = Freq), vjust = -0.4, size = 3.6) +
      scale_fill_manual(values = c(continue = PALETTE$primaire, binaire = PALETTE$secondaire,
                                   categorielle = PALETTE$vert, id = "#adb5bd")) +
      labs(x = "", y = "Nombre de variables", title = "Types de variables detectees") +
      theme_sante() + theme(legend.position = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })

  output$expl_apercu <- renderDT({
    datatable(head(df_courant(), 100), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$expl_var_select <- renderUI({
    selectInput("expl_var", "Variable a etudier", choices = names(df_courant()))
  })

  output$expl_var_resume <- renderUI({
    req(input$expl_var)
    d <- df_courant()
    t <- types_courants()[input$expl_var]
    x <- d[[input$expl_var]]
    if (t == "continue") {
      s <- stats_continue(x)
      tagList(
        tags$table(class = "table table-sm",
          tags$tr(tags$th("N"), tags$td(s$N)),
          tags$tr(tags$th("Moyenne"), tags$td(s$Moyenne)),
          tags$tr(tags$th("Mediane"), tags$td(s$Mediane)),
          tags$tr(tags$th("Ecart-type"), tags$td(s$Ecart_type)),
          tags$tr(tags$th("IC 95% de la moyenne"),
                  tags$td(paste0("[", s$IC95_inf, " ; ", s$IC95_sup, "]")))
        )
      )
    } else {
      tagList(
        tags$table(class = "table table-sm",
          tags$tr(tags$th("N valides"), tags$td(sum(!is.na(x)))),
          tags$tr(tags$th("Manquants"), tags$td(sum(is.na(x)))),
          tags$tr(tags$th("Modalites"), tags$td(length(unique(na.omit(x)))))
        )
      )
    }
  })

  output$expl_var_table <- renderDT({
    req(input$expl_var)
    d <- df_courant()
    t <- types_courants()[input$expl_var]
    x <- d[[input$expl_var]]
    if (t == "continue") {
      s <- stats_continue(x)
      res <- as.data.frame(t(s))
      names(res) <- names(s)
      res <- cbind(Statistique = rownames(res), res)
      rownames(res) <- NULL
      datatable(res, rownames = FALSE)
    } else {
      res <- stats_categorielle(x)
      datatable(res, rownames = FALSE,
                options = list(dom = "t", pageLength = 15))
    }
  })

  # -------------------------------------------------------------------
  # Visualisations
  # -------------------------------------------------------------------

  output$viz_var1 <- renderUI({
    selectInput("viz_v1", if (input$viz_type %in% c("scatter", "corr", "croisees", "moyennes"))
      "Variable 1" else "Variable", choices = names(df_courant()))
  })

  output$viz_var2 <- renderUI({
    req(input$viz_type)
    if (input$viz_type == "scatter")
      selectInput("viz_v2", "Variable Y", choices = vars_continues(), selected = vars_continues()[2])
    else if (input$viz_type == "corr")
      selectizeInput("viz_v2", "Variables pour la correlation",
                     choices = vars_continues(), selected = head(vars_continues(), 6),
                     multiple = TRUE)
    else if (input$viz_type %in% c("croisees", "moyennes"))
      selectInput("viz_v2", "Variable de groupe", choices = vars_discretes())
    else NULL
  })

  output$viz_groupe <- renderUI({
    req(input$viz_type)
    if (input$viz_type == "scatter")
      selectInput("viz_groupe", "Colorer par (facultatif)",
                  choices = c("Aucun" = "", vars_binaires()), selected = "")
    else if (input$viz_type == "box")
      selectInput("viz_groupe", "Grouper par (facultatif)",
                  choices = c("Aucun" = "", vars_discretes()), selected = "")
    else NULL
  })

  output$viz_sortie <- renderUI({
    req(input$viz_type)
    if (input$viz_type == "corr") plotlyOutput("viz_corr", height = 520)
    else plotlyOutput("viz_plot", height = 480)
  })

  graph_courant <- reactive({
    d <- df_courant(); req(input$viz_type)
    switch(input$viz_type,
      hist = { req(input$viz_v1); graph_histogramme(d, input$viz_v1, bins = input$viz_bins, couleur = PALETTE$primaire) },
      box = { req(input$viz_v1)
              grp <- if (is.null(input$viz_groupe) || input$viz_groupe == "") NULL else input$viz_groupe
              graph_boxplot(d, input$viz_v1, grp) },
      bar = { req(input$viz_v1); graph_barres(d, input$viz_v1) },
      pie = { req(input$viz_v1); graph_camembert(d, input$viz_v1) },
      scatter = { req(input$viz_v1, input$viz_v2)
                  grp <- if (is.null(input$viz_groupe) || input$viz_groupe == "") NULL else input$viz_groupe
                  graph_nuage(d, input$viz_v1, input$viz_v2, grp) },
      croisees = { req(input$viz_v1, input$viz_v2); graph_barres_croisees(d, input$viz_v1, input$viz_v2) },
      moyennes = { req(input$viz_v1, input$viz_v2); graph_moyennes_ic(d, input$viz_v1, input$viz_v2) },
      manquants = graph_manquants(d),
      NULL
    )
  })

  output$viz_plot <- renderPlotly({
    p <- graph_courant()
    if (is.null(p)) return(plotly_empty())
    p <- p + theme_sante()
    if (isTRUE(input$viz_plotly)) ggplotly(p) %>% config(displayModeBar = FALSE)
    else ggplotly(p + theme(plot.margin = margin(10, 10, 10, 10))) %>% config(displayModeBar = FALSE)
  })

  output$viz_corr <- renderPlotly({
    req(input$viz_v2)
    p <- graph_correlation(df_courant(), input$viz_v2)
    if (is.null(p)) return(plotly_empty())
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })

  # -------------------------------------------------------------------
  # Descriptives
  # -------------------------------------------------------------------

  output$desc_var_select <- renderUI({
    selectizeInput("desc_vars", "Variables a decrire",
                   choices = names(df_courant()),
                   selected = names(df_courant()),
                   multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  output$desc_continu <- renderDT({
    vars <- intersect(input$desc_vars %||% vars_continues(), vars_continues())
    res <- tableau_descriptif_continu(df_courant(), vars)
    if (is.null(res)) return(datatable(data.frame(Message = "Aucune variable quantitative selectionnee")))
    datatable(res, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$desc_discret <- renderDT({
    vars <- intersect(input$desc_vars %||% vars_discretes(), vars_discretes())
    res <- tableau_descriptif_discret(df_courant(), vars)
    if (is.null(res)) return(datatable(data.frame(Message = "Aucune variable qualitative selectionnee")))
    datatable(res, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })

  output$desc_synthese <- renderUI({
    d <- df_courant()
    q <- resume_qualite(d)
    s <- tableau_outliers(d)
    n_out <- if (is.null(s)) 0 else sum(s$N_outliers)
    HTML(sprintf(
      "<div class='synthese-box'>
        <p><strong>%s observations</strong> reparties sur <strong>%d variables</strong>.</p>
        <p>Le taux de valeurs manquantes global est de <strong>%.2f%%</strong>, avec %d colonnes totalement completes
        et %d colonnes presentant plus de la moitie de valeurs manquantes.</p>
        <p>Le controle par la regle de Tukey signale <strong>%d valeurs aberrantes</strong> au total sur les variables quantitatives.</p>
      </div>",
      format(q$n_lignes, big.mark = " "), q$n_colonnes, q$pct_manquant_global,
      q$colonnes_completes, q$colonnes_critiques, n_out))
  })

  # -------------------------------------------------------------------
  # Analyse bivariee
  # -------------------------------------------------------------------

  # Colonnes ecartees des analyses : identifiant et variables de plan
  cols_analysables <- function() {
    d <- df_courant()
    setdiff(names(d), c("SEQN", "SDMVPSU", "SDMVSTRA", "WTINT2YR", "WTMEC2YR",
                        "WTINT4YR", "WTMEC4YR"))
  }

  output$biv_var1 <- renderUI({
    ch <- cols_analysables()
    defaut <- if ("hypertension" %in% ch) "hypertension" else ch[1]
    selectInput("biv_v1", "Variable 1", choices = ch, selected = defaut)
  })
  output$biv_var2 <- renderUI({
    ch <- cols_analysables()
    defaut <- if ("RIAGENDR" %in% ch) "RIAGENDR" else ch[min(2, length(ch))]
    selectInput("biv_v2", "Variable 2", choices = ch, selected = defaut)
  })

  output$biv_table <- renderDT({
    req(input$biv_v1, input$biv_v2)
    tab <- tableau_croise(df_courant(), input$biv_v1, input$biv_v2)
    if (is.null(tab)) return(datatable(data.frame(Message = "Croisement impossible")))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$biv_graph <- renderPlotly({
    req(input$biv_v1, input$biv_v2)
    t1 <- types_courants()[input$biv_v1]
    t2 <- types_courants()[input$biv_v2]
    d <- df_courant()
    p <- NULL
    if (t1 == "continue" && t2 != "continue") p <- graph_boxplot(d, input$biv_v1, input$biv_v2)
    else if (t2 == "continue" && t1 != "continue") p <- graph_boxplot(d, input$biv_v2, input$biv_v1)
    else if (t1 == "continue" && t2 == "continue") p <- graph_nuage(d, input$biv_v1, input$biv_v2)
    else p <- graph_barres_croisees(d, input$biv_v1, input$biv_v2)
    if (is.null(p)) return(plotly_empty())
    ggplotly(p + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$biv_tests <- renderUI({
    req(input$biv_v1, input$biv_v2)
    d <- df_courant()
    t1 <- types_courants()[input$biv_v1]
    t2 <- types_courants()[input$biv_v2]
    blocs <- list()
    if (t1 == "continue" && t2 != "continue") {
      r <- test_moyennes(d, input$biv_v1, input$biv_v2)
      if (!is.null(r) && isTRUE(r$disponible)) {
        blocs[[1]] <- HTML(sprintf(
          "<div class='test-bloc'><h4>%s</h4>
           <p><strong>Statistique :</strong> %s &nbsp; <strong>ddl :</strong> %s &nbsp; <strong>p :</strong> %s</p>
           <p><strong>Homogeneite des variances :</strong> %s</p>
           <p class='interpretation'>%s</p></div>",
          r$test, r$statistique, r$ddl, r$p_formate,
          r$homogeneite_variances, r$interpretation))
        tbl <- datatable(r$descriptions, rownames = FALSE, options = list(dom = "t"))
        blocs[[2]] <- tbl
        ph <- test_posthoc_tukey(d, input$biv_v1, input$biv_v2)
        if (!is.null(ph)) {
          blocs[[3]] <- HTML("<h4>Comparaisons post-hoc (Tukey HSD)</h4>")
          blocs[[4]] <- datatable(ph, rownames = FALSE, options = list(pageLength = 10))
        }
      }
    } else if (t2 == "continue" && t1 != "continue") {
      r <- test_moyennes(d, input$biv_v2, input$biv_v1)
      if (!is.null(r) && isTRUE(r$disponible)) {
        blocs[[1]] <- HTML(sprintf(
          "<div class='test-bloc'><h4>%s</h4>
           <p><strong>Statistique :</strong> %s &nbsp; <strong>ddl :</strong> %s &nbsp; <strong>p :</strong> %s</p>
           <p class='interpretation'>%s</p></div>",
          r$test, r$statistique, r$ddl, r$p_formate, r$interpretation))
        blocs[[2]] <- datatable(r$descriptions, rownames = FALSE, options = list(dom = "t"))
      }
    } else if (t1 == "continue" && t2 == "continue") {
      r <- test_correlation_complet(d[[input$biv_v1]], d[[input$biv_v2]])
      if (!is.null(r)) {
        blocs[[1]] <- HTML(sprintf("<div class='test-bloc'><h4>Correlation entre %s et %s</h4></div>",
                                   input$biv_v1, input$biv_v2))
        blocs[[2]] <- datatable(r, rownames = FALSE, options = list(dom = "t"))
        blocs[[3]] <- HTML(sprintf("<p class='interpretation'>%s</p>", attr(r, "interpretation")))
      }
    } else {
      r <- test_chi2(d, input$biv_v1, input$biv_v2)
      if (!is.null(r) && isTRUE(r$disponible)) {
        blocs[[1]] <- HTML(sprintf(
          "<div class='test-bloc'><h4>Test du chi-deux d'independance</h4>
           <p><strong>Statistique :</strong> %s &nbsp; <strong>ddl :</strong> %s &nbsp; <strong>p :</strong> %s</p>
           <p><strong>V de Cramer :</strong> %s (%s)</p>
           <p class='interpretation'>%s</p></div>",
          r$statistique, r$ddl, r$p_formate, r$v_cramer, r$force_association, r$conclusion))
      } else if (!is.null(r)) {
        blocs[[1]] <- HTML(sprintf("<p class='interpretation'>%s</p>", r$message))
      }
    }
    if (length(blocs) == 0) return(HTML("<p>Aucun test disponible pour ce couple de variables.</p>"))
    do.call(tagList, blocs)
  })

  # -------------------------------------------------------------------
  # Regression logistique
  # -------------------------------------------------------------------

  output$reg_outcome <- renderUI({
    selectInput("reg_y", "Variable reponse binaire (0/1)",
                choices = vars_binaires())
  })

  output$reg_predicteurs <- renderUI({
    selectizeInput("reg_x", "Variables explicatives",
                   choices = setdiff(names(df_courant()), vars_binaires()[1]),
                   selected = intersect(c("RIDAGEYR", "RIAGENDR", "BMXBMI", "current_smoker"),
                                        names(df_courant())),
                   multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  modele_reg <- eventReactive(input$reg_lancer, {
    req(input$reg_y, input$reg_x)
    d <- df_courant()
    preds <- setdiff(input$reg_x, input$reg_y)
    res <- ajuster_logistique(d, input$reg_y, preds)
    if (!is.null(res$erreur)) {
      showNotification(res$erreur, type = "error", duration = 6)
      return(NULL)
    }
    if (isTRUE(input$reg_stepwise)) {
      sel <- selection_pas_a_pas(d, input$reg_y, preds)
      if (!is.null(sel)) {
        res <- ajuster_logistique(d, input$reg_y, sel$variables_retenues)
        showNotification(paste("Variables retenues :", paste(sel$variables_retenues, collapse = ", ")),
                         type = "message", duration = 6)
      }
    }
    res
  })

  output$reg_coefs <- renderDT({
    m <- modele_reg()
    if (is.null(m)) return(datatable(data.frame(Message = "Aucun modele estime")))
    tb <- tableau_coefficients(m$modele)
    datatable(tb, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Significatif",
                  backgroundColor = styleEqual(c("Oui", "Non"), c("#d9edf0", "#ffffff")))
  })

  output$reg_n <- renderValueBox({
    m <- modele_reg()
    val <- if (is.null(m)) " - " else format(m$n, big.mark = " ")
    valueBox(val, "Observations du modele", icon = icon("list-ol"), color = "aqua")
  })
  output$reg_events <- renderValueBox({
    m <- modele_reg()
    val <- if (is.null(m)) " - " else m$evenements
    valueBox(val, "Evenements (cas)", icon = icon("flag"), color = "green")
  })
  output$reg_auc <- renderValueBox({
    m <- modele_reg()
    if (is.null(m)) return(valueBox(" - ", "AUC", icon = icon("chart-area"), color = "yellow"))
    roc <- courbe_roc(m$modele)
    valueBox(round(attr(roc, "auc"), 3), "AUC (courbe ROC)", icon = icon("chart-area"), color = "yellow")
  })
  output$reg_nagelkerke <- renderValueBox({
    m <- modele_reg()
    if (is.null(m)) return(valueBox(" - ", "Pseudo R2 Nagelkerke", icon = icon("ruler"), color = "red"))
    q <- pseudo_r2(m$modele)
    valueBox(q$Nagelkerke, "Pseudo R2 Nagelkerke", icon = icon("ruler"), color = "red")
  })

  output$reg_roc <- renderPlotly({
    m <- modele_reg(); if (is.null(m)) return(plotly_empty())
    ggplotly(graph_roc(m$modele) + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$reg_proba <- renderPlotly({
    m <- modele_reg(); if (is.null(m)) return(plotly_empty())
    ggplotly(graph_probabilites(m$modele) + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$reg_residus <- renderPlotly({
    m <- modele_reg(); if (is.null(m)) return(plotly_empty())
    ggplotly(graph_residus(m$modele) + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$reg_cook <- renderPlotly({
    m <- modele_reg(); if (is.null(m)) return(plotly_empty())
    ggplotly(graph_cook(m$modele) + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$reg_qualite <- renderUI({
    m <- modele_reg()
    if (is.null(m)) return(HTML("<p>Lancez l'ajustement du modele pour afficher la qualite.</p>"))
    q <- pseudo_r2(m$modele)
    lr <- test_rapport_vraisemblance(m$modele)
    hl <- test_hosmer_lemeshow(m$modele)
    cls <- tableau_classification(m$modele, input$reg_seuil)
    HTML(sprintf(
      "<table class='table table-sm'>
        <tr><th>Pseudo R2 McFadden</th><td>%s</td></tr>
        <tr><th>Pseudo R2 Nagelkerke</th><td>%s</td></tr>
        <tr><th>AIC</th><td>%s</td></tr>
        <tr><th>BIC</th><td>%s</td></tr>
        <tr><th>Log-vraisemblance</th><td>%s</td></tr>
        <tr><th>Test du rapport de vraisemblance</th><td>chi2 = %s, ddl = %d, p = %s</td></tr>
        %s
        <tr><th>Exactitude (seuil %.2f)</th><td>%.1f%%</td></tr>
        <tr><th>Sensibilite</th><td>%.1f%%</td></tr>
        <tr><th>Specificite</th><td>%.1f%%</td></tr>
        <tr><th>Valeur predictive positive</th><td>%.1f%%</td></tr>
        <tr><th>F1</th><td>%s</td></tr>
      </table>
      <p class='interpretation'>%s</p>
      %s",
      q$McFadden, q$Nagelkerke, q$AIC, q$BIC, q$Log_vraisemblance,
      lr$statistique, lr$ddl, lr$p_formate,
      if (!is.null(hl)) sprintf("<tr><th>Hosmer-Lemeshow</th><td>chi2 = %s, p = %s</td></tr>",
                                hl$statistique, hl$p_formate) else "",
      cls$seuil, cls$exactitude * 100, cls$sensibilite * 100, cls$specificite * 100,
      cls$vpp * 100, cls$f1,
      lr$interpretation,
      if (!is.null(hl)) paste0("<p class='interpretation'>", hl$interpretation, "</p>") else ""
    ))
  })

  output$reg_confusion <- renderDT({
    m <- modele_reg()
    if (is.null(m)) return(datatable(data.frame(Message = "Aucun modele estime")))
    cls <- tableau_classification(m$modele, input$reg_seuil)
    cm <- as.data.frame.matrix(cls$matrice)
    cm <- cbind(Predit = rownames(cm), cm)
    rownames(cm) <- NULL
    datatable(cm, rownames = FALSE, options = list(dom = "t"))
  })

  output$reg_vif <- renderDT({
    m <- modele_reg()
    if (is.null(m)) return(datatable(data.frame(Message = "Aucun modele estime")))
    v <- vif_modele(m$modele)
    if (is.null(v)) return(datatable(data.frame(Message = "VIF non disponible (paquet car absent)")))
    datatable(v, rownames = FALSE, options = list(dom = "t"))
  })

  output$reg_influents <- renderDT({
    m <- modele_reg()
    if (is.null(m)) return(datatable(data.frame(Message = "Aucun modele estime")))
    pi <- points_influents(m$modele)
    d <- pi$donnees
    d <- d[d$Influent, , drop = FALSE]
    if (nrow(d) == 0) return(datatable(data.frame(Message = "Aucun point influent au seuil retenu")))
    d$Ajuste <- round(d$Ajuste, 4); d$Cook <- round(d$Cook, 5)
    datatable(d, rownames = FALSE, options = list(pageLength = 10))
  })

  output$reg_interpretation <- renderUI({
    m <- modele_reg()
    if (is.null(m)) return(HTML("<p>Ajustez le modele pour obtenir une interpretation automatique.</p>"))
    txt <- interpreter_coefficients(m$modele)
    HTML(paste0("<div class='interpretation-bloc'><pre style='white-space:pre-wrap;'>",
                txt, "</pre></div>"))
  })

  # -------------------------------------------------------------------
  # Tests statistiques
  # -------------------------------------------------------------------

  output$test_var1 <- renderUI({
    selectInput("test_v1", "Variable 1", choices = names(df_courant()))
  })
  output$test_var2 <- renderUI({
    d <- names(df_courant())
    selectInput("test_v2", "Variable 2", choices = d, selected = d[min(2, length(d))])
  })

  resultats_tests <- eventReactive(input$test_lancer, {
    req(input$test_v1, input$test_v2)
    batterie_tests(df_courant(), input$test_v1, input$test_v2)
  })

  output$test_resultats <- renderUI({
    res <- resultats_tests()
    if (is.null(res) || length(res) == 0)
      return(HTML("<p>Lancez l'analyse pour afficher les tests adaptes aux deux variables choisies.</p>"))
    blocs <- list()
    ajouter_tbl <- function(titre, tbl) {
      if (is.null(tbl)) return(NULL)
      list(HTML(paste0("<h4>", titre, "</h4>")), datatable(as.data.frame(tbl), rownames = FALSE,
                                                          options = list(dom = "t", pageLength = 10)))
    }
    if (!is.null(res$correlation)) {
      blocs <- c(blocs, ajouter_tbl("Tests de correlation", res$correlation),
                 list(HTML(sprintf("<p class='interpretation'>%s</p>", attr(res$correlation, "interpretation")))))
    }
    if (!is.null(res$normalite)) blocs <- c(blocs, ajouter_tbl("Test de normalite (Shapiro-Wilk)", res$normalite))
    if (!is.null(res$homogeneite)) blocs <- c(blocs, ajouter_tbl("Homogeneite des variances", res$homogeneite))
    if (!is.null(res$comparaison_2)) blocs <- c(blocs, ajouter_tbl("Comparaison de deux groupes", res$comparaison_2))
    if (!is.null(res$comparaison_k)) blocs <- c(blocs, ajouter_tbl("Comparaison de k groupes", res$comparaison_k))
    if (!is.null(res$posthoc)) blocs <- c(blocs, ajouter_tbl("Post-hoc Tukey HSD", res$posthoc))
    if (!is.null(res$association)) {
      blocs <- c(blocs, ajouter_tbl("Tests d'association", res$association))
    }
    do.call(tagList, blocs)
  })

  output$test_graph <- renderPlotly({
    res <- resultats_tests(); req(res)
    d <- df_courant()
    t1 <- types_courants()[input$test_v1]
    t2 <- types_courants()[input$test_v2]
    p <- NULL
    if (t1 == "continue" && t2 == "continue") p <- graph_nuage(d, input$test_v1, input$test_v2)
    else if (t1 == "continue") p <- graph_boxplot(d, input$test_v1, input$test_v2)
    else if (t2 == "continue") p <- graph_boxplot(d, input$test_v2, input$test_v1)
    else p <- graph_barres_croisees(d, input$test_v1, input$test_v2)
    if (is.null(p)) return(plotly_empty())
    ggplotly(p + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  # -------------------------------------------------------------------
  # Analyse de survie
  # -------------------------------------------------------------------

  output$surv_temps <- renderUI({
    selectInput("surv_t", "Variable de temps (suivi, > 0)",
                choices = vars_continues())
  })
  output$surv_evenement <- renderUI({
    selectInput("surv_e", "Variable evenement (0/1)", choices = vars_binaires())
  })
  output$surv_groupe <- renderUI({
    selectInput("surv_g", "Groupe (facultatif)",
                choices = c("Aucun" = "", vars_discretes()))
  })

  surv_data <- eventReactive(input$surv_lancer, {
    req(input$surv_t, input$surv_e)
    grp <- if (is.null(input$surv_g) || input$surv_g == "") NULL else input$surv_g
    preparer_survie(df_courant(), input$surv_t, input$surv_e, grp)
  })

  surv_fit <- reactive({
    sd <- surv_data(); req(sd)
    if (!is.null(sd$erreur)) return(NULL)
    estimer_km(sd$donnees, groupe = "group" %in% names(sd$donnees))
  })

  output$surv_mediane <- renderDT({
    f <- surv_fit()
    if (is.null(f)) return(datatable(data.frame(Message = "Lancez l'estimation")))
    datatable(survie_mediane(f), rownames = FALSE, options = list(dom = "t"))
  })

  output$surv_logrank <- renderUI({
    sd <- surv_data()
    if (is.null(sd) || !is.null(sd$erreur))
      return(HTML(paste0("<p>", if (!is.null(sd)) sd$erreur else "Lancez l'estimation", "</p>")))
    lr <- test_logrank(sd$donnees)
    if (is.null(lr)) return(HTML("<p class='interpretation'>Test du log-rank indisponible : au moins deux groupes sont necessaires.</p>"))
    HTML(sprintf("<div class='test-bloc'><h4>Test du log-rank</h4>
                  <p><strong>chi2 :</strong> %s &nbsp; <strong>ddl :</strong> %d &nbsp; <strong>p :</strong> %s</p>
                  <p class='interpretation'>%s</p></div>",
                 lr$statistique, lr$ddl, lr$p_formate, lr$interpretation))
  })

  output$surv_courbe <- renderPlotly({
    sd <- surv_data()
    if (is.null(sd) || !is.null(sd$erreur)) return(plotly_empty())
    f <- surv_fit(); req(f)
    p <- graph_survie(f, titre = "Courbes de survie Kaplan-Meier")
    if (is.null(p)) return(plotly_empty())
    ggplotly(p + theme_sante()) %>% config(displayModeBar = FALSE)
  })

  output$surv_cox <- renderDT({
    sd <- surv_data()
    if (is.null(sd) || !is.null(sd$erreur)) return(datatable(data.frame(Message = "Lancez l'estimation")))
    covs <- if ("group" %in% names(sd$donnees)) "group" else NULL
    cox <- ajuster_cox(sd$donnees, covs)
    if (is.null(cox) || !is.null(cox$erreur))
      return(datatable(data.frame(Message = "Modele de Cox indisponible")))
    datatable(cox$coefficients, rownames = FALSE, options = list(pageLength = 10))
  })

  output$surv_ph <- renderDT({
    sd <- surv_data()
    if (is.null(sd) || !is.null(sd$erreur)) return(datatable(data.frame(Message = "Lancez l'estimation")))
    covs <- if ("group" %in% names(sd$donnees)) "group" else NULL
    cox <- ajuster_cox(sd$donnees, covs)
    if (is.null(cox) || !is.null(cox$erreur)) return(datatable(data.frame(Message = "Indisponible")))
    ph <- test_ph_assomption(cox$modele)
    if (is.null(ph)) return(datatable(data.frame(Message = "Test indisponible")))
    datatable(ph, rownames = FALSE, options = list(dom = "t"))
  })

  # -------------------------------------------------------------------
  # Diagnostic des donnees
  # -------------------------------------------------------------------

  output$diag_resume <- renderDT({
    d <- df_courant()
    q <- resume_qualite(d)
    res <- data.frame(
      Indicateur = c("Observations", "Variables", "Cellules totales",
                     "Cellules manquantes", "Taux de manquant global (%)",
                     "Lignes dupliquees", "Colonnes completes",
                     "Colonnes critiques (> 50% manquant)"),
      Valeur = c(format(q$n_lignes, big.mark = " "), q$n_colonnes,
                 format(q$cellules, big.mark = " "),
                 format(q$cellules_manquantes, big.mark = " "),
                 q$pct_manquant_global, q$lignes_doublons,
                 q$colonnes_completes, q$colonnes_critiques),
      stringsAsFactors = FALSE
    )
    datatable(res, rownames = FALSE, options = list(dom = "t"))
  })

  output$diag_completude <- renderDT({
    res <- tableau_completude(df_courant())
    datatable(res, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Pct_manquant",
                  background = styleColorBar(res$Pct_manquant, "#f5c6a5"),
                  backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$diag_outliers <- renderDT({
    res <- tableau_outliers(df_courant(), as.numeric(input$diag_k))
    if (is.null(res)) return(datatable(data.frame(Message = "Aucune variable quantitative")))
    datatable(res, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diag_coherence <- renderDT({
    res <- controle_coherence(df_courant())
    datatable(res, rownames = FALSE, options = list(dom = "t"))
  })

  output$diag_extremes <- renderDT({
    res <- tableau_extremes(df_courant())
    if (is.null(res)) return(datatable(data.frame(Message = "Aucune variable quantitative")))
    datatable(res, rownames = FALSE, options = list(pageLength = 15))
  })

  observeEvent(input$diag_appliquer, {
    d <- df_courant()
    res <- traiter_manquants(d, input$diag_strategie)
    donnees_travail(res$df)
    message_diag(res$rapport)
    output$diag_message <- renderText(res$rapport)
    showNotification(res$rapport, type = "message")
  })

  output$diag_message <- renderText(message_diag())

  # -------------------------------------------------------------------
  # Tableau filtrable
  # -------------------------------------------------------------------

  output$tab_filtre_cat <- renderUI({
    selectInput("tab_cat", "Filtrer par variable qualitative",
                choices = c("Aucun" = "", vars_discretes()))
  })

  output$tab_filtre_cat_val <- renderUI({
    req(input$tab_cat)
    if (input$tab_cat == "") return(NULL)
    d <- df_courant()
    pickerInput("tab_cat_val", "Modalites",
                choices = sort(unique(na.omit(d[[input$tab_cat]]))),
                selected = sort(unique(na.omit(d[[input$tab_cat]]))),
                multiple = TRUE, options = list(`actions-box` = TRUE))
  })

  output$tab_filtre_num <- renderUI({
    selectInput("tab_num", "Filtrer par variable quantitative",
                choices = c("Aucun" = "", vars_continues()))
  })

  output$tab_filtre_num_range <- renderUI({
    req(input$tab_num)
    if (input$tab_num == "") return(NULL)
    x <- df_courant()[[input$tab_num]]
    sliderInput("tab_num_range", "Intervalle",
                min = floor(min(x, na.rm = TRUE)),
                max = ceiling(max(x, na.rm = TRUE)),
                value = range(x, na.rm = TRUE))
  })

  output$tab_colonnes <- renderUI({
    selectizeInput("tab_cols", "Colonnes a afficher",
                   choices = names(df_courant()),
                   selected = names(df_courant())[seq_len(min(10, ncol(df_courant())))],
                   multiple = TRUE)
  })

  df_tab <- reactive({
    d <- df_courant()
    if (!is.null(input$tab_cat) && input$tab_cat != "" && !is.null(input$tab_cat_val))
      d <- d[as.character(d[[input$tab_cat]]) %in% input$tab_cat_val, ]
    if (!is.null(input$tab_num) && input$tab_num != "" && !is.null(input$tab_num_range))
      d <- d[d[[input$tab_num]] >= input$tab_num_range[1] &
               d[[input$tab_num]] <= input$tab_num_range[2], ]
    cols <- intersect(input$tab_cols %||% names(d), names(d))
    if (length(cols) > 0) d <- d[, cols, drop = FALSE]
    d
  })

  output$tab_tableau <- renderDT({
    datatable(df_tab(), rownames = FALSE, extensions = c("Buttons", "Scroller"),
              options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE,
                             scrollY = 400, scroller = TRUE,
                             dom = "Bfrtip"))
  })

  # -------------------------------------------------------------------
  # Import / Export
  # -------------------------------------------------------------------

  output$imp_resume <- renderUI({
    d <- df_courant()
    HTML(sprintf("<p>Jeu de donnees charge : <strong>%s observations</strong> et <strong>%d variables</strong>.</p>",
                 format(nrow(d), big.mark = " "), ncol(d)))
  })

  output$export_donnees <- downloadHandler(
    filename = function() paste0("donnees_filtrees_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(df_tab(), file)
  )

  output$export_codebook <- downloadHandler(
    filename = function() paste0("dictionnaire_variables_", Sys.Date(), ".csv"),
    content = function(file) {
      cb <- codebook_actif()
      if (is.null(cb)) {
        d <- df_courant(); t <- types_courants()
        cb <- data.frame(variable = names(d), libelle = names(d),
                         type = unname(t), unite = "", stringsAsFactors = FALSE)
      }
      readr::write_csv(cb, file)
    }
  )

  output$export_resume_stats <- downloadHandler(
    filename = function() paste0("statistiques_descriptives_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- df_courant()
      cont <- tableau_descriptif_continu(d)
      readr::write_csv(if (is.null(cont)) data.frame() else cont, file)
    }
  )

  contenu_rapport <- reactive({
    d <- df_courant()
    filtres <- ""
    if (!is.null(input$tab_cat) && input$tab_cat != "")
      filtres <- paste0("- Variable qualitative : ", input$tab_cat, "\n")
    if (!is.null(input$tab_num) && input$tab_num != "")
      filtres <- paste0(filtres, "- Variable quantitative : ", input$tab_num,
                        " dans [", paste(input$tab_num_range, collapse = " ; "), "]\n")
    m <- modele_reg()
    res_reg <- NULL
    if (!is.null(m)) {
      res_reg <- list(tableau = tableau_coefficients(m$modele),
                      formule = m$formule,
                      qualite = pseudo_r2(m$modele),
                      interpretation = interpreter_coefficients(m$modele))
    }
    construire_rapport(d, titre = "Rapport d'analyse d'enquete en sante publique",
                       resultat_regression = res_reg, filtres_texte = filtres)
  })

  output$export_rapport_md <- downloadHandler(
    filename = function() paste0("rapport_analyse_", Sys.Date(), ".md"),
    content = function(file) writeLines(contenu_rapport(), file)
  )

  # ===================================================================
  # Plan de sondage complexe
  # ===================================================================

  plan_info <- reactive({
    d <- df_courant()
    if (is.null(d)) return(NULL)
    tryCatch(construire_plan(d), error = function(e) list(erreur = conditionMessage(e)))
  })

  output$plan_resume <- renderUI({
    info <- plan_info()
    if (is.null(info)) return(helpText("Aucune donnee chargee."))
    if (!is.null(info$erreur)) {
      return(div(class = "alerte-plan",
        HTML(paste0("<b>Plan de sondage indisponible.</b> ", info$erreur,
          "<br>Les estimations ponderees ne peuvent pas etre calculees sur ce jeu de donnees. ",
          "Cela arrive quand les variables de poids, de strate ou d'unites primaires sont absentes, ",
          "ce qui est courant pour un fichier importe."))))
    }
    statut_validite <- if (info$ddl > 0 && info$strates_uniques == 0) "primary" else "warning"
    tagList(
      fluidRow(
        valueBox(value = info$poids, subtitle = "Variable de ponderation",
                 icon = icon("weight-hanging"), color = "blue", width = 3),
        valueBox(value = info$n_strates, subtitle = "Strates",
                 icon = icon("layer-group"), color = "aqua", width = 3),
        valueBox(value = info$n_psu, subtitle = "Unites primaires",
                 icon = icon("sitemap"), color = "green", width = 3),
        valueBox(value = info$ddl, subtitle = "Degres de liberte",
                 icon = icon("ruler"), color = "yellow", width = 3)
      ),
      fluidRow(
        box(width = 6, status = "primary",
            h4("Population representee"),
            p(paste0("La somme des poids atteint ",
                     format(round(info$poids_total), big.mark = " "),
                     " personnes. Chaque individu de l'echantillon represente en moyenne ",
                     format(round(info$poids_total / info$n), big.mark = " ", nsmall = 0),
                     " personnes de la population cible.")),
            p(paste0("Le poids median vaut ",
                     format(round(info$poids_median), big.mark = " "),
                     ", avec des valeurs extremes de part et d'autre selon les strates."))
        ),
        box(width = 6, status = statut_validite,
            h4("Validite de l'estimation"),
            if (info$strates_uniques == 0) {
              p("Toutes les strates comptent au moins deux unites primaires :
                 la variance est estimable sans correction particuliere.")
            } else {
              p(paste0(info$strates_uniques, " strate(s) ne comptent qu'une seule unite primaire. ",
                       "La variance de ces strates est ajustee automatiquement."))
            },
            p(paste0("Les tests s'appuient sur ", info$ddl,
                     " degres de liberte, soit le nombre d'unites primaires moins le nombre ",
                     "de strates. C'est la convention du plan de sondage pour le calcul ",
                     "des intervalles."))
        )
      )
    )
  })

  output$plan_poids <- renderUI({
    d <- df_courant()
    v <- detecter_vars_plan(d)
    if (is.null(v$poids)) return(helpText("Aucune variable de ponderation detectee."))
    selectInput("plan_poids_var", "Variable de ponderation",
                choices = v$poids, selected = v$poids[1])
  })

  output$plan_variables <- renderUI({
    d <- df_courant()
    t <- types_courants()
    bin <- names(t)[t == "binaire"]
    bin <- bin[vapply(bin, function(v) all(na.omit(d[[v]]) %in% c(0, 1)), logical(1))]
    if (length(bin) == 0) return(helpText("Aucune variable binaire 0/1 disponible."))
    selectizeInput("plan_vars", "Indicateurs a estimer",
                   choices = bin, selected = head(bin, 5), multiple = TRUE)
  })

  # Plan recalcule avec le poids choisi
  plan_choisi <- eventReactive(input$plan_lancer, {
    d <- df_courant()
    withProgress(message = "Estimation des prevalences ponderees", value = 0.3, {
      p <- construire_plan(d, poids = input$plan_poids_var)
      incProgress(0.4)
      if (!is.null(p$erreur)) return(p)
      vars <- input$plan_vars
      p$prevalences <- tableau_prevalences_ponderees(d, p, vars)
      incProgress(0.3)
      p$effets <- tableau_effets_plan(d, p, vars)
    })
    p
  }, ignoreNULL = FALSE)

  output$plan_prevalences <- renderDT({
    p <- plan_choisi()
    req(p)
    if (!is.null(p$erreur) || is.null(p$prevalences)) {
      return(datatable(data.frame(Message = "Aucune estimation disponible."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(p$prevalences, rownames = FALSE,
              colnames = c("Variable", "N valide", "N cas", "Cas ponderes",
                           "Prevalence brute (%)", "Prevalence ponderee (%)",
                           "Erreur standard (%)", "IC 95% inferieur",
                           "IC 95% superieur"),
              options = list(dom = "t", pageLength = 15, scrollX = TRUE)) |>
      formatRound(c("Prevalence_brute_pct", "Prevalence_ponderee_pct",
                    "Erreur_std_pct", "IC95_inf_pct", "IC95_sup_pct"), 2) |>
      formatRound("N_cas_pondere", 0)
  })

  output$plan_graph_prevalences <- renderPlotly({
    p <- plan_choisi()
    req(p)
    if (!is.null(p$erreur)) return(NULL)
    g <- tryCatch(graph_prevalences_comparees(df_courant(), p, input$plan_vars),
                  error = function(e) NULL)
    if (is.null(g)) return(NULL)
    ggplotly(g, tooltip = c("x", "y", "fill"))
  })

  observeEvent(input$plan_lancer, {
    p <- plan_choisi()
    req(p)
    if (!is.null(p$erreur)) return()
    d <- df_courant()
    withProgress(message = "Moyennes ponderees", value = 0.5, {
      p$moyennes <- tableau_moyennes_ponderees(d, p, vars_continues())
    })
    output$plan_moyennes <- renderDT({
      if (is.null(p$moyennes) || nrow(p$moyennes) == 0) {
        return(datatable(data.frame(Message = "Aucune variable quantitative disponible."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      datatable(p$moyennes, rownames = FALSE,
                colnames = c("Variable", "N valide", "Moyenne brute",
                             "Moyenne ponderee", "Erreur standard",
                             "IC 95% inferieur", "IC 95% superieur",
                             "Mediane ponderee"),
                options = list(dom = "t", pageLength = 20, scrollX = TRUE)) |>
        formatRound(c("Moyenne_brute", "Moyenne_ponderee", "IC95_inf", "IC95_sup",
                      "Mediane_ponderee"), 3) |>
        formatRound("Erreur_std", 4)
    })
  })

  output$plan_effets <- renderDT({
    p <- plan_choisi()
    req(p)
    if (!is.null(p$erreur) || is.null(p$effets)) {
      return(datatable(data.frame(Message = "Aucun effet de plan calcule."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(p$effets, rownames = FALSE,
              colnames = c("Variable", "Prevalence brute (%)", "IC 95% brut",
                           "Prevalence ponderee (%)", "IC 95% pondere",
                           "Effet de plan", "Largeur IC brut", "Largeur IC pondere"),
              options = list(dom = "t", pageLength = 15, scrollX = TRUE)) |>
      formatRound("Effet_de_plan", 3)
  })

  output$plan_dom_variable <- renderUI({
    d <- df_courant()
    t <- types_courants()
    bin <- names(t)[t == "binaire"]
    bin <- bin[vapply(bin, function(v) all(na.omit(d[[v]]) %in% c(0, 1)), logical(1))]
    selectInput("plan_dom_var", "Indicateur", choices = bin,
                selected = if ("hypertension" %in% bin) "hypertension" else head(bin, 1))
  })

  output$plan_dom_groupe <- renderUI({
    d <- df_courant()
    t <- types_courants()
    qual <- names(t)[t %in% c("binaire", "categorielle")]
    qual <- qual[vapply(qual, function(v) {
      n <- length(unique(na.omit(d[[v]])))
      n >= 2 && n <= 15
    }, logical(1))]
    qual <- setdiff(qual, "SEQN")
    selectInput("plan_dom_grp", "Domaine (variable de groupe)", choices = qual,
                selected = if ("RIAGENDR" %in% qual) "RIAGENDR" else head(qual, 1))
  })

  dom_res <- eventReactive(input$plan_dom_lancer, {
    p <- plan_info()
    req(p)
    if (!is.null(p$erreur)) return(NULL)
    req(input$plan_dom_var, input$plan_dom_grp)
    tryCatch(prevalence_par_domaine(p$plan, input$plan_dom_var, input$plan_dom_grp),
             error = function(e) NULL)
  }, ignoreNULL = FALSE)

  output$plan_domaine <- renderDT({
    r <- dom_res()
    if (is.null(r) || nrow(r) == 0) {
      return(datatable(data.frame(Message = "Aucune estimation disponible pour cette combinaison."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(r, rownames = FALSE,
              colnames = c("Domaine", "Modalite", "N", "Prevalence ponderee (%)",
                           "Erreur standard (%)", "IC 95% inferieur", "IC 95% superieur"),
              options = list(dom = "t", pageLength = 15, scrollX = TRUE)) |>
      formatRound(c("Prevalence_ponderee_pct", "Erreur_std_pct",
                    "IC95_inf_pct", "IC95_sup_pct"), 2)
  })

  output$plan_graph_domaine <- renderPlotly({
    r <- dom_res()
    p <- plan_info()
    req(r, p)
    if (is.null(p$erreur) && nrow(r) > 0) {
      return(plot_ly(r, x = ~Prevalence_ponderee_pct, y = ~Modalite, type = "bar",
                     orientation = "h", marker = list(color = "#1f6f8b"),
                     error_x = list(type = "data",
                                    array = ~(IC95_sup_pct - Prevalence_ponderee_pct),
                                    arrayminus = ~(Prevalence_ponderee_pct - IC95_inf_pct),
                                    color = "#3d5a80")) |>
        layout(xaxis = list(title = "Prevalence ponderee (%)"),
               yaxis = list(title = ""),
               margin = list(l = 120)))
    }
    p2 <- plan_choisi()
    if (is.null(p2) || !is.null(p2$erreur)) return(NULL)
    g <- tryCatch(graph_prevalence_domaine(p2$plan, input$plan_dom_var, input$plan_dom_grp),
                  error = function(e) NULL)
    if (is.null(g)) return(NULL)
    ggplotly(g)
  })

  output$plan_rao_scott <- renderDT({
    p <- plan_choisi()
    req(p)
    if (!is.null(p$erreur)) {
      return(datatable(data.frame(Message = "Test indisponible."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    v1 <- input$plan_dom_var
    v2 <- input$plan_dom_grp
    req(v1, v2)
    if (identical(v1, v2)) {
      return(datatable(data.frame(Message = "Choisissez deux variables distinctes."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    r <- tryCatch(test_rao_scott(p$plan, v1, v2), error = function(e) NULL)
    if (is.null(r)) {
      return(datatable(data.frame(Message = "Le test n'a pas pu etre calcule."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(r, rownames = FALSE,
              colnames = c("Test", "Statistique", "Degres de liberte", "p-value"),
              options = list(dom = "t", scrollX = TRUE)) |>
      formatStyle("Test", fontWeight = "bold")
  })

  output$plan_reg_outcome <- renderUI({
    d <- df_courant()
    t <- types_courants()
    bin <- names(t)[t == "binaire"]
    bin <- bin[vapply(bin, function(v) all(na.omit(d[[v]]) %in% c(0, 1)), logical(1))]
    selectInput("plan_reg_y", "Variable reponse (0/1)", choices = bin,
                selected = if ("hypertension" %in% bin) "hypertension" else head(bin, 1))
  })

  output$plan_reg_predicteurs <- renderUI({
    d <- df_courant()
    cols <- setdiff(names(d), c("SEQN", "SDMVPSU", "SDMVSTRA",
                                input$plan_reg_y))
    t <- types_courants()
    cols <- cols[t[cols] %in% c("continue", "binaire")]
    defaut <- intersect(c("RIDAGEYR", "RIAGENDR", "BMXBMI", "current_smoker"), cols)
    selectizeInput("plan_reg_x", "Variables explicatives",
                   choices = cols, selected = defaut, multiple = TRUE)
  })

  plan_reg <- eventReactive(input$plan_reg_lancer, {
    p <- plan_info()
    req(p)
    if (!is.null(p$erreur)) return(list(erreur = "Plan de sondage indisponible."))
    req(input$plan_reg_y, input$plan_reg_x)
    withProgress(message = "Ajustement du modele pondere", value = 0.5, {
      ajuster_logistique_pondere(p$plan, input$plan_reg_y, input$plan_reg_x)
    })
  }, ignoreNULL = FALSE)

  output$plan_reg_coefs <- renderDT({
    r <- plan_reg()
    req(r)
    if (!is.null(r$erreur)) {
      return(datatable(data.frame(Message = r$erreur),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(r$coefficients, rownames = FALSE,
              colnames = c("Terme", "Coefficient", "Erreur standard", "Rapport de cotes",
                           "IC 95% inferieur", "IC 95% superieur", "p-value",
                           "Significatif a 5%"),
              options = list(dom = "t", scrollX = TRUE)) |>
      formatRound(c("Coefficient", "Erreur_std", "OR", "IC95_inf", "IC95_sup"), 4) |>
      formatStyle("Significatif",
                  backgroundColor = styleEqual(c("Oui", "Non"),
                                               c("#d4edda", "#f8f9fa")))
  })

  output$plan_reg_qualite <- renderUI({
    r <- plan_reg()
    req(r)
    if (!is.null(r$erreur)) return(NULL)
    tagList(
      h4("Qualite du modele"),
      tags$ul(
        tags$li(paste0("Degres de liberte du plan : ", r$degres_liberte)),
        tags$li(paste0("AIC : ", r$AIC)),
        tags$li(paste0("Pseudo R2 (deviance) : ",
                       if (is.na(r$pseudo_r2)) "non calculable" else r$pseudo_r2))
      ),
      p(r$interpretation),
      p(em(paste0("Note : avec ", r$degres_liberte,
                  " degres de liberte, les intervalles sont larges. ",
                  "Les variables a faible effectif dans certaines strates peuvent ",
                  "produire des estimations instables.")))
    )
  })

  output$export_rapport_html <- downloadHandler(
    filename = function() paste0("rapport_analyse_", Sys.Date(), ".html"),
    content = function(file) {
      rmd <- file.path(tempdir(), "rapport.Rmd")
      ecrire_rapport_rmd(contenu_rapport(), rmd)
      if (requireNamespace("rmarkdown", quietly = TRUE)) {
        out <- tryCatch(
          rmarkdown::render(rmd, output_file = file, quiet = TRUE),
          error = function(e) NULL
        )
        if (is.null(out)) writeLines(contenu_rapport(), file)
      } else {
        writeLines(contenu_rapport(), file)
      }
    }
  )
}
