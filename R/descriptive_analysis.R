# =====================================================================
# descriptive_analysis.R
# Statistiques descriptives, analyse bivariee et visualisations
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source("R/data_cleaning.R")

# ---------------------------------------------------------------------
# 1. Statistiques descriptives univariees
# ---------------------------------------------------------------------

#' Statistiques de tendance centrale et de dispersion pour une variable continue
stats_continue <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(NULL)
  m <- mean(x); s <- sd(x)
  data.frame(
    N = n,
    Moyenne = round(m, 3),
    Ecart_type = round(s, 3),
    Erreur_std = round(s / sqrt(n), 3),
    Minimum = round(min(x), 3),
    Q1 = round(quantile(x, 0.25), 3),
    Mediane = round(median(x), 3),
    Q3 = round(quantile(x, 0.75), 3),
    Maximum = round(max(x), 3),
    Etendue = round(max(x) - min(x), 3),
    IQR = round(IQR(x), 3),
    Asymetrie = round(mean(((x - m) / s)^3), 3),
    Aplatissement = round(mean(((x - m) / s)^4) - 3, 3),
    CV_pct = round(s / m * 100, 2),
    IC95_inf = round(m - 1.96 * s / sqrt(n), 3),
    IC95_sup = round(m + 1.96 * s / sqrt(n), 3),
    stringsAsFactors = FALSE
  )
}

#' Tableau descriptif complet pour toutes les variables continues
tableau_descriptif_continu <- function(df, vars = NULL) {
  types <- detecter_types(df)
  vars <- vars %||% names(types)[types == "continue"]
  if (length(vars) == 0) return(NULL)
  res <- lapply(vars, function(v) {
    s <- stats_continue(df[[v]])
    if (is.null(s)) return(NULL)
    cbind(Variable = v, s)
  })
  res <- Filter(Negate(is.null), res)
  do.call(rbind, res)
}

#' Frequences pour une variable categorielle
stats_categorielle <- function(x, nom = "Variable") {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  tab <- table(x)
  df <- data.frame(
    Modalite = names(tab),
    Effectif = as.integer(tab),
    Pourcentage = round(as.numeric(prop.table(tab)) * 100, 2),
    stringsAsFactors = FALSE
  )
  df$Pourcentage_cumule <- round(cumsum(df$Pourcentage), 2)
  df
}

#' Tableau de frequences pour toutes les variables discretes
tableau_descriptif_discret <- function(df, vars = NULL) {
  types <- detecter_types(df)
  vars <- vars %||% names(types)[types %in% c("binaire", "categorielle")]
  if (length(vars) == 0) return(NULL)
  res <- lapply(vars, function(v) {
    s <- stats_categorielle(df[[v]])
    if (is.null(s)) return(NULL)
    cbind(Variable = v, s)
  })
  res <- Filter(Negate(is.null), res)
  do.call(rbind, res)
}

# ---------------------------------------------------------------------
# 2. Analyse bivariee
# ---------------------------------------------------------------------

#' Table de contingence croisee avec pourcentages ligne et colonne
tableau_croise <- function(df, var1, var2) {
  tab <- table(df[[var1]], df[[var2]], useNA = "no")
  if (length(dim(tab)) != 2) return(NULL)
  res <- as.data.frame.matrix(tab)
  res <- cbind(Modalite = rownames(res), res)
  rownames(res) <- NULL
  res
}

#' Test du chi-deux avec V de Cramer et interpretation automatique
test_chi2 <- function(df, var1, var2) {
  d <- df[, c(var1, var2)]
  d <- d[complete.cases(d), ]
  tab <- table(d[[var1]], d[[var2]])
  if (any(dim(tab) < 2) || sum(tab) < 5) {
    return(list(disponible = FALSE,
                message = "Effectifs insuffisants pour le test du chi-deux."))
  }
  test <- suppressWarnings(stats::chisq.test(tab))
  n <- sum(tab)
  min_dim <- min(dim(tab)) - 1
  v_cramer <- sqrt(as.numeric(test$statistic) / (n * min_dim))
  # V de Cramer biaise corrige
  phi2 <- as.numeric(test$statistic) / n
  phi2corr <- max(0, phi2 - (length(var1) * 0) -
                    ((ncol(tab) - 1) * (nrow(tab) - 1)) / (n - 1))
  rcorr <- nrow(tab) - ((nrow(tab) - 1)^2) / (n - 1)
  ccorr <- ncol(tab) - ((ncol(tab) - 1)^2) / (n - 1)
  v_corr <- sqrt(phi2corr / min(rcorr, ccorr))
  force <- if (v_corr < 0.1) "negligeable" else if (v_corr < 0.3) "faible" else
    if (v_corr < 0.5) "moderee" else "forte"
  p <- test$p.value
  concl <- if (p < 0.001) "tres significative (p < 0.001)" else
    if (p < 0.01) "significative (p < 0.01)" else
      if (p < 0.05) "significative (p < 0.05)" else "non significative"
  list(
    disponible = TRUE,
    statistique = round(as.numeric(test$statistic), 3),
    ddl = as.integer(test$parameter),
    p_value = p,
    p_formate = format.pval(p, digits = 4, eps = 0.0001),
    v_cramer = round(v_corr, 3),
    force_association = force,
    conclusion = paste0("Association ", concl, " entre ", var1, " et ", var2,
                        " (V de Cramer = ", round(v_corr, 3), ", force ", force, ")."),
    attendus_faibles = sum(test$expected < 5)
  )
}

#' Comparaison de moyennes (t-test ou ANOVA) avec test de Levene
test_moyennes <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  d[[var_grp]] <- droplevels(as.factor(d[[var_grp]]))
  groupes <- levels(d[[var_grp]])
  if (length(groupes) < 2) {
    return(list(disponible = FALSE, message = "Moins de deux groupes disponibles."))
  }
  descriptions <- do.call(rbind, lapply(groupes, function(g) {
    x <- d[[var_num]][d[[var_grp]] == g]
    data.frame(
      Groupe = g,
      N = length(x),
      Moyenne = round(mean(x), 2),
      Ecart_type = round(sd(x), 2),
      Mediane = round(median(x), 2),
      stringsAsFactors = FALSE
    )
  }))
  if (length(groupes) == 2) {
    tt <- stats::t.test(d[[var_num]] ~ d[[var_grp]])
    test_nom <- "Test t de Student"
    stat <- round(as.numeric(tt$statistic), 3)
    p <- tt$p.value
    ddl <- round(as.numeric(tt$parameter), 2)
    var_test <- stats::var.test(d[[var_num]] ~ d[[var_grp]])
    homogeneite <- if (var_test$p.value < 0.05) "variances inegales" else "variances homogenes"
    interpretation <- paste0(
      "La difference de moyenne de ", var_num, " entre les groupes est ",
      if (p < 0.05) "statistiquement significative" else "non significative",
      " (", test_nom, ", t = ", stat, ", p = ", format.pval(p, digits = 4), ")."
    )
  } else {
    av <- stats::aov(d[[var_num]] ~ d[[var_grp]])
    sm <- summary(av)[[1]]
    stat <- round(sm[["F value"]][1], 3)
    p <- sm[["Pr(>F)"]][1]
    ddl <- paste0(sm[["Df"]][1], "; ", sm[["Df"]][2])
    test_nom <- "ANOVA a un facteur"
    homogeneite <- "non evaluee"
    interpretation <- paste0(
      "L'effet du groupe sur ", var_num, " est ",
      if (p < 0.05) "statistiquement significatif" else "non significatif",
      " (", test_nom, ", F = ", stat, ", p = ", format.pval(p, digits = 4), ")."
    )
  }
  list(disponible = TRUE, test = test_nom, statistique = stat, ddl = ddl,
       p_value = p, p_formate = format.pval(p, digits = 4, eps = 0.0001),
       descriptions = descriptions, homogeneite_variances = homogeneite,
       interpretation = interpretation)
}

#' Post-hoc Tukey HSD apres ANOVA
test_posthoc_tukey <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  d[[var_grp]] <- droplevels(as.factor(d[[var_grp]]))
  if (nlevels(d[[var_grp]]) < 3) return(NULL)
  av <- stats::aov(d[[var_num]] ~ d[[var_grp]])
  tk <- stats::TukeyHSD(av)
  res <- as.data.frame(tk[[1]])
  res <- cbind(Comparaison = rownames(res), round(res, 4))
  rownames(res) <- NULL
  res
}

#' Correlation entre variables continues avec tests de significativite
matrice_correlation <- function(df, vars, methode = "pearson") {
  vars <- vars[vapply(vars, function(v) is.numeric(df[[v]]), logical(1))]
  if (length(vars) < 2) return(NULL)
  m <- as.matrix(df[, vars])
  stats::cor(m, use = "pairwise.complete.obs", method = methode)
}

#' Tableau des correlations significatives deux a deux
correlations_significatives <- function(df, vars, methode = "pearson") {
  vars <- vars[vapply(vars, function(v) is.numeric(df[[v]]), logical(1))]
  if (length(vars) < 2) return(NULL)
  combos <- utils::combn(vars, 2, simplify = FALSE)
  res <- lapply(combos, function(cc) {
    x <- df[[cc[1]]]; y <- df[[cc[2]]]
    ok <- complete.cases(x, y)
    if (sum(ok) < 4) return(NULL)
    tst <- suppressWarnings(stats::cor.test(x[ok], y[ok], method = methode))
    data.frame(
      Variable_1 = cc[1], Variable_2 = cc[2],
      N = sum(ok),
      r = round(as.numeric(tst$estimate), 3),
      p_value = format.pval(tst$p.value, digits = 4, eps = 0.0001),
      IC95_inf = round(tst$conf.int[1], 3),
      IC95_sup = round(tst$conf.int[2], 3),
      Significatif = ifelse(tst$p.value < 0.05, "Oui", "Non"),
      stringsAsFactors = FALSE
    )
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  out <- do.call(rbind, res)
  out[order(abs(out$r), decreasing = TRUE), ]
}

# ---------------------------------------------------------------------
# 3. Visualisations
# ---------------------------------------------------------------------

#' Histogramme avec densite pour une variable continue
graph_histogramme <- function(df, var, bins = 30, couleur = "#1f6f8b") {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = couleur, colour = "white", alpha = 0.85) +
    geom_density(colour = "#e07a5f", linewidth = 1) +
    labs(x = var, y = "Densite", title = paste("Distribution de", var)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

#' Boite a moustaches, eventuellement groupee
graph_boxplot <- function(df, var, groupe = NULL, couleur = "#1f6f8b") {
  if (is.null(groupe)) {
    ggplot(df, aes(x = "", y = .data[[var]])) +
      geom_boxplot(fill = couleur, alpha = 0.8, width = 0.4, outlier.colour = "#e07a5f") +
      labs(x = "", y = var, title = paste("Boite a moustaches de", var)) +
      theme_minimal(base_size = 13)
  } else {
    ggplot(df, aes(x = as.factor(.data[[groupe]]), y = .data[[var]],
                   fill = as.factor(.data[[groupe]]))) +
      geom_boxplot(alpha = 0.8, outlier.colour = "#e07a5f") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = groupe, y = var,
           title = paste(var, "selon", groupe), fill = groupe) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  }
}

#' Diagramme en barres pour une variable categorielle
graph_barres <- function(df, var, couleur = "#1f6f8b", tri = TRUE) {
  d <- stats_categorielle(df[[var]])
  if (is.null(d)) return(NULL)
  if (tri) d$Modalite <- factor(d$Modalite, levels = d$Modalite[order(d$Effectif)])
  ggplot(d, aes(x = Modalite, y = Effectif)) +
    geom_col(fill = couleur, alpha = 0.9) +
    geom_text(aes(label = paste0(Effectif, "\n(", Pourcentage, "%)")),
              vjust = -0.2, size = 3.2) +
    labs(x = var, y = "Effectif", title = paste("Repartition de", var)) +
    theme_minimal(base_size = 13) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
}

#' Diagramme circulaire (camembert) pour une variable categorielle
graph_camembert <- function(df, var) {
  d <- stats_categorielle(df[[var]])
  if (is.null(d)) return(NULL)
  d$Etiquette <- paste0(d$Modalite, "\n", d$Pourcentage, "%")
  ggplot(d, aes(x = "", y = Effectif, fill = Modalite)) +
    geom_col(width = 1, colour = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = Etiquette),
              position = position_stack(vjust = 0.5), size = 3.2, colour = "white") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = paste("Repartition de", var), fill = var) +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

#' Nuage de points avec droite de tendance
graph_nuage <- function(df, x, y, couleur = NULL, lisse = TRUE) {
  g <- ggplot(df, aes(x = .data[[x]], y = .data[[y]]))
  if (!is.null(couleur)) {
    g <- g + geom_point(aes(colour = as.factor(.data[[couleur]])), alpha = 0.6, size = 1.8) +
      scale_colour_brewer(palette = "Set2", name = couleur)
  } else {
    g <- g + geom_point(alpha = 0.55, size = 1.8, colour = "#1f6f8b")
  }
  if (lisse) g <- g + geom_smooth(method = "lm", colour = "#e07a5f", se = TRUE, linewidth = 0.9)
  g + labs(x = x, y = y, title = paste(y, "en fonction de", x)) +
    theme_minimal(base_size = 13)
}

#' Carte de chaleur de la matrice de correlation
graph_correlation <- function(df, vars, methode = "pearson") {
  m <- matrice_correlation(df, vars, methode)
  if (is.null(m)) return(NULL)
  md <- as.data.frame(as.table(m))
  names(md) <- c("Var1", "Var2", "r")
  ggplot(md, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = round(r, 2)), size = 3, colour = ifelse(abs(md$r) > 0.5, "white", "black")) +
    scale_fill_gradient2(low = "#8c2f39", mid = "white", high = "#1f6f8b",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = paste("Matrice de correlation (", methode, ")"), x = "", y = "", fill = "r") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Distribution des valeurs manquantes par variable (barres horizontales)
graph_manquants <- function(df) {
  d <- data.frame(Variable = names(df),
                  Pct = vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)))
  d <- d[d$Pct > 0, , drop = FALSE]
  if (nrow(d) == 0) {
    return(ggplot() + annotate("text", x = 0, y = 0,
                               label = "Aucune valeur manquante dans le jeu de donnees") +
             theme_void())
  }
  d$Variable <- factor(d$Variable, levels = d$Variable[order(d$Pct)])
  ggplot(d, aes(x = Pct, y = Variable)) +
    geom_col(fill = "#e07a5f", alpha = 0.9) +
    geom_text(aes(label = paste0(round(Pct, 1), "%")), hjust = -0.1, size = 3.2) +
    labs(x = "Pourcentage de valeurs manquantes", y = "",
         title = "Valeurs manquantes par variable") +
    theme_minimal(base_size = 13) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
}

#' Diagramme en barres empilees pour une analyse croisee
graph_barres_croisees <- function(df, var1, var2, position = "fill") {
  ggplot(df, aes(x = as.factor(.data[[var1]]), fill = as.factor(.data[[var2]]))) +
    geom_bar(position = position, alpha = 0.9) +
    scale_fill_brewer(palette = "Set2", name = var2) +
    labs(x = var1, y = ifelse(position == "fill", "Proportion", "Effectif"),
         title = paste(var2, "selon", var1)) +
    theme_minimal(base_size = 13) +
    scale_y_continuous(labels = if (position == "fill") scales::percent else waiver())
}

#' Barres d'erreur des moyennes par groupe avec intervalle de confiance
graph_moyennes_ic <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  agg <- d |> dplyr::group_by(Groupe = as.factor(.data[[var_grp]])) |>
    dplyr::summarise(n = dplyr::n(),
                     moy = mean(.data[[var_num]]),
                     sd = sd(.data[[var_num]]), .groups = "drop") |>
    dplyr::mutate(se = sd / sqrt(n), ic = 1.96 * se)
  ggplot(agg, aes(x = Groupe, y = moy, fill = Groupe)) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_errorbar(aes(ymin = moy - ic, ymax = moy + ic), width = 0.2) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = var_grp, y = paste("Moyenne de", var_num),
         title = paste("Moyenne de", var_num, "par", var_grp)) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}
