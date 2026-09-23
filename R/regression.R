# =====================================================================
# regression.R
# Regression logistique sur une variable binaire, diagnostic du modele,
# selection de variables et courbe ROC
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

source("R/data_cleaning.R")

# ---------------------------------------------------------------------
# 1. Ajustement du modele
# ---------------------------------------------------------------------

#' Ajuster une regression logistique
#'
#' @param df donnees
#' @param outcome nom de la variable binaire (0/1)
#' @param predicteurs vecteur de noms de variables explicatives
#' @return liste(modele, formule, donnees_utilisees, n, evenements)
ajuster_logistique <- function(df, outcome, predicteurs) {
  predicteurs <- unique(predicteurs)
  vars <- c(outcome, predicteurs)
  d <- df[, vars, drop = FALSE]
  d <- d[complete.cases(d), , drop = FALSE]

  # Verifier que l'outcome est bien binaire 0/1
  y <- d[[outcome]]
  if (!all(unique(y) %in% c(0, 1))) {
    return(list(erreur = "La variable reponse doit etre binaire codee 0/1."))
  }
  if (length(predicteurs) == 0) {
    return(list(erreur = "Selectionnez au moins un predicteur."))
  }
  # Retirer les predicteurs constants
  garder <- predicteurs[vapply(predicteurs, function(v) {
    length(unique(na.omit(d[[v]]))) > 1
  }, logical(1))]
  retires <- setdiff(predicteurs, garder)
  if (length(garder) == 0) {
    return(list(erreur = "Tous les predicteurs sont constants."))
  }
  formule <- stats::as.formula(paste(outcome, "~", paste(garder, collapse = " + ")))
  modele <- tryCatch(
    stats::glm(formule, data = d, family = stats::binomial()),
    error = function(e) NULL
  )
  if (is.null(modele)) {
    return(list(erreur = "Le modele n'a pas pu etre estime (separation ou effectifs trop faibles)."))
  }
  list(modele = modele, formule = formule, donnees = d,
       n = nrow(d), evenements = sum(d[[outcome]] == 1),
       predicteurs_retires = retires)
}

# ---------------------------------------------------------------------
# 2. Tableau des coefficients (rapports de cotes)
# ---------------------------------------------------------------------

#' Tableau des coefficients avec odds ratios et intervalles de confiance
tableau_coefficients <- function(modele) {
  s <- summary(modele)$coefficients
  co <- s[, 1]; se <- s[, 2]; p <- s[, 4]
  or <- exp(co)
  ic_inf <- exp(co - 1.96 * se)
  ic_sup <- exp(co + 1.96 * se)
  res <- data.frame(
    Terme = rownames(s),
    Coefficient = round(co, 4),
    Erreur_std = round(se, 4),
    Z = round(co / se, 3),
    OR = round(or, 4),
    IC95_inf = round(ic_inf, 4),
    IC95_sup = round(ic_sup, 4),
    p_value = format.pval(p, digits = 4, eps = 0.0001),
    Significatif = ifelse(p < 0.05, "Oui", "Non"),
    stringsAsFactors = FALSE
  )
  rownames(res) <- NULL
  res
}

#' Interpretation en langage naturel des rapports de cotes significatifs
interpreter_coefficients <- function(modele) {
  s <- summary(modele)$coefficients
  co <- s[, 1]; se <- s[, 2]; p <- s[, 4]
  termes <- rownames(s)[-1]
  co <- co[-1]; se <- se[-1]; p <- p[-1]
  if (length(co) == 0) return("Aucun terme interpretable.")
  lignes <- vapply(seq_along(co), function(i) {
    or <- exp(co[i])
    sens <- if (or > 1) "augmente" else "diminue"
    facteur <- if (or > 1) or else 1 / or
    sig <- if (p[i] < 0.05) "association significative" else "association non significative"
    sprintf("%s : un OR de %.2f (%s le risque d'un facteur %.2f), %s (p = %s).",
            termes[i], or, sens, facteur, sig,
            format.pval(p[i], digits = 3, eps = 0.0001))
  }, character(1))
  paste(lignes, collapse = "\n")
}

# ---------------------------------------------------------------------
# 3. Qualite d'ajustement
# ---------------------------------------------------------------------

#' Pseudo R2 de McFadden et de Nagelkerke
pseudo_r2 <- function(modele) {
  ll_full <- as.numeric(stats::logLik(modele))
  null_formule <- stats::as.formula(paste(deparse(stats::formula(modele)[[2]]), "~ 1"))
  null_modele <- stats::glm(null_formule, data = stats::model.frame(modele),
                            family = stats::binomial())
  ll_null <- as.numeric(stats::logLik(null_modele))
  n <- stats::nobs(modele)
  mcfadden <- 1 - ll_full / ll_null
  cox <- 1 - exp((2 / n) * (ll_null - ll_full))
  nagelkerke <- cox / (1 - exp(2 * ll_null / n))
  list(
    McFadden = round(mcfadden, 4),
    Cox_Snell = round(cox, 4),
    Nagelkerke = round(nagelkerke, 4),
    AIC = round(stats::AIC(modele), 2),
    BIC = round(stats::BIC(modele), 2),
    Log_vraisemblance = round(ll_full, 2),
    Deviance_residuelle = round(stats::deviance(modele), 2)
  )
}

#' Test du rapport de vraisemblance (modele complet contre modele nul)
test_rapport_vraisemblance <- function(modele) {
  ll_full <- as.numeric(stats::logLik(modele))
  null_formule <- stats::as.formula(paste(deparse(stats::formula(modele)[[2]]), "~ 1"))
  null <- stats::glm(null_formule, data = stats::model.frame(modele),
                     family = stats::binomial())
  ll_null <- as.numeric(stats::logLik(null))
  stat <- 2 * (ll_full - ll_null)
  ddl <- length(stats::coef(modele)) - 1
  p <- stats::pchisq(stat, ddl, lower.tail = FALSE)
  list(
    statistique = round(stat, 3), ddl = ddl,
    p_value = p, p_formate = format.pval(p, digits = 4, eps = 0.0001),
    interpretation = paste0(
      "Le modele est globalement ", if (p < 0.05) "significatif" else "non significatif",
      " : il explique mieux les donnees que le modele nul (LR = ", round(stat, 2),
      ", p = ", format.pval(p, digits = 4), ").")
  )
}

#' Test de Hosmer-Lemeshow (calibration par deciles)
test_hosmer_lemeshow <- function(modele, g = 10) {
  prob <- stats::fitted(modele)
  y <- stats::model.response(stats::model.frame(modele))
  if (length(unique(prob)) < g) g <- max(2, length(unique(prob)) - 1)
  coupe <- stats::quantile(prob, probs = seq(0, 1, length.out = g + 1), na.rm = TRUE)
  coupe <- unique(coupe)
  if (length(coupe) < 3) return(NULL)
  grp <- cut(prob, breaks = coupe, include.lowest = TRUE)
  obs1 <- tapply(y, grp, sum)
  att1 <- tapply(prob, grp, sum)
  n_grp <- tapply(y, grp, length)
  obs0 <- n_grp - obs1; att0 <- n_grp - att1
  hl <- sum((obs1 - att1)^2 / att1 + (obs0 - att0)^2 / att0)
  ddl <- length(n_grp) - 2
  p <- stats::pchisq(hl, ddl, lower.tail = FALSE)
  list(
    statistique = round(hl, 3), ddl = ddl, p_value = p,
    p_formate = format.pval(p, digits = 4, eps = 0.0001),
    interpretation = if (p < 0.05)
      "Le test rejette la bonne calibration : les probabilites predites s'ecartent des frequences observees."
    else "Le modele est correctement calibre : les probabilites predites concordent avec les frequences observees."
  )
}

#' Tableau de classification au seuil choisi
tableau_classification <- function(modele, seuil = 0.5) {
  prob <- stats::fitted(modele)
  y <- stats::model.response(stats::model.frame(modele))
  pred <- ifelse(prob >= seuil, 1, 0)
  cm <- table(Predit = factor(pred, levels = c(0, 1)),
              Observe = factor(y, levels = c(0, 1)))
  vp <- cm[2, 2]; vn <- cm[1, 1]; fp <- cm[2, 1]; fn <- cm[1, 2]
  sensibilite <- if ((vp + fn) > 0) vp / (vp + fn) else NA
  specificite <- if ((vn + fp) > 0) vn / (vn + fp) else NA
  vpp <- if ((vp + fp) > 0) vp / (vp + fp) else NA
  vpn <- if ((vn + fn) > 0) vn / (vn + fn) else NA
  exactitude <- (vp + vn) / sum(cm)
  f1 <- if (!is.na(vpp) && (vpp + sensibilite) > 0)
    2 * vpp * sensibilite / (vpp + sensibilite) else NA
  list(
    matrice = cm,
    exactitude = round(exactitude, 4),
    sensibilite = round(sensibilite, 4),
    specificite = round(specificite, 4),
    vpp = round(vpp, 4),
    vpn = round(vpn, 4),
    f1 = round(f1, 4),
    seuil = seuil
  )
}

# ---------------------------------------------------------------------
# 4. Courbe ROC et AUC
# ---------------------------------------------------------------------

#' Calcul de la courbe ROC et de l'aire sous la courbe
courbe_roc <- function(modele) {
  prob <- stats::fitted(modele)
  y <- stats::model.response(stats::model.frame(modele))
  seuils <- sort(unique(c(0, prob, 1)), decreasing = TRUE)
  res <- do.call(rbind, lapply(seuils, function(s) {
    pred <- ifelse(prob >= s, 1, 0)
    vp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
    vn <- sum(pred == 0 & y == 0); fn <- sum(pred == 0 & y == 1)
    data.frame(
      Seuil = s,
      Sensibilite = if ((vp + fn) > 0) vp / (vp + fn) else 0,
      Specificite = if ((vn + fp) > 0) vn / (vn + fp) else 0
    )
  }))
  res$Faux_positifs <- 1 - res$Specificite
  o <- order(res$Faux_positifs, res$Sensibilite)
  x <- res$Faux_positifs[o]; yy <- res$Sensibilite[o]
  auc <- sum(diff(x) * (head(yy, -1) + tail(yy, -1)) / 2)
  res <- res[order(res$Faux_positifs), ]
  attr(res, "auc") <- abs(auc)
  res
}

#' Graphique de la courbe ROC
graph_roc <- function(modele) {
  roc <- courbe_roc(modele)
  auc <- attr(roc, "auc")
  ggplot(roc, aes(x = Faux_positifs, y = Sensibilite)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_line(colour = "#1f6f8b", linewidth = 1.1) +
    labs(x = "1 - Specificite (faux positifs)", y = "Sensibilite (vrais positifs)",
         title = sprintf("Courbe ROC (AUC = %.3f)", auc)) +
    theme_minimal(base_size = 13)
}

# ---------------------------------------------------------------------
# 5. Diagnostic du modele
# ---------------------------------------------------------------------

#' Graphique des residus de Pearson contre les valeurs ajustees
graph_residus <- function(modele) {
  df <- data.frame(
    Ajuste = stats::fitted(modele),
    Residus = stats::residuals(modele, type = "pearson")
  )
  ggplot(df, aes(x = Ajuste, y = Residus)) +
    geom_point(alpha = 0.5, colour = "#1f6f8b") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#e07a5f") +
    geom_smooth(se = FALSE, colour = "#e07a5f", linewidth = 0.8) +
    labs(x = "Probabilite ajustee", y = "Residus de Pearson",
         title = "Diagnostic : residus de Pearson") +
    theme_minimal(base_size = 13)
}

#' Detection des points influents (distance de Cook)
points_influents <- function(modele, seuil = NULL) {
  cook <- stats::cooks.distance(modele)
  n <- length(cook)
  if (is.null(seuil) || length(seuil) == 0) seuil <- 4 / n
  df <- data.frame(
    Observation = seq_len(n),
    Cook = as.numeric(cook),
    Ajuste = stats::fitted(modele)
  )
  df$Influent <- df$Cook > seuil
  list(donnees = df, seuil = seuil, n_influents = sum(df$Influent))
}

#' Graphique de la distance de Cook
graph_cook <- function(modele) {
  pi <- points_influents(modele)
  df <- pi$donnees
  ggplot(df, aes(x = Observation, y = Cook)) +
    geom_segment(aes(xend = Observation, yend = 0), colour = "grey70") +
    geom_point(aes(colour = Influent), size = 1.8) +
    scale_colour_manual(values = c(`FALSE` = "#1f6f8b", `TRUE` = "#e07a5f"),
                        labels = c("Non", "Oui"), name = "Influent") +
    geom_hline(yintercept = pi$seuil, linetype = "dashed", colour = "#e07a5f") +
    labs(x = "Observation", y = "Distance de Cook",
         title = sprintf("Points influents (seuil = %.4f, %d detectes)", pi$seuil, pi$n_influents)) +
    theme_minimal(base_size = 13)
}

#' Facteurs d'inflation de la variance (multicolinearite)
vif_modele <- function(modele) {
  if (!requireNamespace("car", quietly = TRUE)) return(NULL)
  v <- tryCatch(car::vif(modele), error = function(e) NULL)
  if (is.null(v)) return(NULL)
  if (is.matrix(v)) {
    df <- data.frame(Terme = rownames(v), VIF = round(v[, 1], 3),
                     GVIF_ajuste = round(v[, 3], 3))
  } else {
    df <- data.frame(Terme = names(v), VIF = round(as.numeric(v), 3))
  }
  rownames(df) <- NULL
  df
}

#' Tracé des probabilites predites (distribution par statut observe)
graph_probabilites <- function(modele) {
  prob <- stats::fitted(modele)
  y <- stats::model.response(stats::model.frame(modele))
  df <- data.frame(Probabilite = prob, Statut = factor(y, levels = c(0, 1),
                                                       labels = c("Non cas", "Cas")))
  ggplot(df, aes(x = Probabilite, fill = Statut)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 30, colour = "white") +
    scale_fill_manual(values = c("Non cas" = "#1f6f8b", "Cas" = "#e07a5f")) +
    labs(x = "Probabilite predite", y = "Effectif", fill = "Statut observe",
         title = "Distribution des probabilites predites") +
    theme_minimal(base_size = 13)
}

# ---------------------------------------------------------------------
# 6. Selection de variables (pas a pas)
# ---------------------------------------------------------------------

#' Selection pas a pas selon l'AIC
selection_pas_a_pas <- function(df, outcome, predicteurs, direction = "both") {
  vars <- c(outcome, predicteurs)
  d <- df[, vars, drop = FALSE]
  d <- d[complete.cases(d), , drop = FALSE]
  nul <- stats::glm(stats::as.formula(paste(outcome, "~ 1")), data = d,
                    family = stats::binomial())
  complet <- stats::as.formula(paste(outcome, "~", paste(predicteurs, collapse = " + ")))
  res <- tryCatch(
    stats::step(nul, scope = list(lower = nul, upper = complet),
                direction = direction, trace = 0, k = 2),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  list(modele = res, variables_retenues = attr(stats::terms(res), "term.labels"),
       aic = round(stats::AIC(res), 2))
}

# ---------------------------------------------------------------------
# 7. Regression multiple (variable continue)
# ---------------------------------------------------------------------

#' Ajuster une regression lineaire multiple avec diagnostic
ajuster_lineaire <- function(df, outcome, predicteurs) {
  vars <- c(outcome, predicteurs)
  d <- df[, vars, drop = FALSE]
  d <- d[complete.cases(d), , drop = FALSE]
  formule <- stats::as.formula(paste(outcome, "~", paste(predicteurs, collapse = " + ")))
  modele <- stats::lm(formule, data = d)
  s <- summary(modele)
  coefs <- as.data.frame(s$coefficients)
  names(coefs) <- c("Coefficient", "Erreur_std", "t", "p_value")
  coefs$Terme <- rownames(coefs)
  rownames(coefs) <- NULL
  coefs$IC95_inf <- coefs$Coefficient - 1.96 * coefs$Erreur_std
  coefs$IC95_sup <- coefs$Coefficient + 1.96 * coefs$Erreur_std
  for (c in c("Coefficient", "Erreur_std", "t", "IC95_inf", "IC95_sup"))
    coefs[[c]] <- round(coefs[[c]], 4)
  coefs$p_value <- format.pval(coefs$p_value, digits = 4, eps = 0.0001)
  coefs <- coefs[, c("Terme", "Coefficient", "Erreur_std", "t",
                     "IC95_inf", "IC95_sup", "p_value")]
  qualite <- list(
    R2 = round(s$r.squared, 4),
    R2_ajuste = round(s$adj.r.squared, 4),
    F = round(s$fstatistic[1], 3),
    ddl = paste0(s$fstatistic[2], "; ", s$fstatistic[3]),
    p_modele = format.pval(stats::pf(s$fstatistic[1], s$fstatistic[2],
                                     s$fstatistic[3], lower.tail = FALSE), digits = 4),
    AIC = round(stats::AIC(modele), 2),
    RMSE = round(summary(modele)$sigma, 3)
  )
  list(modele = modele, coefficients = coefs, qualite = qualite, n = nrow(d))
}
