# =====================================================================
# survey_analysis.R
# Prise en compte du plan de sondage complexe NHANES
# (poids d'examen, strates, unites primaires de sondage)
#
# Estimation des prevalences et des moyennes avec intervalles de
# confiance tenant compte de l'echantillonnage, et modeles ponderes.
#
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(survey)
})

source("R/data_cleaning.R")

# Variables de plan reconnues dans NHANES
VARS_PLAN <- list(
  poids = c("WTMEC2YR", "WTINT2YR", "WTMEC4YR", "WTINT4YR"),
  strate = "SDMVSTRA",
  psu = "SDMVPSU"
)

# ---------------------------------------------------------------------
# 1. Detection et construction du plan
# ---------------------------------------------------------------------

#' Detecter les variables de plan de sondage disponibles
detecter_vars_plan <- function(df) {
  poids <- intersect(VARS_PLAN$poids, names(df))
  list(
    poids = if (length(poids) > 0) poids else NULL,
    strate = intersect(VARS_PLAN$strate, names(df)),
    psu = intersect(VARS_PLAN$psu, names(df))
  )
}

#' Verifier que le plan de sondage est exploitable
verifier_plan <- function(df) {
  v <- detecter_vars_plan(df)
  if (is.null(v$poids)) {
    return(list(ok = FALSE, message = "Aucune variable de ponderation detectee."))
  }
  if (length(v$strate) == 0 || length(v$psu) == 0) {
    return(list(ok = FALSE,
                message = "Strates ou unites primaires de sondage absentes."))
  }
  list(ok = TRUE, poids = v$poids, strate = v$strate[1], psu = v$psu[1])
}

#' Construire l'objet plan de sondage
#'
#' @param df donnees
#' @param poids nom de la variable de ponderation (defaut : premiere detectee)
#' @return objet svydesign, ou NULL avec message d'erreur
construire_plan <- function(df, poids = NULL) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    return(list(erreur = "Le paquet survey n'est pas installe."))
  }
  v <- verifier_plan(df)
  if (isFALSE(v$ok)) return(list(erreur = v$message))
  poids <- poids %||% v$poids[1]
  if (!poids %in% names(df)) {
    return(list(erreur = paste("Variable de ponderation inconnue :", poids)))
  }
  d <- df
  # Ecarter les observations sans poids ou sans strate
  d <- d[!is.na(d[[poids]]) & d[[poids]] > 0 &
           !is.na(d[[v$strate]]) & !is.na(d[[v$psu]]), , drop = FALSE]
  if (nrow(d) == 0) {
    return(list(erreur = "Aucune observation avec un plan de sondage complet."))
  }
  # Les strates a une seule unite primaire provoquent une variance non estimable
  tab <- table(d[[v$strate]], d[[v$psu]])
  strates_singles <- names(which(rowSums(tab > 0) < 2))
  options(survey.lonely.psu = "adjust")
  plan <- tryCatch(
    survey::svydesign(
      ids = stats::as.formula(paste0("~", v$psu)),
      strata = stats::as.formula(paste0("~", v$strate)),
      weights = stats::as.formula(paste0("~", poids)),
      data = d,
      nest = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(plan)) {
    return(list(erreur = "Le plan de sondage n'a pas pu etre construit."))
  }
  list(
    plan = plan,
    poids = poids,
    strate = v$strate,
    psu = v$psu,
    n = nrow(d),
    n_strates = length(unique(d[[v$strate]])),
    n_psu = length(unique(d[[v$psu]])),
    # degres de liberte : nombre d'unites primaires moins le nombre de strates,
    # convention du plan de sondage pour les tests
    ddl = survey::degf(plan),
    poids_total = sum(d[[poids]]),
    poids_median = stats::median(d[[poids]]),
    strates_uniques = length(strates_singles)
  )
}

# ---------------------------------------------------------------------
# 2. Estimation ponderee de prevalences
# ---------------------------------------------------------------------

#' Prevalence ponderee d'un indicateur binaire, avec intervalle de confiance
#'
#' @param plan objet svydesign
#' @param variable nom de la variable, codee 0/1
#' @param valeur valeur consideree comme cas (defaut 1)
prevalence_ponderee <- function(plan, variable, valeur = 1) {
  if (!variable %in% names(plan$variables)) return(NULL)
  x <- plan$variables[[variable]]
  ok <- !is.na(x)
  if (sum(ok) < 10) return(NULL)

  # indicateur binaire ajoute aux donnees du plan
  plan2 <- plan
  plan2$variables$.cas <- ifelse(is.na(x), NA_real_, as.numeric(x == valeur))

  est <- tryCatch(survey::svymean(~.cas, design = plan2, na.rm = TRUE),
                  error = function(e) NULL)
  if (is.null(est)) return(NULL)
  ci <- stats::confint(est)

  p <- as.numeric(est[1])
  se <- as.numeric(survey::SE(est)[1])
  n_val <- sum(ok)
  n_cas <- sum(x == valeur, na.rm = TRUE)

  data.frame(
    Variable = variable,
    N_valide = n_val,
    N_cas = n_cas,
    # effectif equivalent en population : cas x poids
    N_cas_pondere = round(sum(stats::weights(plan)[ok & x == valeur])),
    Prevalence_brute_pct = round(n_cas / n_val * 100, 2),
    Prevalence_ponderee_pct = round(p * 100, 2),
    Erreur_std_pct = round(se * 100, 3),
    IC95_inf_pct = round(ci[1] * 100, 2),
    IC95_sup_pct = round(ci[2] * 100, 2),
    stringsAsFactors = FALSE
  )
}

#' Tableau de prevalences ponderees pour plusieurs indicateurs
tableau_prevalences_ponderees <- function(df, plan_info, variables, libelles = NULL) {
  if (is.null(plan_info) || !is.null(plan_info$erreur)) return(NULL)
  plan <- plan_info$plan
  res <- lapply(variables, function(v) {
    if (!v %in% names(plan$variables)) return(NULL)
    p <- tryCatch(prevalence_ponderee(plan, v), error = function(e) NULL)
    if (!is.null(p) && !is.null(libelles) && v %in% names(libelles)) {
      p$Variable <- unname(libelles[v])
    }
    p
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# ---------------------------------------------------------------------
# 3. Moyennes ponderees
# ---------------------------------------------------------------------

#' Moyenne ponderee d'une variable continue avec intervalle de confiance
moyenne_ponderee <- function(plan, variable) {
  if (!variable %in% names(plan$variables)) return(NULL)
  form <- stats::as.formula(paste0("~", variable))
  est <- tryCatch(survey::svymean(form, design = plan, na.rm = TRUE),
                  error = function(e) NULL)
  if (is.null(est)) return(NULL)
  ci <- stats::confint(est)
  x <- plan$variables[[variable]]
  data.frame(
    Variable = variable,
    N_valide = sum(!is.na(x)),
    Moyenne_brute = round(mean(x, na.rm = TRUE), 3),
    Moyenne_ponderee = round(as.numeric(est[1]), 3),
    Erreur_std = round(as.numeric(survey::SE(est)[1]), 4),
    IC95_inf = round(ci[1], 3),
    IC95_sup = round(ci[2], 3),
    stringsAsFactors = FALSE
  )
}

#' Mediane ponderee
mediane_ponderee <- function(plan, variable) {
  if (!variable %in% names(plan$variables)) return(NULL)
  form <- stats::as.formula(paste0("~", variable))
  est <- tryCatch(survey::svyquantile(form, design = plan,
                                      quantiles = 0.5, ci = TRUE, na.rm = TRUE),
                  error = function(e) NULL)
  if (is.null(est)) return(NULL)
  m <- tryCatch(as.numeric(est[[1]][, 1]), error = function(e) NULL)
  x <- plan$variables[[variable]]
  data.frame(
    Variable = variable,
    Mediane_brute = round(stats::median(x, na.rm = TRUE), 3),
    Mediane_ponderee = round(m[1], 3),
    stringsAsFactors = FALSE
  )
}

#' Tableau des moyennes et medianes ponderees
tableau_moyennes_ponderees <- function(df, plan_info, variables) {
  if (is.null(plan_info) || !is.null(plan_info$erreur)) return(NULL)
  plan <- plan_info$plan
  res <- lapply(variables, function(v) {
    if (!is.numeric(plan$variables[[v]])) return(NULL)
    m <- tryCatch(moyenne_ponderee(plan, v), error = function(e) NULL)
    md <- tryCatch(mediane_ponderee(plan, v), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    if (!is.null(md)) m$Mediane_ponderee <- md$Mediane_ponderee
    m
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# ---------------------------------------------------------------------
# 4. Prevalences par domaine
# ---------------------------------------------------------------------

#' Prevalence ponderee par sous-groupe (domaine)
prevalence_par_domaine <- function(plan, variable, domaine, valeur = 1) {
  if (!all(c(variable, domaine) %in% names(plan$variables))) return(NULL)
  d <- plan$variables
  modalites <- sort(unique(stats::na.omit(d[[domaine]])))
  if (length(modalites) < 2 || length(modalites) > 15) return(NULL)

  # indicateur binaire porte par les donnees du plan
  plan2 <- plan
  x <- d[[variable]]
  plan2$variables$.cas <- ifelse(is.na(x), NA_real_, as.numeric(x == valeur))

  res <- lapply(modalites, function(m) {
    # sous-population par domaine, variance correcte
    sub <- tryCatch(
      eval(substitute(subset(plan2, DOM == m), list(DOM = as.name(domaine)))),
      error = function(e) NULL)
    if (is.null(sub)) return(NULL)
    est <- tryCatch(survey::svymean(~.cas, sub, na.rm = TRUE),
                    error = function(e) NULL)
    if (is.null(est)) return(NULL)
    ci <- stats::confint(est)
    data.frame(
      Domaine = domaine,
      Modalite = as.character(m),
      N = sum(d[[domaine]] == m, na.rm = TRUE),
      Prevalence_ponderee_pct = round(as.numeric(est[1]) * 100, 2),
      Erreur_std_pct = round(as.numeric(survey::SE(est)[1]) * 100, 3),
      IC95_inf_pct = round(ci[1] * 100, 2),
      IC95_sup_pct = round(ci[2] * 100, 2),
      stringsAsFactors = FALSE
    )
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# ---------------------------------------------------------------------
# 5. Comparaison de moyennes et tests ponderes
# ---------------------------------------------------------------------

#' Test de comparaison de moyennes ponderees entre groupes
test_moyennes_ponderees <- function(plan, variable, groupe) {
  if (!all(c(variable, groupe) %in% names(plan$variables))) return(NULL)
  form <- stats::as.formula(paste0("~", variable))
  grp <- stats::as.formula(paste0("~", groupe))
  moy <- tryCatch(survey::svyby(form, grp, design = plan, FUN = survey::svymean,
                                na.rm = TRUE), error = function(e) NULL)
  if (is.null(moy) || nrow(moy) < 2) return(NULL)
  # colonnes : le groupe, la moyenne (nommee comme la variable), puis l'erreur standard
  col_moy <- variable
  if (!col_moy %in% names(moy)) {
    col_moy <- setdiff(names(moy), c(groupe, "se"))[1]
  }
  if (is.na(col_moy) || !"se" %in% names(moy)) return(NULL)
  # test d'independance sur la moyenne
  mod <- tryCatch(survey::svyglm(
    stats::as.formula(paste0(variable, " ~ as.factor(", groupe, ")")),
    design = plan), error = function(e) NULL)
  if (is.null(mod)) return(NULL)
  sm <- summary(mod)$coefficients
  # deuxieme ligne : le contraste entre les deux premiers niveaux
  if (nrow(sm) < 2) return(NULL)
  est <- sm[2, 1]; se <- sm[2, 2]
  # statistique t avec degres de liberte du plan
  tstat <- est / se
  df_res <- max(1, survey::degf(plan))
  p <- 2 * stats::pt(-abs(tstat), df = df_res)
  moy_tab <- data.frame(
    Groupe = as.character(moy[[groupe]]),
    Moyenne_ponderee = round(as.numeric(moy[[col_moy]]), 3),
    Erreur_std = round(as.numeric(moy[["se"]]), 4),
    stringsAsFactors = FALSE
  )
  list(
    disponible = TRUE,
    tableau = moy_tab,
    difference = round(est, 3),
    erreur_std = round(se, 4),
    statistique_t = round(tstat, 3),
    ddl = df_res,
    p_value = p,
    p_formate = format.pval(p, digits = 4, eps = 0.0001),
    interpretation = paste0(
      "La difference de moyenne ponderee de ", variable, " entre les groupes est ",
      if (p < 0.05) "statistiquement significative" else "non significative",
      " (t = ", round(tstat, 2), ", ddl = ", df_res,
      ", p = ", format.pval(p, digits = 4), ").")
  )
}

#' Test du chi-deux de Rao-Scott (association entre deux variables)
test_rao_scott <- function(plan, var1, var2) {
  if (!all(c(var1, var2) %in% names(plan$variables))) return(NULL)
  tab <- tryCatch(
    survey::svychisq(stats::as.formula(paste0("~", var1, "+", var2)),
                     design = plan, statistic = "Chisq", na.rm = TRUE),
    error = function(e) NULL)
  if (is.null(tab)) return(NULL)
  rs <- tryCatch(
    survey::svychisq(stats::as.formula(paste0("~", var1, "+", var2)),
                     design = plan, statistic = "F", na.rm = TRUE),
    error = function(e) NULL)
  data.frame(
    Test = c("Chi-deux de Rao-Scott (design corrige)",
             "Test F de Rao-Scott"),
    Statistique = c(round(as.numeric(tab$statistic), 3),
                    if (!is.null(rs)) round(as.numeric(rs$statistic), 3) else NA),
    Ddl = c(as.integer(tab$parameter),
            if (!is.null(rs)) paste0(round(rs$parameter[1]), "; ",
                                     round(rs$parameter[2], 1)) else NA),
    p_value = c(format.pval(tab$p.value, digits = 4, eps = 0.0001),
                if (!is.null(rs)) format.pval(rs$p.value, digits = 4, eps = 0.0001) else NA),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 6. Modeles ponderes
# ---------------------------------------------------------------------

#' Regression logistique ponderee tenant compte du plan de sondage
ajuster_logistique_pondere <- function(plan, outcome, predicteurs) {
  predicteurs <- unique(predicteurs)
  predicteurs <- predicteurs[!is.na(predicteurs) & nzchar(predicteurs)]
  if (length(predicteurs) == 0) {
    return(list(erreur = "Selectionnez au moins une variable explicative."))
  }
  if (!outcome %in% names(plan$variables)) {
    return(list(erreur = "Variable reponse absente du plan."))
  }
  y <- plan$variables[[outcome]]
  if (!all(unique(na.omit(y)) %in% c(0, 1))) {
    return(list(erreur = "La variable reponse doit etre binaire codee 0/1."))
  }
  formule <- stats::as.formula(paste(outcome, "~", paste(predicteurs, collapse = " + ")))
  modele <- tryCatch(
    survey::svyglm(formule, design = plan, family = stats::quasibinomial()),
    error = function(e) NULL)
  if (is.null(modele)) {
    return(list(erreur = "Le modele pondere n'a pas pu etre estime."))
  }
  s <- summary(modele)$coefficients
  co <- s[, 1]; se <- s[, 2]; p <- s[, 4]
  res <- data.frame(
    Terme = rownames(s),
    Coefficient = round(co, 4),
    Erreur_std = round(se, 4),
    OR = round(exp(co), 4),
    IC95_inf = round(exp(co - 1.96 * se), 4),
    IC95_sup = round(exp(co + 1.96 * se), 4),
    p_value = format.pval(p, digits = 4, eps = 0.0001),
    Significatif = ifelse(p < 0.05, "Oui", "Non"),
    stringsAsFactors = FALSE
  )
  rownames(res) <- NULL
  # pseudo R2 : deviance du modele nul recalculee sur le meme plan
  r2 <- tryCatch({
    f0 <- stats::as.formula(paste(outcome, "~ 1"))
    m0 <- survey::svyglm(f0, design = plan, family = stats::quasibinomial())
    round(1 - stats::deviance(modele) / stats::deviance(m0), 4)
  }, error = function(e) NA_real_)
  # AIC() sur un modele svyglm renvoie plusieurs criteres (eff.p, AIC, deltabar) :
  # on extrait explicitement le terme nomme AIC
  aic_val <- tryCatch({
    a <- stats::AIC(modele)
    if ("AIC" %in% names(a)) as.numeric(a[["AIC"]]) else as.numeric(a)[1]
  }, error = function(e) NA_real_)
  list(
    modele = modele,
    coefficients = res,
    formule = formule,
    n = survey::degf(plan),
    degres_liberte = survey::degf(plan),
    AIC = if (is.na(aic_val)) NA_real_ else round(aic_val, 2),
    pseudo_r2 = r2,
    interpretation = paste0(
      "Les rapports de cotes sont estimes en tenant compte du plan de sondage complexe ",
      "(poids, strates et unites primaires), avec ", survey::degf(plan),
      " degres de liberte. Les intervalles reposent sur la loi de Student a ce nombre de degres.")
  )
}

#' Regression lineaire ponderee
ajuster_lineaire_pondere <- function(plan, outcome, predicteurs) {
  predicteurs <- unique(predicteurs)
  predicteurs <- predicteurs[!is.na(predicteurs) & nzchar(predicteurs)]
  if (length(predicteurs) == 0) {
    return(list(erreur = "Selectionnez au moins une variable explicative."))
  }
  formule <- stats::as.formula(paste(outcome, "~", paste(predicteurs, collapse = " + ")))
  modele <- tryCatch(survey::svyglm(formule, design = plan),
                     error = function(e) NULL)
  if (is.null(modele)) return(list(erreur = "Le modele pondere n'a pas pu etre estime."))
  s <- summary(modele)$coefficients
  res <- data.frame(
    Terme = rownames(s),
    Coefficient = round(s[, 1], 4),
    Erreur_std = round(s[, 2], 4),
    t = round(s[, 3], 3),
    p_value = format.pval(s[, 4], digits = 4, eps = 0.0001),
    stringsAsFactors = FALSE
  )
  rownames(res) <- NULL
  list(modele = modele, coefficients = res,
       R2 = round(1 - stats::deviance(modele) / modele$null.deviance, 4),
       degres_liberte = survey::degf(plan))
}

# ---------------------------------------------------------------------
# 7. Effet de plan (design effect)
# ---------------------------------------------------------------------

#' Comparer une estimation simple et une estimation ponderee
effet_de_plan <- function(df, plan_info, variable, valeur = 1) {
  if (is.null(plan_info) || !is.null(plan_info$erreur)) return(NULL)
  plan <- plan_info$plan
  x <- plan$variables[[variable]]
  ok <- !is.na(x)
  if (sum(ok) < 10) return(NULL)

  # estimation simple
  y <- as.numeric(x[ok] == valeur)
  p_brut <- mean(y)
  se_brut <- sqrt(p_brut * (1 - p_brut) / length(y))

  # estimation ponderee
  dd <- plan
  dd$variables$.cas <- NA_real_
  dd$variables$.cas[ok] <- y
  est <- survey::svymean(~.cas, dd, na.rm = TRUE)
  p_pond <- as.numeric(est[1])
  se_pond <- as.numeric(survey::SE(est)[1])

  deff <- (se_pond^2) / (se_brut^2)
  data.frame(
    Variable = variable,
    Prevalence_brute_pct = round(p_brut * 100, 2),
    IC95_brut = paste0("[", round((p_brut - 1.96 * se_brut) * 100, 1), " ; ",
                       round((p_brut + 1.96 * se_brut) * 100, 1), "]"),
    Prevalence_ponderee_pct = round(p_pond * 100, 2),
    IC95_pondere = paste0("[", round((p_pond - 1.96 * se_pond) * 100, 1), " ; ",
                          round((p_pond + 1.96 * se_pond) * 100, 1), "]"),
    Effet_de_plan = round(deff, 3),
    Largeur_IC_brut = round(2 * 1.96 * se_brut * 100, 2),
    Largeur_IC_pondere = round(2 * 1.96 * se_pond * 100, 2),
    stringsAsFactors = FALSE
  )
}

#' Tableau des effets de plan pour plusieurs indicateurs
tableau_effets_plan <- function(df, plan_info, variables) {
  if (is.null(plan_info) || !is.null(plan_info$erreur)) return(NULL)
  res <- lapply(variables, function(v) {
    tryCatch(effet_de_plan(df, plan_info, v), error = function(e) NULL)
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# ---------------------------------------------------------------------
# 8. Graphiques
# ---------------------------------------------------------------------

#' Comparaison graphique des prevalences brutes et ponderees
graph_prevalences_comparees <- function(df, plan_info, variables,
                                        libelles = NULL) {
  if (is.null(plan_info) || !is.null(plan_info$erreur)) return(NULL)
  tab <- tableau_prevalences_ponderees(df, plan_info, variables, libelles)
  if (is.null(tab) || nrow(tab) == 0) return(NULL)
  d <- rbind(
    data.frame(Indicateur = tab$Variable, Type = "Brute",
               Prevalence = tab$Prevalence_brute_pct),
    data.frame(Indicateur = tab$Variable, Type = "Ponderee",
               Prevalence = tab$Prevalence_ponderee_pct)
  )
  d$Indicateur <- factor(d$Indicateur, levels = unique(tab$Variable))
  ggplot(d, aes(x = Indicateur, y = Prevalence, fill = Type)) +
    geom_col(position = "dodge", alpha = 0.9) +
    scale_fill_manual(values = c(Brute = "#8aa6b5", Ponderee = "#1f6f8b")) +
    coord_flip() +
    labs(x = "", y = "Prevalence (%)", fill = "Estimation",
         title = "Prevalences brutes et ponderees") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

#' Prevalences ponderees par domaine, avec intervalles de confiance
graph_prevalence_domaine <- function(plan, variable, domaine, valeur = 1) {
  tab <- prevalence_par_domaine(plan, variable, domaine, valeur)
  if (is.null(tab) || nrow(tab) == 0) return(NULL)
  tab$Modalite <- factor(tab$Modalite,
                         levels = tab$Modalite[order(tab$Prevalence_ponderee_pct)])
  ggplot(tab, aes(x = Modalite, y = Prevalence_ponderee_pct)) +
    geom_col(fill = "#1f6f8b", alpha = 0.9, width = 0.62) +
    geom_errorbar(aes(ymin = IC95_inf_pct, ymax = IC95_sup_pct),
                  width = 0.18, colour = "#3d5a80") +
    geom_text(aes(label = paste0(Prevalence_ponderee_pct, "%")),
              hjust = -0.15, size = 3.2) +
    coord_flip() +
    labs(x = domaine, y = "Prevalence ponderee (%)",
         title = paste(variable, "selon", domaine, "(pondere)")) +
    theme_minimal(base_size = 13) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)))
}
