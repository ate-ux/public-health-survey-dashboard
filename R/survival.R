# =====================================================================
# survival.R
# Analyse de survie : estimation Kaplan-Meier, test du log-rank et
# modele de Cox. Le module s'active uniquement lorsqu'une variable
# de suivi (temps) et une variable d'evenement sont disponibles.
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(survival)
  library(ggplot2)
})

source("R/data_cleaning.R")

#' Preparer les donnees de survie
#'
#' Creer un temps de suivi et un indicateur d'evenement a partir d'une
#' variable quantitative (temps) et d'une variable binaire (evenement).
preparer_survie <- function(df, temps, evenement, groupe = NULL) {
  vars <- c(temps, evenement)
  if (!is.null(groupe)) vars <- c(vars, groupe)
  d <- df[, vars, drop = FALSE]
  d <- d[complete.cases(d), , drop = FALSE]
  d <- d[d[[temps]] > 0, , drop = FALSE]
  noms <- c("time", "status")
  if (!is.null(groupe)) noms <- c(noms, "group")
  names(d) <- noms
  d$status <- as.integer(d$status)
  if (!all(d$status %in% c(0, 1))) {
    return(list(erreur = "La variable evenement doit etre binaire 0/1."))
  }
  if (!is.null(groupe)) d$group <- as.factor(d$group)
  list(donnees = d, n = nrow(d), evenements = sum(d$status == 1))
}

#' Estimer la survie par Kaplan-Meier
estimer_km <- function(surv_data, groupe = FALSE) {
  formule <- if (groupe && "group" %in% names(surv_data))
    survival::Surv(time, status) ~ group else survival::Surv(time, status) ~ 1
  fit <- survival::survfit(formule, data = surv_data)
  fit
}

#' Tableau de survie Kaplan-Meier (temps, survie, IC)
tableau_km <- function(fit) {
  s <- summary(fit)
  res <- data.frame(
    Temps = s$time,
    N_risque = s$n.risk,
    N_evenement = s$n.event,
    Survie = round(s$surv, 4),
    Erreur_std = round(s$std.err, 4),
    IC95_inf = round(s$lower, 4),
    IC95_sup = round(s$upper, 4)
  )
  if (!is.null(s$strata)) res <- cbind(Strate = as.character(s$strata), res)
  if (nrow(res) > 60) res <- res[seq(1, nrow(res), length.out = 60), ]
  rownames(res) <- NULL
  res
}

#' Survie mediane par groupe
survie_mediane <- function(fit) {
  s <- summary(fit)$table
  if (is.null(dim(s))) {
    data.frame(Groupe = "Global", N = s["records"], Evenements = s["events"],
               Survie_mediane = round(s["median"], 2),
               IC95_inf = round(s["0.95LCL"], 2), IC95_sup = round(s["0.95UCL"], 2))
  } else {
    data.frame(
      Groupe = sub("^group=", "", rownames(s)),
      N = s[, "records"], Evenements = s[, "events"],
      Survie_mediane = round(s[, "median"], 2),
      IC95_inf = round(s[, "0.95LCL"], 2),
      IC95_sup = round(s[, "0.95UCL"], 2),
      stringsAsFactors = FALSE
    )
  }
}

#' Test du log-rank entre groupes
test_logrank <- function(surv_data) {
  if (!"group" %in% names(surv_data)) return(NULL)
  if (nlevels(surv_data$group) < 2) return(NULL)
  diff <- survival::survdiff(survival::Surv(time, status) ~ group, data = surv_data)
  p <- stats::pchisq(diff$chisq, length(diff$n) - 1, lower.tail = FALSE)
  list(
    statistique = round(diff$chisq, 3),
    ddl = length(diff$n) - 1,
    p_value = p,
    p_formate = format.pval(p, digits = 4, eps = 0.0001),
    interpretation = paste0(
      "Les courbes de survie des groupes sont ",
      if (p < 0.05) "significativement differentes" else "statistiquement comparables",
      " (log-rank, chi2 = ", round(diff$chisq, 2), ", p = ", format.pval(p, digits = 4), ").")
  )
}

#' Modele de Cox a risques proportionnels
ajuster_cox <- function(surv_data, covariables = NULL) {
  if (is.null(covariables) || length(covariables) == 0) {
    partie <- if ("group" %in% names(surv_data)) "group" else return(NULL)
    formule <- stats::as.formula(paste("survival::Surv(time, status) ~", partie))
  } else {
    formule <- stats::as.formula(paste("survival::Surv(time, status) ~",
                                       paste(covariables, collapse = " + ")))
  }
  modele <- tryCatch(survival::coxph(formule, data = surv_data), error = function(e) NULL)
  if (is.null(modele)) return(list(erreur = "Le modele de Cox n'a pas pu etre estime."))
  s <- summary(modele)
  coefs <- as.data.frame(s$coefficients)
  coefs$Terme <- rownames(coefs)
  rownames(coefs) <- NULL
  names(coefs)[1:5] <- c("Coefficient", "HR", "Erreur_std", "Z", "p_value")
  coefs$IC95_inf <- round(exp(coefs$Coefficient - 1.96 * coefs$Erreur_std), 4)
  coefs$IC95_sup <- round(exp(coefs$Coefficient + 1.96 * coefs$Erreur_std), 4)
  coefs[, c("Coefficient", "Erreur_std", "Z")] <- round(coefs[, c("Coefficient", "Erreur_std", "Z")], 4)
  coefs$HR <- round(coefs$HR, 4)
  coefs$p_value <- format.pval(coefs$p_value, digits = 4, eps = 0.0001)
  coefs <- coefs[, c("Terme", "Coefficient", "HR", "IC95_inf", "IC95_sup",
                     "Erreur_std", "Z", "p_value")]
  list(
    modele = modele, coefficients = coefs,
    qualite = list(
      Concordance = round(s$concordance[1], 4),
      Test_global_LR = round(s$logtest["test"], 3),
      p_global = format.pval(s$logtest["pvalue"], digits = 4),
      Test_Wald = round(s$waldtest["test"], 3),
      AIC = round(stats::AIC(modele), 2)
    )
  )
}

#' Verification de l'hypothese des risques proportionnels (residus de Schoenfeld)
test_ph_assomption <- function(modele_cox) {
  zph <- tryCatch(survival::cox.zph(modele_cox), error = function(e) NULL)
  if (is.null(zph)) return(NULL)
  tb <- as.data.frame(zph$table)
  tb$Terme <- rownames(tb)
  rownames(tb) <- NULL
  names(tb) <- c("rho", "chi2", "p_value", "Terme")
  tb$p_value <- format.pval(tb$p_value, digits = 4, eps = 0.0001)
  tb[, c("Terme", "rho", "chi2", "p_value")]
}

#' Graphique de survie Kaplan-Meier
#'
#' Construction directe a partir de l'objet survfit, sans dependance a
#' survminer. Un graphique en escalier est trace par strate.
graph_survie <- function(fit, titre = "Courbes de survie (Kaplan-Meier)",
                         couleurs = c("#1f6f8b", "#e07a5f", "#2a9d8f", "#3d5a80")) {
  if (is.null(fit)) return(NULL)
  if (is.null(fit$strata)) {
    df <- data.frame(Strate = "Global", Temps = fit$time, Survie = fit$surv)
  } else {
    niveaux <- levels(fit$strata)
    df <- do.call(rbind, lapply(niveaux, function(s) {
      idx <- which(fit$strata == s)
      data.frame(Strate = sub("^group=", "", s),
                 Temps = fit$time[idx], Survie = fit$surv[idx])
    }))
  }
  # Ajouter le point de depart (temps 0, survie 1)
  depart <- do.call(rbind, lapply(unique(df$Strate), function(s)
    data.frame(Strate = s, Temps = 0, Survie = 1)))
  df <- rbind(depart, df)
  df <- df[order(df$Strate, df$Temps), ]
  ggplot(df, aes(x = Temps, y = Survie, colour = Strate)) +
    geom_step(linewidth = 1) +
    scale_colour_manual(values = rep(couleurs, length.out = length(unique(df$Strate)))) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Temps de suivi", y = "Probabilite de survie",
         title = titre, colour = "Groupe") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}
