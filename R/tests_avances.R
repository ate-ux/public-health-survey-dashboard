# =====================================================================
# tests_avances.R
# Batterie de tests statistiques avec interpretation automatique :
# normalite, homogeneite, chi-deux, Fisher, t-test, Mann-Whitney,
# ANOVA, Kruskal-Wallis, correlations, post-hoc.
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/descriptive_analysis.R")

# ---------------------------------------------------------------------
# 1. Tests de normalite
# ---------------------------------------------------------------------

#' Test de Shapiro-Wilk (echantillons jusqu'a 5000 observations)
test_normalite <- function(x, nom = "variable") {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NULL)
  # Shapiro-Wilk limite a 5000 observations : echantillonner si besoin
  if (length(x) > 5000) {
    x <- sample(x, 5000)
    note <- " (echantillon aleatoire de 5000 observations)"
  } else {
    note <- ""
  }
  sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
  if (is.null(sw)) return(NULL)
  p <- sw$p.value
  data.frame(
    Variable = nom, N = length(x),
    W = round(as.numeric(sw$statistic), 4),
    p_value = format.pval(p, digits = 4, eps = 0.0001),
    Normalite = if (p < 0.05) "Rejetee (non normale)" else "Non rejetee (compatible normale)",
    Asymetrie = round(mean(((x - mean(x)) / sd(x))^3), 3),
    Interpretation = if (p < 0.05)
      paste0("La distribution de ", nom, " s'ecarte significativement de la loi normale", note, ".")
    else paste0("La distribution de ", nom, " est compatible avec la loi normale", note, "."),
    stringsAsFactors = FALSE
  )
}

#' Test de Kolmogorov-Smirnov contre une loi normale standardisee
test_ks <- function(x, nom = "variable") {
  x <- x[!is.na(x)]
  if (length(x) < 5) return(NULL)
  z <- (x - mean(x)) / sd(x)
  ks <- stats::ks.test(z, "pnorm")
  data.frame(Variable = nom, D = round(as.numeric(ks$statistic), 4),
             p_value = format.pval(ks$p.value, digits = 4, eps = 0.0001),
             Normalite = if (ks$p.value < 0.05) "Rejetee" else "Non rejetee",
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------
# 2. Tests d'homogeneite des variances
# ---------------------------------------------------------------------

#' Test de Levene (robuste) et de Bartlett
test_homogeneite <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  d[[var_grp]] <- as.factor(d[[var_grp]])
  res <- list()
  if (requireNamespace("car", quietly = TRUE)) {
    lv <- tryCatch(car::leveneTest(d[[var_num]] ~ d[[var_grp]]), error = function(e) NULL)
    if (!is.null(lv)) {
      res$Levene <- list(
        statistique = round(lv[["F value"]][1], 4),
        p_value = format.pval(lv[["Pr(>F)"]][1], digits = 4, eps = 0.0001),
        conclusion = if (lv[["Pr(>F)"]][1] < 0.05)
          "Variances inegales : preferer un test non parametrique." else
          "Variances homogenes : le test t ou l'ANOVA est valide."
      )
    }
  }
  bt <- tryCatch(stats::bartlett.test(d[[var_num]] ~ d[[var_grp]]), error = function(e) NULL)
  if (!is.null(bt)) {
    res$Bartlett <- list(
      statistique = round(as.numeric(bt$statistic), 4),
      p_value = format.pval(bt$p.value, digits = 4, eps = 0.0001),
      conclusion = if (bt$p.value < 0.05) "Variances inegales." else "Variances homogenes."
    )
  }
  if (length(res) == 0) return(NULL)
  do.call(rbind, lapply(names(res), function(n) {
    data.frame(Test = n, Statistique = res[[n]]$statistique,
               p_value = res[[n]]$p_value, Conclusion = res[[n]]$conclusion,
               stringsAsFactors = FALSE)
  }))
}

# ---------------------------------------------------------------------
# 3. Tests d'association entre variables categorielles
# ---------------------------------------------------------------------

#' Chi-deux, Fisher exact et V de Cramer sur un tableau croise
test_association <- function(df, var1, var2) {
  d <- df[, c(var1, var2)]
  d <- d[complete.cases(d), ]
  tab <- table(d[[var1]], d[[var2]])
  if (any(dim(tab) < 2)) return(NULL)
  n <- sum(tab)
  res <- data.frame()
  chi <- suppressWarnings(stats::chisq.test(tab))
  phi2 <- as.numeric(chi$statistic) / n
  v <- sqrt(phi2 / (min(dim(tab)) - 1))
  res <- rbind(res, data.frame(
    Test = "Chi-deux de Pearson",
    Statistique = round(as.numeric(chi$statistic), 4),
    Ddl = as.integer(chi$parameter),
    p_value = format.pval(chi$p.value, digits = 4, eps = 0.0001),
    Commentaire = sprintf("V de Cramer = %.3f", v),
    stringsAsFactors = FALSE))
  # Fisher si tableau 2x2 ou petits effectifs
  if (all(dim(tab) == c(2, 2)) || any(chi$expected < 5)) {
    ft <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
    if (!is.null(ft)) {
      res <- rbind(res, data.frame(
        Test = "Fisher exact",
        Statistique = "",
        Ddl = NA_integer_,
        p_value = format.pval(ft$p.value, digits = 4, eps = 0.0001),
        Commentaire = sprintf("Odds ratio = %.3f", as.numeric(ft$estimate)),
        stringsAsFactors = FALSE))
    }
  }
  # Test du rapport de vraisemblance
  lr <- tryCatch({
    suppressWarnings({
      g <- stats::glm(as.numeric(d[[var1]]) ~ d[[var2]], family = stats::binomial())
      chi_full <- stats::deviance(g); chi_null <- stats::deviance(stats::update(g, . ~ 1))
      st <- chi_null - chi_full; dl <- length(stats::coef(g)) - 1
      list(stat = st, dl = dl, p = stats::pchisq(st, dl, lower.tail = FALSE))
    })
  }, error = function(e) NULL)
  if (!is.null(lr)) {
    res <- rbind(res, data.frame(
      Test = "Rapport de vraisemblance",
      Statistique = round(lr$stat, 4), Ddl = lr$dl,
      p_value = format.pval(lr$p, digits = 4, eps = 0.0001),
      Commentaire = "", stringsAsFactors = FALSE))
  }
  attr(res, "v_cramer") <- round(v, 3)
  attr(res, "tableau") <- tab
  res
}

# ---------------------------------------------------------------------
# 4. Comparaison de deux groupes (numerique)
# ---------------------------------------------------------------------

#' Test t de Student, Welch et Mann-Whitney, avec tailles d'effet
test_comparaison_deux_groupes <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  d[[var_grp]] <- droplevels(as.factor(d[[var_grp]]))
  if (nlevels(d[[var_grp]]) != 2) return(NULL)
  g1 <- d[[var_num]][d[[var_grp]] == levels(d[[var_grp]])[1]]
  g2 <- d[[var_num]][d[[var_grp]] == levels(d[[var_grp]])[2]]
  n1 <- length(g1); n2 <- length(g2)
  s1 <- sd(g1); s2 <- sd(g2)
  # taille d'effet d de Cohen (variance mise en commun)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  d_cohen <- (mean(g1) - mean(g2)) / sp
  force <- if (abs(d_cohen) < 0.2) "negligeable" else if (abs(d_cohen) < 0.5) "petite" else
    if (abs(d_cohen) < 0.8) "moyenne" else "grande"
  tt <- stats::t.test(g1, g2)                    # Student
  tw <- stats::t.test(g1, g2, var.equal = FALSE) # Welch
  mw <- suppressWarnings(stats::wilcox.test(g1, g2))
  # correlation rang-biseriale (approx. de r)
  z <- stats::qnorm(mw$p.value / 2)
  r_rb <- abs(z) / sqrt(n1 + n2)
  data.frame(
    Test = c("t de Student", "t de Welch (variances inegales)", "Mann-Whitney (Wilcoxon)"),
    Statistique = c(round(as.numeric(tt$statistic), 4),
                    round(as.numeric(tw$statistic), 4),
                    round(as.numeric(mw$statistic), 4)),
    p_value = c(format.pval(tt$p.value, digits = 4, eps = 0.0001),
                format.pval(tw$p.value, digits = 4, eps = 0.0001),
                format.pval(mw$p.value, digits = 4, eps = 0.0001)),
    Taille_effet = c(sprintf("d = %.3f (%s)", d_cohen, force), "", sprintf("r = %.3f", r_rb)),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 5. Comparaison de k groupes
# ---------------------------------------------------------------------

#' ANOVA, Welch ANOVA et Kruskal-Wallis
test_comparaison_k_groupes <- function(df, var_num, var_grp) {
  d <- df[, c(var_num, var_grp)]
  d <- d[complete.cases(d), ]
  d[[var_grp]] <- droplevels(as.factor(d[[var_grp]]))
  k <- nlevels(d[[var_grp]])
  if (k < 3) return(NULL)
  av <- stats::aov(d[[var_num]] ~ d[[var_grp]])
  sm <- summary(av)[[1]]
  kw <- stats::kruskal.test(d[[var_num]] ~ d[[var_grp]])
  # eta carre (taille d'effet ANOVA)
  ss_between <- sm[["Sum Sq"]][1]; ss_total <- sum(sm[["Sum Sq"]])
  eta2 <- ss_between / ss_total
  force <- if (eta2 < 0.01) "negligeable" else if (eta2 < 0.06) "petite" else
    if (eta2 < 0.14) "moyenne" else "grande"
  data.frame(
    Test = c("ANOVA a un facteur", "Kruskal-Wallis"),
    Statistique = c(sprintf("F = %.3f", sm[["F value"]][1]),
                    sprintf("chi2 = %.3f", as.numeric(kw$statistic))),
    Ddl = c(paste0(sm[["Df"]][1], "; ", sm[["Df"]][2]), as.integer(kw$parameter)),
    p_value = c(format.pval(sm[["Pr(>F)"]][1], digits = 4, eps = 0.0001),
                format.pval(kw$p.value, digits = 4, eps = 0.0001)),
    Taille_effet = c(sprintf("eta2 = %.3f (%s)", eta2, force), ""),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 6. Tests de correlation
# ---------------------------------------------------------------------

#' Correlation de Pearson, Spearman et Kendall avec interpretation
test_correlation_complet <- function(x, y) {
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 4) return(NULL)
  pear <- stats::cor.test(x, y, method = "pearson")
  spear <- suppressWarnings(stats::cor.test(x, y, method = "spearman"))
  kend <- suppressWarnings(stats::cor.test(x, y, method = "kendall"))
  res <- data.frame(
    Methode = c("Pearson", "Spearman", "Kendall"),
    Coefficient = round(c(pear$estimate, spear$estimate, kend$estimate), 4),
    p_value = c(format.pval(pear$p.value, digits = 4, eps = 0.0001),
                format.pval(spear$p.value, digits = 4, eps = 0.0001),
                format.pval(kend$p.value, digits = 4, eps = 0.0001)),
    IC95_inf = round(c(pear$conf.int[1], NA, NA), 4),
    IC95_sup = round(c(pear$conf.int[2], NA, NA), 4),
    stringsAsFactors = FALSE
  )
  r <- as.numeric(pear$estimate)
  force <- if (abs(r) < 0.1) "negligeable" else if (abs(r) < 0.3) "faible" else
    if (abs(r) < 0.5) "moderee" else if (abs(r) < 0.7) "forte" else "tres forte"
  attr(res, "interpretation") <- sprintf(
    "Correlation de Pearson r = %.3f (%s, sens %s), p = %s.",
    r, force, if (r >= 0) "positif" else "negatif",
    format.pval(pear$p.value, digits = 4))
  res
}

# ---------------------------------------------------------------------
# 7. Batterie automatisee
# ---------------------------------------------------------------------

#' Lancer une batterie de tests selon les types des deux variables
batterie_tests <- function(df, var1, var2) {
  t1 <- detecter_types(df)[var1]
  t2 <- detecter_types(df)[var2]
  resultats <- list()
  if (t1 == "continue" && t2 == "continue") {
    resultats$correlation <- test_correlation_complet(df[[var1]], df[[var2]])
  } else if (xor(t1 == "continue", t2 == "continue")) {
    num <- if (t1 == "continue") var1 else var2
    grp <- if (t1 == "continue") var2 else var1
    resultats$normalite <- test_normalite(df[[num]])
    resultats$homogeneite <- test_homogeneite(df, num, grp)
    resultats$comparaison_2 <- test_comparaison_deux_groupes(df, num, grp)
    resultats$comparaison_k <- test_comparaison_k_groupes(df, num, grp)
    resultats$posthoc <- test_posthoc_tukey(df, num, grp)
  } else {
    resultats$association <- test_association(df, var1, var2)
  }
  resultats
}
