# =====================================================================
# report_generation.R
# Generation du rapport R Markdown a partir des analyses de l'application
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

#' Construire le contenu markdown d'un rapport d'analyse
#'
#' @param df donnees filtrees
#' @param titre titre du rapport
#' @param variables_selection vecteur de variables a decrire
#' @param resultat_bivarie liste issue de l'analyse bivariee (optionnel)
#' @param resultat_regression liste issue de la regression (optionnel)
#' @return chaine de caracteres markdown
construire_rapport <- function(df, titre = "Rapport d'analyse",
                               variables_selection = NULL,
                               resultat_bivarie = NULL,
                               resultat_regression = NULL,
                               filtres_texte = "") {
  source("R/data_cleaning.R")
  source("R/descriptive_analysis.R")

  q <- resume_qualite(df)
  md <- c()
  md <- c(md, sprintf("# %s", titre), "")
  md <- c(md, sprintf("*Genere le %s par le Public Health Survey Analytics Dashboard.*",
                      format(Sys.time(), "%d/%m/%Y a %H:%M")), "")
  if (nzchar(filtres_texte)) {
    md <- c(md, "## Filtres appliques", "", filtres_texte, "")
  }

  # 1. Vue d'ensemble
  md <- c(md, "## 1. Vue d'ensemble du jeu de donnees", "")
  md <- c(md, sprintf("- Observations : **%s**", format(q$n_lignes, big.mark = " ")))
  md <- c(md, sprintf("- Variables : **%d**", q$n_colonnes))
  md <- c(md, sprintf("- Cellules manquantes : **%s** (%.2f%%)",
                      format(q$cellules_manquantes, big.mark = " "), q$pct_manquant_global))
  md <- c(md, sprintf("- Lignes dupliquees : **%d**", q$lignes_doublons))
  md <- c(md, sprintf("- Colonnes completes : **%d** sur %d", q$colonnes_completes, q$n_colonnes))
  md <- c(md, "")

  # 2. Statistiques descriptives
  md <- c(md, "## 2. Statistiques descriptives", "")
  vars <- variables_selection %||% names(df)
  types <- detecter_types(df)
  vars_cont <- intersect(vars, names(types)[types == "continue"])
  cont <- tableau_descriptif_continu(df, vars_cont)
  if (!is.null(cont) && nrow(cont) > 0) {
    md <- c(md, "### 2.1 Variables quantitatives", "")
    md <- c(md, knitr::kable(head(cont, 30), format = "pipe"), "")
  }
  vars_disc <- intersect(vars, names(types)[types %in% c("binaire", "categorielle")])
  disc <- tableau_descriptif_discret(df, vars_disc)
  if (!is.null(disc) && nrow(disc) > 0) {
    keeps <- names(disc)[names(disc) %in% c("Variable", "Modalite", "Effectif", "Pourcentage")]
    md <- c(md, "### 2.2 Variables qualitatives (premieres modalites)", "")
    md <- c(md, knitr::kable(head(disc[, keeps, drop = FALSE], 25), format = "pipe"), "")
  }

  # 3. Analyse bivariee
  if (!is.null(resultat_bivarie)) {
    md <- c(md, "## 3. Analyse bivariee", "")
    if (!is.null(resultat_bivarie$test)) {
      md <- c(md, sprintf("**Test realise :** %s", resultat_bivarie$test))
      md <- c(md, "")
      md <- c(md, sprintf("- Statistique : %s", resultat_bivarie$statistique))
      md <- c(md, sprintf("- p-value : %s", resultat_bivarie$p_formate))
      md <- c(md, "")
      md <- c(md, resultat_bivarie$conclusion %||% resultat_bivarie$interpretation %||% "")
      md <- c(md, "")
    }
  }

  # 4. Regression
  if (!is.null(resultat_regression) && !is.null(resultat_regression$tableau)) {
    md <- c(md, "## 4. Regression logistique", "")
    md <- c(md, sprintf("**Modele :** %s",
                        paste(deparse(resultat_regression$formule), collapse = " ")), "")
    if (!is.null(resultat_regression$qualite)) {
      qq <- resultat_regression$qualite
      md <- c(md, sprintf("- Pseudo R2 (Nagelkerke) : %.4f", qq$Nagelkerke %||% NA))
      md <- c(md, sprintf("- AIC : %.2f", qq$AIC %||% NA))
      if (!is.null(qq$Concordance))
        md <- c(md, sprintf("- Concordance (C-statistique) : %.4f", qq$Concordance))
      md <- c(md, "")
    }
    md <- c(md, knitr::kable(resultat_regression$tableau, format = "pipe"), "")
    if (!is.null(resultat_regression$interpretation)) {
      md <- c(md, "### Interpretation des rapports de cotes", "")
      md <- c(md, resultat_regression$interpretation, "")
    }
  }

  # 5. Limites
  md <- c(md, "## 5. Limites et precautions d'interpretation", "")
  md <- c(md, "- Les associations observees sont de nature observationnelle : elles n'etablissent pas de causalite.")
  md <- c(md, "- Les analyses ne tiennent pas compte des poids de sondage ni du plan de sondage complexe dans cette version.")
  md <- c(md, sprintf("- %.2f%% des cellules sont manquantes : verifier le mecanisme de manque (MCAR, MAR, MNAR).", q$pct_manquant_global))
  md <- c(md, "- Les tests multiples n'ont pas fait l'objet d'une correction (Bonferroni ou FDR).")
  md <- c(md, "")
  md <- c(md, "---", "")
  md <- c(md, "*Rapport produit par le Public Health Survey Analytics Dashboard.*")
  paste(md, collapse = "\n")
}

#' Ecrire le rapport markdown dans un fichier temporaire
ecrire_rapport_rmd <- function(contenu_md, chemin_rmd,
                               chemin_csv = NULL, titre = "Rapport") {
  entete <- c("---",
              sprintf('title: "%s"', titre),
              "output:",
              "  html_document:",
              "    toc: true",
              "    toc_float: true",
              "    theme: flatly",
              "    df_print: paged",
              "  pdf_document:",
              "    toc: true",
              "---", "", "```{r setup, include=FALSE}",
              'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)',
              "```", "")
  writeLines(c(entete, contenu_md), chemin_rmd)
  chemin_rmd
}
