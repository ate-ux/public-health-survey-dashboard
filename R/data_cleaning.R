# =====================================================================
# data_cleaning.R
# Chargement, typage, contrôle qualite et diagnostic des donnees
# Projet : Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# ---------------------------------------------------------------------
# 1. Chargement
# ---------------------------------------------------------------------

#' Charger un fichier CSV en preservant les noms de colonnes
#'
#' @param path chemin du fichier
#' @param sep separateur (auto-detection si NULL)
#' @return data.frame
charger_csv <- function(path, sep = NULL) {
  if (is.null(sep)) {
    df <- readr::read_csv(path, show_col_types = FALSE,
                          na = c("", "NA", "N/A", "na", "999", "9999"))
  } else {
    df <- readr::read_delim(path, delim = sep, show_col_types = FALSE,
                            na = c("", "NA", "N/A", "na", "999", "9999"))
  }
  as.data.frame(df)
}

#' Charger le dictionnaire de variables (codebook)
charger_codebook <- function(path = "data/codebook.csv") {
  if (file.exists(path)) {
    as.data.frame(readr::read_csv(path, show_col_types = FALSE))
  } else {
    NULL
  }
}

# ---------------------------------------------------------------------
# 2. Typage automatique
# ---------------------------------------------------------------------

#' Retourne le type logique de chaque colonne
#' @return vecteur nomme parmi "continue", "binaire", "categorielle", "id"
detecter_types <- function(df) {
  vapply(names(df), function(v) {
    x <- df[[v]]
    if (is.factor(x)) {
      n <- nlevels(x)
      if (n == 2) return("binaire")
      if (n <= 12) return("categorielle")
      return("categorielle")
    }
    if (is.character(x)) {
      n <- length(unique(na.omit(x)))
      if (n == 2) return("binaire")
      return("categorielle")
    }
    if (is.logical(x)) return("binaire")
    if (is.numeric(x)) {
      u <- unique(na.omit(x))
      # Identifiant probable : entier, unique, croissant
      if (length(u) == sum(!is.na(x)) && length(u) > 50 &&
          all(u == round(u))) return("id")
      if (length(u) <= 2) return("binaire")
      if (length(u) <= 10 && all(u == round(u))) return("categorielle")
      return("continue")
    }
    "categorielle"
  }, character(1))
}

#' Libelle lisible d'une variable a partir du codebook
libelle_variable <- function(v, codebook = NULL) {
  if (is.null(codebook)) return(v)
  hit <- codebook$libelle[codebook$variable == v]
  if (length(hit) == 0) return(v)
  paste0(v, " : ", hit[1])
}

# ---------------------------------------------------------------------
# 3. Controle qualite
# ---------------------------------------------------------------------

#' Tableau de completude par variable
tableau_completude <- function(df) {
  n <- nrow(df)
  data.frame(
    Variable   = names(df),
    Type       = unname(detecter_types(df)),
    N_valides  = vapply(df, function(x) sum(!is.na(x)), numeric(1)),
    N_manquant = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    Pct_manquant = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
    N_uniques  = vapply(df, function(x) length(unique(na.omit(x))), numeric(1)),
    stringsAsFactors = FALSE
  ) |> dplyr::arrange(dplyr::desc(Pct_manquant))
}

#' Resume global de la qualite des donnees
resume_qualite <- function(df) {
  n <- nrow(df); p <- ncol(df)
  cellules <- n * p
  manquantes <- sum(is.na(df))
  doublons <- sum(duplicated(df))
  list(
    n_lignes = n,
    n_colonnes = p,
    cellules = cellules,
    cellules_manquantes = manquantes,
    pct_manquant_global = round(manquantes / cellules * 100, 2),
    lignes_doublons = doublons,
    colonnes_completes = sum(colSums(is.na(df)) == 0),
    colonnes_critiques = sum(colMeans(is.na(df)) > 0.5)
  )
}

#' Detecter les valeurs aberrantes par la regle de l'ecart interquartile
detecter_outliers <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  borne_basse <- q1 - k * iqr
  borne_haute <- q3 + k * iqr
  which(x < borne_basse | x > borne_haute)
}

#' Tableau recapitulatif des valeurs aberrantes pour les variables continues
tableau_outliers <- function(df, k = 1.5) {
  types <- detecter_types(df)
  vars <- names(types)[types == "continue"]
  if (length(vars) == 0) return(NULL)
  res <- lapply(vars, function(v) {
    x <- df[[v]]
    idx <- detecter_outliers(x, k)
    q1 <- quantile(x, 0.25, na.rm = TRUE); q3 <- quantile(x, 0.75, na.rm = TRUE)
    data.frame(
      Variable = v,
      N_outliers = length(idx),
      Pct = round(length(idx) / sum(!is.na(x)) * 100, 2),
      Borne_basse = round(q1 - k * (q3 - q1), 2),
      Borne_haute = round(q3 + k * (q3 - q1), 2),
      Min = round(min(x, na.rm = TRUE), 2),
      Max = round(max(x, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res)
}

#' Valeurs extremes observees (les n plus grandes et plus petites) par variable
tableau_extremes <- function(df, n = 3) {
  types <- detecter_types(df)
  vars <- names(types)[types == "continue"]
  res <- lapply(vars, function(v) {
    x <- df[[v]]; x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    data.frame(
      Variable = v,
      Minimum = paste(round(sort(x)[seq_len(min(n, length(x)))], 2), collapse = " | "),
      Maximum = paste(round(sort(x, decreasing = TRUE)[seq_len(min(n, length(x)))], 2), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

#' Detection de variables a variance nulle ou quasi nulle
tableau_variance_nulle <- function(df, seuil = 0.95) {
  res <- lapply(names(df), function(v) {
    x <- na.omit(df[[v]])
    if (length(x) == 0) return(NULL)
    tab <- table(x)
    prop <- max(tab) / sum(tab)
    if (prop >= seuil) {
      data.frame(Variable = v, Modalite_dominante = names(tab)[which.max(tab)],
                 Proportion = round(prop * 100, 1), stringsAsFactors = FALSE)
    } else NULL
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

#' Controle de coherence interne : exemples de regles de plausibilite
controle_coherence <- function(df) {
  problemes <- list()
  ajouter <- function(regle, n, detail) {
    if (n > 0) problemes[[length(problemes) + 1]] <<- data.frame(
      Regle = regle, N_incoherences = n, Detail = detail, stringsAsFactors = FALSE)
  }
  if (all(c("BMXHT", "BMXWT") %in% names(df))) {
    n <- sum(!is.na(df$BMXHT) & !is.na(df$BMXWT) &
               abs(df$BMXHT - pmax(df$BMXHT, df$BMXWT)) < 0.001)
    ajouter("Taille superieure au poids (unites inversees ?)", n, "BMXHT vs BMXWT")
  }
  if ("SBP_mean" %in% names(df) && "DBP_mean" %in% names(df)) {
    n <- sum(df$DBP_mean >= df$SBP_mean, na.rm = TRUE)
    ajouter("Tension diastolique sup. ou egale a la systolique", n, "DBP_mean vs SBP_mean")
  }
  if ("RIDAGEYR" %in% names(df)) {
    n <- sum(df$RIDAGEYR < 0 | df$RIDAGEYR > 85, na.rm = TRUE)
    ajouter("Age hors intervalle plausible 0-85", n, "RIDAGEYR")
  }
  if ("BMXBMI" %in% names(df)) {
    n <- sum(df$BMXBMI < 10 | df$BMXBMI > 70, na.rm = TRUE)
    ajouter("IMC hors intervalle plausible 10-70", n, "BMXBMI")
  }
  if (length(problemes) == 0) {
    return(data.frame(Regle = "Aucune incoherence detectee sur les regles actives",
                      N_incoherences = 0, Detail = "", stringsAsFactors = FALSE))
  }
  do.call(rbind, problemes)
}

# ---------------------------------------------------------------------
# 4. Nettoyage
# ---------------------------------------------------------------------

#' Appliquer une strategie de traitement des valeurs manquantes
#'
#' @param df donnees
#' @param strategie "aucune", "supprimer", "imputer_mediane", "imputer_mode"
#' @param vars variables concernees (toutes les colonnes si NULL)
#' @return liste(df, rapport)
traiter_manquants <- function(df, strategie = "aucune", vars = NULL) {
  n_avant <- nrow(df)
  rapport <- paste("Aucune modification appliquee.")
  if (strategie == "supprimer") {
    df2 <- tidyr::drop_na(df, dplyr::any_of(vars %||% names(df)))
    rapport <- sprintf("%d lignes supprimees (completude stricte).", n_avant - nrow(df2))
    return(list(df = df2, rapport = rapport))
  }
  if (strategie %in% c("imputer_mediane", "imputer_mode")) {
    cible <- vars %||% names(df)
    n_imp <- 0
    for (v in cible) {
      if (!any(is.na(df[[v]]))) next
      if (strategie == "imputer_mediane" && is.numeric(df[[v]])) {
        m <- median(df[[v]], na.rm = TRUE)
        n_imp <- n_imp + sum(is.na(df[[v]]))
        df[[v]][is.na(df[[v]])] <- m
      } else if (strategie == "imputer_mode") {
        x <- na.omit(df[[v]])
        if (length(x) == 0) next
        ux <- unique(x); m <- ux[which.max(tabulate(match(x, ux)))]
        n_imp <- n_imp + sum(is.na(df[[v]]))
        df[[v]][is.na(df[[v]])] <- m
      }
    }
    rapport <- sprintf("%d cellules imputees.", n_imp)
    return(list(df = df, rapport = rapport))
  }
  list(df = df, rapport = rapport)
}

#' Supprimer les lignes dupliquees
supprimer_doublons <- function(df) {
  n <- nrow(df)
  df2 <- dplyr::distinct(df)
  list(df = df2, supprimees = n - nrow(df2))
}

#' Normaliser les noms de colonnes (minuscules, sans accents ni espaces)
normaliser_noms <- function(df) {
  noms <- names(df)
  noms <- iconv(noms, from = "UTF-8", to = "ASCII//TRANSLIT")
  noms <- gsub("[^A-Za-z0-9_]+", "_", noms)
  noms <- gsub("^_|_$", "", noms)
  names(df) <- noms
  df
}

# operateur null-coalescing local
`%||%` <- function(a, b) if (is.null(a)) b else a
