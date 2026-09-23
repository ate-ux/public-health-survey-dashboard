# =====================================================================
# test_modules.R
# Tests unitaires des fonctions d'analyse
# Usage : Rscript tests/test_modules.R
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/data_cleaning.R")
source("R/descriptive_analysis.R")
source("R/regression.R")
source("R/tests_avances.R")
source("R/survey_analysis.R")
source("R/survival.R")
source("R/report_generation.R")

# Petit compteur de tests
resultats <- list()
verifier <- function(nom, condition) {
  statut <- if (isTRUE(condition)) "OK" else "ECHEC"
  resultats[[nom]] <<- statut
  cat(sprintf("[%s] %s\n", statut, nom))
}

cat("\n=== Tests des modules d'analyse ===\n\n")

# ---------------------------------------------------------------------
# Jeu de donnees de test
# ---------------------------------------------------------------------
set.seed(42)
n <- 300
df_test <- data.frame(
  id       = seq_len(n),
  age      = round(rnorm(n, 45, 12)),
  sexe     = sample(c("H", "F"), n, replace = TRUE),
  bmi      = round(rnorm(n, 27, 4), 1),
  age_bin  = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
  groupe   = sample(c("A", "B", "C"), n, replace = TRUE),
  temps    = round(runif(n, 1, 60), 1),
  evenement = sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4)),
  stringsAsFactors = FALSE
)
df_test$age[1:5] <- NA
df_test$bmi[6:10] <- NA
df_test$bmi[11] <- 150  # valeur aberrante

# ---------------------------------------------------------------------
# 1. Nettoyage
# ---------------------------------------------------------------------
cat("-- Nettoyage et diagnostic --\n")

types <- detecter_types(df_test)
verifier("detecter_types identifie l'identifiant", types["id"] == "id")
verifier("detecter_types identifie une variable continue", types["age"] == "continue")
verifier("detecter_types identifie une binaire", types["age_bin"] == "binaire")
verifier("detecter_types identifie une categorielle", types["groupe"] == "categorielle")

q <- resume_qualite(df_test)
verifier("resume_qualite compte les lignes", q$n_lignes == n)
verifier("resume_qualite compte les colonnes", q$n_colonnes == ncol(df_test))
verifier("resume_qualite detecte des manquants", q$cellules_manquantes == 10)
verifier("resume_qualite calcule un taux positif", q$pct_manquant_global > 0)

tc <- tableau_completude(df_test)
verifier("tableau_completude retourne une ligne par variable", nrow(tc) == ncol(df_test))
verifier("tableau_completude est trie par manquants decroissants",
         tc$Pct_manquant[1] >= tc$Pct_manquant[nrow(tc)])

out <- tableau_outliers(df_test, 1.5)
verifier("tableau_outliers detecte la valeur aberrante du BMI",
         !is.null(out) && any(out$Variable == "bmi" & out$N_outliers >= 1))

coh <- controle_coherence(df_test)
verifier("controle_coherence retourne un tableau", is.data.frame(coh))

# ---------------------------------------------------------------------
# 2. Traitement des manquants
# ---------------------------------------------------------------------
cat("\n-- Traitement des valeurs manquantes --\n")

r1 <- traiter_manquants(df_test, "aucune")
verifier("strategie 'aucune' ne modifie pas les donnees", nrow(r1$df) == n)

r2 <- traiter_manquants(df_test, "supprimer", vars = c("age", "bmi"))
verifier("strategie 'supprimer' reduit les lignes", nrow(r2$df) == n - 10)

r3 <- traiter_manquants(df_test, "imputer_mediane", vars = c("age", "bmi"))
verifier("strategie 'imputer_mediane' supprime les NA dans les variables cibles",
         sum(is.na(r3$df$age)) == 0 && sum(is.na(r3$df$bmi)) == 0)

# ---------------------------------------------------------------------
# 3. Statistiques descriptives
# ---------------------------------------------------------------------
cat("\n-- Statistiques descriptives --\n")

s <- stats_continue(df_test$age)
verifier("stats_continue retourne un N correct", s$N == sum(!is.na(df_test$age)))
verifier("stats_continue calcule une moyenne plausible", abs(s$Moyenne - 45) < 10)
verifier("stats_continue respecte l'ordre mediane dans l'intervalle interquartile",
         s$Mediane >= s$Minimum && s$Mediane <= s$Maximum)
verifier("stats_continue produit un IC 95% coherent", s$IC95_inf < s$Moyenne && s$IC95_sup > s$Moyenne)

sd_cat <- stats_categorielle(df_test$sexe)
verifier("stats_categorielle compte les effectifs", sum(sd_cat$Effectif) == n)
verifier("stats_categorielle calcule des pourcentages a 100%",
         abs(sum(sd_cat$Pourcentage) - 100) < 0.1)

dc <- tableau_descriptif_continu(df_test)
verifier("tableau_descriptif_continu exclut les variables discretes",
         !("sexe" %in% dc$Variable))

# ---------------------------------------------------------------------
# 4. Analyse bivariee
# ---------------------------------------------------------------------
cat("\n-- Analyse bivariee --\n")

chi <- test_chi2(df_test, "sexe", "groupe")
verifier("test_chi2 s'execute sur deux discretes", isTRUE(chi$disponible))
verifier("test_chi2 fournit une p-value", chi$p_value >= 0 && chi$p_value <= 1)
verifier("test_chi2 fournit un V de Cramer borne", chi$v_cramer >= 0 && chi$v_cramer <= 1)

tm <- test_moyennes(df_test, "age", "sexe")
verifier("test_moyennes compare deux groupes", isTRUE(tm$disponible))
verifier("test_moyennes identifie le test t", tm$test == "Test t de Student")
verifier("test_moyennes retourne un tableau de groupes", nrow(tm$descriptions) == 2)

tm3 <- test_moyennes(df_test, "age", "groupe")
verifier("test_moyennes bascule en ANOVA pour trois groupes", tm3$test == "ANOVA a un facteur")

tk <- test_posthoc_tukey(df_test, "age", "groupe")
verifier("test_posthoc_tukey produit trois comparaisons", !is.null(tk) && nrow(tk) == 3)

cor_res <- test_correlation_complet(df_test$age, df_test$bmi)
verifier("test_correlation_complet retourne trois methodes", nrow(cor_res) == 3)
verifier("test_correlation_complet borne les coefficients",
         all(abs(cor_res$Coefficient) <= 1))

# ---------------------------------------------------------------------
# 5. Tests avances
# ---------------------------------------------------------------------
cat("\n-- Tests statistiques avances --\n")

tn <- test_normalite(df_test$age)
verifier("test_normalite retourne un resultat", !is.null(tn))
verifier("test_normalite fournit une conclusion", nzchar(tn$Interpretation))

th <- test_homogeneite(df_test, "age", "sexe")
verifier("test_homogeneite retourne au moins un test", !is.null(th) && nrow(th) >= 1)
verifier("test_homogeneite fournit une conclusion par test",
         !is.null(th) && all(nzchar(th$Conclusion)))

t2 <- test_comparaison_deux_groupes(df_test, "age", "sexe")
verifier("test_comparaison_deux_groupes retourne trois tests", nrow(t2) == 3)
verifier("test_comparaison_deux_groupes calcule une taille d'effet",
         grepl("d =", t2$Taille_effet[1]))

t3 <- test_comparaison_k_groupes(df_test, "age", "groupe")
verifier("test_comparaison_k_groupes retourne deux tests", nrow(t3) == 2)
verifier("test_comparaison_k_groupes calcule eta carre",
         grepl("eta2", t3$Taille_effet[1]))

ta <- test_association(df_test, "sexe", "groupe")
verifier("test_association retourne au moins un test", nrow(ta) >= 1)

bt <- batterie_tests(df_test, "age", "sexe")
verifier("batterie_tests detecte le cas continu x discret",
         !is.null(bt$comparaison_2) && is.null(bt$association))

bt2 <- batterie_tests(df_test, "sexe", "groupe")
verifier("batterie_tests detecte le cas discret x discret",
         !is.null(bt2$association))

# ---------------------------------------------------------------------
# 6. Regression logistique
# ---------------------------------------------------------------------
cat("\n-- Regression logistique --\n")

m <- ajuster_logistique(df_test, "age_bin", c("age", "bmi", "sexe"))
verifier("ajuster_logistique retourne un modele", is.null(m$erreur))
verifier("ajuster_logistique compte les observations",
         m$n == sum(complete.cases(df_test[, c("age_bin", "age", "bmi", "sexe")])))

co <- tableau_coefficients(m$modele)
verifier("tableau_coefficients couvre tous les termes", nrow(co) == length(coef(m$modele)))
verifier("tableau_coefficients calcule des odds ratios positifs", all(co$OR > 0))
verifier("tableau_coefficients borne les intervalles autour de l'OR",
         all(co$IC95_inf <= co$OR & co$IC95_sup >= co$OR))

q2 <- pseudo_r2(m$modele)
verifier("pseudo_r2 retourne Nagelkerke dans [0, 1]",
         q2$Nagelkerke >= 0 && q2$Nagelkerke <= 1)
verifier("pseudo_r2 retourne un AIC fini", is.finite(q2$AIC))

lr <- test_rapport_vraisemblance(m$modele)
verifier("test_rapport_vraisemblance fournit une p-value",
         lr$p_value >= 0 && lr$p_value <= 1)

hl <- test_hosmer_lemeshow(m$modele)
verifier("test_hosmer_lemeshow retourne un resultat", !is.null(hl))

cls <- tableau_classification(m$modele, 0.5)
verifier("tableau_classification produit une matrice 2x2", all(dim(cls$matrice) == c(2, 2)))
verifier("tableau_classification borne l'exactitude dans [0, 1]",
         cls$exactitude >= 0 && cls$exactitude <= 1)
verifier("tableau_classification borne la sensibilite dans [0, 1]",
         cls$sensibilite >= 0 && cls$sensibilite <= 1)

roc <- courbe_roc(m$modele)
auc <- attr(roc, "auc")
verifier("courbe_roc produit une AUC dans [0, 1]", auc >= 0 && auc <= 1)
verifier("courbe_roc retourne une courbe ordonnee",
         all(diff(roc$Faux_positifs) >= 0))

interp <- interpreter_coefficients(m$modele)
verifier("interpreter_coefficients produit du texte", nzchar(interp))

vif <- vif_modele(m$modele)
verifier("vif_modele retourne un tableau ou NULL", is.null(vif) || is.data.frame(vif))

pi <- points_influents(m$modele)
verifier("points_influents compte les observations", nrow(pi$donnees) == m$n)

# Cas d'erreur : outcome non binaire
m_err <- ajuster_logistique(df_test, "age", c("bmi"))
verifier("ajuster_logistique refuse une reponse non binaire", !is.null(m_err$erreur))

# Cas d'erreur : aucun predicteur
m_err2 <- ajuster_logistique(df_test, "age_bin", character(0))
verifier("ajuster_logistique refuse l'absence de predicteur", !is.null(m_err2$erreur))

# ---------------------------------------------------------------------
# 7. Analyse de survie
# ---------------------------------------------------------------------
cat("\n-- Analyse de survie --\n")

sd_surv <- preparer_survie(df_test, "temps", "evenement", "sexe")
verifier("preparer_survie structure les donnees", is.null(sd_surv$erreur))
verifier("preparer_survie renomme les colonnes",
         all(c("time", "status") %in% names(sd_surv$donnees)))

fit <- estimer_km(sd_surv$donnees, groupe = TRUE)
verifier("estimer_km produit un objet survfit", inherits(fit, "survfit"))

tkm <- tableau_km(fit)
verifier("tableau_km retourne des lignes", nrow(tkm) > 0)
verifier("tableau_km borne la survie dans [0, 1]",
         all(tkm$Survie >= 0 & tkm$Survie <= 1))

med <- survie_mediane(fit)
verifier("survie_mediane retourne une ligne par groupe", nrow(med) == 2)

lr_surv <- test_logrank(sd_surv$donnees)
verifier("test_logrank fournit une p-value",
         lr_surv$p_value >= 0 && lr_surv$p_value <= 1)

cox <- ajuster_cox(sd_surv$donnees, "group")
verifier("ajuster_cox estime un modele", is.null(cox$erreur))
verifier("ajuster_cox calcule la concordance dans [0, 1]",
         cox$qualite$Concordance >= 0.5 && cox$qualite$Concordance <= 1)

ph <- test_ph_assomption(cox$modele)
verifier("test_ph_assomption retourne un tableau", is.data.frame(ph))

# Cas d'erreur : evenement non binaire
df_err <- df_test; df_err$evenement <- df_err$evenement + 5
sd_err <- preparer_survie(df_err, "temps", "evenement")
verifier("preparer_survie refuse un evenement non binaire", !is.null(sd_err$erreur))

# ---------------------------------------------------------------------
# 8. Generation du rapport
# ---------------------------------------------------------------------
cat("\n-- Generation du rapport --\n")

md <- construire_rapport(df_test, titre = "Test")
verifier("construire_rapport produit du markdown", grepl("^# Test", md))
verifier("construire_rapport inclut la section qualite", grepl("Vue d'ensemble", md))
verifier("construire_rapport inclut les limites", grepl("Limites", md))

# Cas avec regression
md2 <- construire_rapport(df_test, "Test avec regression",
  resultat_regression = list(tableau = co, formule = m$formule,
                             qualite = q2, interpretation = interp))
verifier("construire_rapport integre la regression", grepl("Regression logistique", md2))

# ---------------------------------------------------------------------
# Tests du plan de sondage
# ---------------------------------------------------------------------
cat("\n-- Plan de sondage --\n")

# Jeu de test avec un plan de sondage simule
set.seed(7)
n_plan <- 400
df_plan <- data.frame(
  SEQN = seq_len(n_plan),
  SDMVSTRA = rep(1:10, each = 40),
  SDMVPSU = rep(rep(1:2, each = 20), 10),
  WTMEC2YR = rep(c(100, 300), 200),
  age = round(rnorm(n_plan, 45, 12)),
  sexe = sample(c(1, 2), n_plan, replace = TRUE),
  bmi = round(rnorm(n_plan, 27, 4), 1),
  cas = sample(0:1, n_plan, replace = TRUE, prob = c(0.65, 0.35)),
  stringsAsFactors = FALSE
)

vp <- detecter_vars_plan(df_plan)
verifier("detecter_vars_plan trouve la ponderation", "WTMEC2YR" %in% vp$poids)
verifier("detecter_vars_plan trouve la strate", identical(vp$strate, "SDMVSTRA"))
verifier("detecter_vars_plan trouve l'unite primaire", identical(vp$psu, "SDMVPSU"))

v <- verifier_plan(df_plan)
verifier("verifier_plan valide un plan complet", isTRUE(v$ok))

sans_plan <- df_plan[, setdiff(names(df_plan), c("WTMEC2YR", "SDMVSTRA", "SDMVPSU"))]
verifier("verifier_plan refuse un jeu sans plan", isFALSE(verifier_plan(sans_plan)$ok))

pi <- construire_plan(df_plan)
verifier("construire_plan retourne un objet plan", !is.null(pi$plan))
verifier("construire_plan compte les strates", pi$n_strates == 10)
verifier("construire_plan calcule les degres de liberte", pi$ddl == 10)
verifier("construire_plan somme les poids", abs(pi$poids_total - sum(df_plan$WTMEC2YR)) < 1)
verifier("construire_plan conserve toutes les observations", pi$n == n_plan)

pr <- prevalence_ponderee(pi$plan, "cas")
verifier("prevalence_ponderee retourne une ligne", is.data.frame(pr) && nrow(pr) == 1)
verifier("prevalence_ponderee borne la prevalence", pr$Prevalence_ponderee_pct >= 0 && pr$Prevalence_ponderee_pct <= 100)
verifier("prevalence_ponderee borne l'intervalle",
         pr$IC95_inf_pct <= pr$Prevalence_ponderee_pct &&
           pr$IC95_sup_pct >= pr$Prevalence_ponderee_pct)
verifier("prevalence_ponderee calcule un effectif pondere",
         pr$N_cas_pondere > 0)

tp <- tableau_prevalences_ponderees(df_plan, pi, c("cas"))
verifier("tableau_prevalences_ponderees retourne les indicateurs", nrow(tp) == 1)

mp <- moyenne_ponderee(pi$plan, "bmi")
verifier("moyenne_ponderee retourne une estimation", !is.null(mp) && is.finite(mp$Moyenne_ponderee))
verifier("moyenne_ponderee borne l'intervalle",
         mp$IC95_inf < mp$Moyenne_ponderee && mp$IC95_sup > mp$Moyenne_ponderee)

tm2 <- tableau_moyennes_ponderees(df_plan, pi, c("bmi", "age"))
verifier("tableau_moyennes_ponderees traite les variables continues", nrow(tm2) == 2)

pd <- prevalence_par_domaine(pi$plan, "cas", "sexe")
verifier("prevalence_par_domaine retourne une ligne par modalite",
         is.data.frame(pd) && nrow(pd) == 2)
verifier("prevalence_par_domaine borne les prevalences",
         all(pd$Prevalence_ponderee_pct >= 0 & pd$Prevalence_ponderee_pct <= 100))

cmp <- test_moyennes_ponderees(pi$plan, "bmi", "sexe")
verifier("test_moyennes_ponderees retourne deux groupes",
         !is.null(cmp) && nrow(cmp$tableau) == 2)
verifier("test_moyennes_ponderees fournit une p-value",
         !is.null(cmp) && cmp$p_value >= 0 && cmp$p_value <= 1)

rs <- test_rao_scott(pi$plan, "cas", "sexe")
verifier("test_rao_scott retourne deux tests", !is.null(rs) && nrow(rs) == 2)
verifier("test_rao_scott fournit une statistique finie",
         !is.null(rs) && all(is.finite(rs$Statistique)))

mp_reg <- ajuster_logistique_pondere(pi$plan, "cas", c("age", "sexe", "bmi"))
verifier("ajuster_logistique_pondere estime un modele", is.null(mp_reg$erreur))
verifier("ajuster_logistique_pondere calcule les rapports de cotes",
         !is.null(mp_reg$coefficients) && all(mp_reg$coefficients$OR > 0))
verifier("ajuster_logistique_pondere retourne les degres de liberte",
         mp_reg$degres_liberte == 10)

mp_court <- ajuster_logistique_pondere(pi$plan, "cas", character(0))
verifier("ajuster_logistique_pondere reflechit une erreur sans predicteur",
         !is.null(mp_court$erreur))

ml <- ajuster_lineaire_pondere(pi$plan, "bmi", c("age", "sexe"))
verifier("ajuster_lineaire_pondere estime un modele", is.null(ml$erreur))
verifier("ajuster_lineaire_pondere calcule un R2", is.finite(ml$R2))

ep <- effet_de_plan(df_plan, pi, "cas")
verifier("effet_de_plan retourne une ligne", is.data.frame(ep) && nrow(ep) == 1)
verifier("effet_de_plan calcule un effet positif", ep$Effet_de_plan > 0)

te <- tableau_effets_plan(df_plan, pi, c("cas"))
verifier("tableau_effets_plan retourne les indicateurs", nrow(te) == 1)

gp <- graph_prevalences_comparees(df_plan, pi, c("cas"))
verifier("graph_prevalences_comparees produit un graphique", inherits(gp, "ggplot"))

gd <- graph_prevalence_domaine(pi$plan, "cas", "sexe")
verifier("graph_prevalence_domaine produit un graphique", inherits(gd, "ggplot"))

# ---------------------------------------------------------------------
# Bilan
# ---------------------------------------------------------------------
cat("\n=== Bilan ===\n")
total <- length(resultats)
ok <- sum(unlist(resultats) == "OK")
cat(sprintf("Tests reussis : %d / %d\n", ok, total))
if (ok < total) {
  cat("Tests en echec :\n")
  print(names(resultats)[unlist(resultats) != "OK"])
  quit(status = 1)
} else {
  cat("Tous les tests sont passes.\n")
  quit(status = 0)
}
