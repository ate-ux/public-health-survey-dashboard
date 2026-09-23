# Journal des modifications

Toutes les evolutions notables du Public Health Survey Analytics Dashboard
sont consignees dans ce fichier. Le format suit les conventions de
Keep a Changelog et le versionnage semantique.

## [1.1.0] - 2026-09-21

### Ajoute

- **Module Plan de sondage** (`R/survey_analysis.R`) : prise en compte du plan
  de sondage complexe de NHANES, avec les poids d'examen, les strates et les
  unites primaires.
  - Construction du plan par `survey::svydesign`, avec detection automatique
    des variables de ponderation, de strate et d'unite primaire.
  - Prevalences ponderees avec erreurs standards et intervalles de confiance.
  - Moyennes et medianes ponderees.
  - Prevalences par domaine, avec variance correcte sur sous-population.
  - Effet de plan (design effect) : comparaison des intervalles bruts et
    ponderes.
  - Test du chi-deux de Rao-Scott, qui corrige la statistique usuelle par
    l'effet de plan, et test F de Rao-Scott.
  - Regression logistique ponderee par quasi-vraisemblance, avec rapports de
    cotes et pseudo R2 fonde sur la deviance.
  - Regression lineaire ponderee.
  - Deux graphiques : comparaison des prevalences brutes et ponderees, et
    prevalences par domaine avec intervalles de confiance.

- Nouvel onglet **Plan de sondage** dans l'application, avec un encadre
  methodologique expliquant les trois elements du plan et leur effet.

- 35 tests unitaires supplementaires couvrant le module de plan de sondage,
  ce qui porte la suite a 112 tests.

### Corrige

- La fonction serveur est desormais rattachee a l'environnement de
  l'application, ce qui corrige un acces defaillant aux fonctions des modules.
- Les selecteurs de l'analyse bivariee excluent l'identifiant et les variables
  de plan de sondage, et proposent des variables d'interet par defaut.
- Extraction explicite du critere AIC du modele pondere, `AIC()` renvoyant
  plusieurs criteres pour un objet de plan de sondage.
- Formatage des effectifs ponderes, avec separation des milliers lisible.

## [1.0.0] - 2026-09-20

### Ajoute

- **Tableau de bord** : quatre indicateurs cles, prevalence des facteurs de
  risque avec intervalles de confiance, profil de l'echantillon filtre,
  filtres rapides par age, sexe, categorie d'IMC et origine ethnique.
- **Exploration des donnees** : structure du jeu de donnees avec dictionnaire
  de variables, detection automatique des types, etude detaillee d'une
  variable choisie.
- **Visualisations interactives** : histogrammes avec densite, boites a
  moustaches, diagrammes en barres, camemberts, nuages de points avec droite
  de tendance, barres croisees, moyennes avec intervalles de confiance,
  matrices de correlation, carte des valeurs manquantes. Rendu plotly.
- **Statistiques descriptives** : tendance centrale, dispersion, asymetrie,
  aplatissement, coefficient de variation, intervalles de confiance a 95%
  pour les variables quantitatives, frequences et pourcentages cumules pour
  les variables qualitatives, synthese automatique.
- **Analyse bivariee** : tableaux croises, test du chi-deux avec V de Cramer
  corrige, comparaison de moyennes par test t ou ANOVA, post-hoc Tukey HSD,
  correlations de Pearson avec intervalles de confiance.
- **Regression logistique** : estimation par maximum de vraisemblance,
  rapports de cotes avec intervalles de confiance, seuil de classification
  ajustable, selection pas a pas selon l'AIC, courbe ROC avec aire sous la
  courbe, test de Hosmer-Lemeshow, tests du rapport de vraisemblance et de
  Wald, pseudo R2 de McFadden, Cox-Snell et Nagelkerke, VIF pour la
  multicolinearite, distance de Cook pour les points influents.
- **Tests statistiques avances** : Shapiro-Wilk, Kolmogorov-Smirnov,
  Levene, Bartlett, chi-deux, Fisher exact, rapport de vraisemblance,
  t de Student, t de Welch, Mann-Whitney, ANOVA, Kruskal-Wallis, Tukey HSD,
  correlations de Pearson, Spearman et Kendall. Tailles d'effet calculees
  automatiquement (d de Cohen, eta carre, r de rang-biseriale).
- **Analyse de survie** : estimation Kaplan-Meier, survie mediane par groupe,
  test du log-rank, modele de Cox a risques proportionnels avec ratios de
  risque, verification de l'hypothese des risques proportionnels par les
  residus de Schoenfeld.
- **Diagnostic des donnees** : resume de qualite, completude par variable,
  detection des valeurs aberrantes par la regle de Tukey, valeurs extremes,
  controle de coherence interne, detection des variables a variance quasi
  nulle, traitement des valeurs manquantes par suppression ou imputation.
- **Tableau filtrable** : filtres croises par variables qualitatives et
  quantitatives, selection des colonnes affichees, recherche et tri.
- **Import et export** : import de fichiers CSV personnalises, export des
  donnees filtrees, du dictionnaire des variables, des statistiques
  descriptives, et generation d'un rapport d'analyse en Markdown ou HTML.

### Technique

- Architecture modulaire : interface dans `app.R`, logique serveur dans
  `R/server.R`, traitements statistiques dans `R/`.
- Fonctions d'analyse independantes de Shiny, donc testables isolément.
- Suite de 77 tests unitaires couvrant le nettoyage, la statistique
  descriptive, la regression, les tests avances, la survie et la generation
  de rapport.
- Jeu de donnees de demonstration : sous-echantillon NHANES 2017-2018
  (3000 adultes, 65 variables), reproductible par graine fixe.
- Deploiement documente : Docker, Docker Compose, shinyapps.io et
  Shiny Server Open Source.

### Donnees

- Source : NHANES 2017-2018, National Center for Health Statistics,
  Centers for Disease Control and Prevention. Domaine public.
- 16 modules de l'enquete fusionnes par identifiant de participant.
- Recodage des valeurs sentinelles (7, 9, 77, 99) en valeurs manquantes.
- Variables derivees : score PHQ-9 total, tension arterielle moyenne,
  categories d'IMC, groupes d'age, indicateurs binaires d'hypertension,
  de diabete et de tabagisme.
