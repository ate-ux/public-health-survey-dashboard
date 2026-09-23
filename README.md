# Public Health Survey Analytics Dashboard

Tableau de bord d'analyse d'enquete en sante publique developpe avec R Shiny.
L'application accompagne l'analyste de la donnee brute jusqu'aux resultats
interpretables : controle qualite, statistiques descriptives, analyse
bivariee, regression logistique, tests statistiques avances et analyse de
survie, avec export des resultats et generation automatique de rapport.

**Auteur :** Joseph ATEBA, Data Scientist
**Contact :** atebajoseph047@gmail.com | +237 670 211 522

---

## Apercu

| Module | Contenu |
|---|---|
| Tableau de bord | Indicateurs cles, prevalence des facteurs de risque, filtres rapides |
| Exploration | Structure du jeu de donnees, types detectes, etude d'une variable |
| Visualisations | Histogrammes, boites a moustaches, barres, camemberts, nuages de points, matrices de correlation |
| Statistiques descriptives | Tendance centrale, dispersion, intervalles de confiance, frequences |
| Analyse bivariee | Tableaux croises, chi-deux, V de Cramer, comparaison de moyennes |
| Regression logistique | Rapports de cotes, courbe ROC, calibration, diagnostic complet |
| Tests statistiques | Normalite, homogeneite, chi-deux, Fisher, t-test, Mann-Whitney, ANOVA, Kruskal-Wallis, post-hoc |
| Analyse de survie | Kaplan-Meier, log-rank, modele de Cox, verification des risques proportionnels |
| Plan de sondage | Prevalences et moyennes ponderees, effet de plan, prevalences par domaine, chi-deux de Rao-Scott, regression logistique ponderee |
| Diagnostic des donnees | Completude, valeurs aberrantes, coherence interne, traitement des manquants |
| Tableau filtrable | Filtres par variable qualitative et quantitative, selection des colonnes |
| Import et export | Import CSV, export des donnees, du dictionnaire, du rapport Markdown et HTML |

## Jeu de donnees

L'application est livree avec un sous-echantillon de l'enquete **NHANES
2017-2018** (National Health and Nutrition Examination Survey), publiee par
les Centres for Disease Control and Prevention (CDC) des Etats-Unis et
placee dans le domaine public.

- 3000 adultes de 18 ans et plus
- 65 variables derivees de 16 modules de l'enquete
- Sante cardiovasculaire, metabolique, mentale, comportements de sante,
  donnees sociodemographiques

Le fichier `data/codebook.csv` documente chaque variable : libelle, type et
unite. L'application accepte egalement l'import de tout fichier CSV
personnalise.

## Installation

### Prerequis

- R 4.3 ou superieur
- RStudio (recommande)
- Docker (facultatif, pour le deploiement conteneurise)

### Dependances R

```r
Rscript dependances.R
```

### Lancement

Depuis RStudio, ouvrir `app.R` et cliquer sur **Run App**.

Depuis la ligne de commande :

```r
Rscript -e "shiny::runApp('.', port = 3838)"
```

L'application s'ouvre sur http://127.0.0.1:3838.

## Structure du projet

```
public-health-shiny/
├── app.R                       Interface et point d'entree
├── dependances.R               Installation des paquets R
├── rapport_analyse.Rmd         Modele de rapport R Markdown
├── Dockerfile                  Image de production
├── docker-compose.yml          Orchestration locale
├── GUIDE_DEPLOIEMENT.md        Guide complet de deploiement
├── CHANGELOG.md                Historique des versions
├── data/
│   ├── nhanes_survey.csv       Jeu de donnees NHANES 2017-2018
│   └── codebook.csv            Dictionnaire des variables
├── R/
│   ├── server.R                Logique serveur
│   ├── data_cleaning.R         Chargement, typage, controle qualite
│   ├── descriptive_analysis.R  Statistiques descriptives et graphiques
│   ├── regression.R            Regression logistique et diagnostic
│   ├── survival.R              Analyse de survie
│   ├── tests_avances.R         Batterie de tests statistiques
│   ├── survey_analysis.R       Plan de sondage complexe (poids, strates)
│   └── report_generation.R     Generation du rapport
├── tests/
│   └── test_modules.R          Tests unitaires des fonctions d'analyse
├── www/
│   └── styles.css              Feuille de style
└── screenshots/                Captures d'ecran
```

## Choix techniques

**Organisation modulaire.** `app.R` ne contient que l'interface. La logique
serveur vit dans `R/server.R` et les traitements statistiques dans `R/`. Chaque
fonction d'analyse est independante de Shiny, ce qui la rend testable en
dehors de l'application.

**Detection automatique des types.** La fonction `detecter_types()` classe
chaque variable en identifiant, continu, binaire ou categoriel a partir de sa
classe R, du nombre de valeurs uniques et de leur nature. Les listes
deroulantes de l'application s'adaptent a ce classement : seules les
variables binaires codees 0/1 sont proposees pour la regression logistique.

**Reproductibilite.** Le sous-echantillon NHANES est constitue avec une graine
fixe (`random_state = 42`). Les resultats numeriques du README et du rapport
sont donc reproductibles a l'identique.

**Gestion des cas limites.** Chaque fonction d'analyse teste la presence
d'effectifs suffisants, de variance non nulle et de deux groupes au minimum
avant de lancer un test. Les messages d'erreur sont explicites en francais.

## Statistiques cles du jeu de donnees

| Indicateur | Valeur |
|---|---|
| Observations | 3000 |
| Variables | 65 |
| Age median | 52 ans |
| Part de femmes | 52.1 % |
| IMC moyen | 29.6 kg/m2 |
| Obesite (IMC >= 30) | 40.6 % |
| Hypertension autorapportee | 35.9 % |
| Diabete autorapporte | 15.8 % |
| Fumeurs actuels | 17.4 % |
| Symptomes depressifs (PHQ-9 >= 10) | 8.9 % |
| Sans assurance sante | 15.2 % |
| Valeurs manquantes | 10.97 % |
| Colonnes completes | 22 sur 65 |

### Types de variables detectees

- 24 variables quantitatives
- 14 variables binaires
- 26 variables qualitatives
- 1 identifiant

### Exemple de regression logistique

Modele ajuste sur l'hypertension autorapportee (age, sexe, IMC, tabagisme) :

| Indicateur | Valeur |
|---|---|
| Observations | 2994 |
| Evenements | 1076 |
| AUC (courbe ROC) | 0.793 |
| Pseudo R2 de Nagelkerke | 0.319 |
| Pseudo R2 de McFadden | 0.203 |
| Exactitude au seuil 0.5 | 72.6 % |
| Sensibilite | 55.9 % |
| Specificite | 82.0 % |
| Hosmer-Lemeshow | p = 0.100 |

Rapports de cotes significatifs : age (OR = 1.066 ; IC 95 % 1.060 a 1.072), IMC
(OR = 1.070 ; IC 95 % 1.057 a 1.083) et tabagisme actuel (OR = 1.373 ;
IC 95 % 1.099 a 1.715).

## Resultats ponderes

Le module Plan de sondage corrige les estimations pour tenir compte de
l'echantillonnage. Voici ce que cela change sur ce jeu de donnees.

### Prevalences brutes et ponderees

| Indicateur | Brute (%) | Ponderee (%) | IC 95 % pondere | Effet de plan |
|---|---|---|---|---|
| Hypertension autorapportee | 35.94 | 29.64 | 26.32 a 32.96 | 3.73 |
| Diabete autorapporte | 15.79 | 11.86 | 9.66 a 14.05 | 2.74 |
| Fumeur actuel | 17.40 | 17.30 | 14.14 a 20.46 | 5.43 |

L'ecart est net pour l'hypertension, ou la prevalence brute surestime la
prevalence ponderee de plus de 6 points. L'effet de plan depasse 2.7 sur les
trois indicateurs : l'echantillonnage complexe reduit donc la precision d'un
facteur allant de 2.7 a 5.4 par rapport a un echantillon aleatoire simple.

### Prevalence par sexe

| Sexe | N | Prevalence ponderee (%) | IC 95 % |
|---|---|---|---|
| Hommes | 1436 | 31.10 | 26.41 a 35.78 |
| Femmes | 1564 | 28.29 | 25.53 a 31.05 |

Le test du chi-deux de Rao-Scott ne conclut pas a une association entre le
sexe et l'hypertension autorapportee (chi-deux = 2.818, ddl = 1, p = 0.130 ;
test F de Rao-Scott : F = 2.290, ddl = 1 ; 15, p = 0.151).

### Regression logistique ponderee

Modele ajuste sur l'hypertension autorapportee, avec 15 degres de liberte.
AIC = 2930.75, pseudo R2 = 0.199.

| Terme | OR | IC 95 % | p |
|---|---|---|---|
| Age (par annee) | 1.0670 | 1.0595 a 1.0746 | < 0.0001 |
| Sexe (femme) | 0.7957 | 0.6146 a 1.0302 | 0.111 |
| IMC (par kg/m2) | 1.0728 | 1.0590 a 1.0867 | < 0.0001 |
| Fumeur actuel | 1.4611 | 1.1623 a 1.8366 | 0.008 |

Comparaison avec le modele non pondere, qui donne des intervalles plus
etroits et une estimation legerement differente pour le tabagisme :

| Terme | OR pondere | OR non pondere |
|---|---|---|
| Age | 1.0670 | 1.0664 |
| IMC | 1.0728 | 1.0698 |
| Fumeur actuel | 1.4611 | 1.3725 |

Le sens des associations est stable, mais les intervalles ponderes sont plus
larges et donc plus prudents. C'est ce que recommande la documentation de
NHANES pour toute estimation destinee a la publication.

## Captures d'ecran

Les captures de chaque module de l'application se trouvent dans le dossier
`screenshots/`. Elles ont ete realisees sur l'application en fonctionnement,
avec le jeu de donnees NHANES livre.

| Fichier | Module |
|---|---|
| `00_accueil.png` | Ecran d'accueil |
| `tableau_bord.png` | Tableau de bord |
| `exploration.png` | Exploration des donnees |
| `visualisations.png` | Visualisations interactives |
| `descriptives.png` | Statistiques descriptives |
| `bivariee.png` | Analyse bivariee |
| `regression.png` | Regression logistique |
| `tests.png` | Tests statistiques |
| `survie.png` | Analyse de survie |
| `diagnostic.png` | Diagnostic des donnees |
| `tableau.png` | Tableau filtrable |
| `plan_sondage.png` | Plan de sondage |
| `plan_sondage_resultats.png` | Prevalences ponderees et intervalles de confiance |
| `import_export.png` | Import et export |

## Verification

La suite compte 112 tests unitaires couvrant le nettoyage, la statistique
descriptive, l'analyse bivariee, la regression logistique, les tests avances,
l'analyse de survie, le plan de sondage et la generation du rapport :

```r
Rscript tests/test_modules.R
```

Sur cette version, les 112 tests passent. L'application a par ailleurs ete
lancee et parcourue onglet par onglet, sans erreur de console.

## Deploiement

Trois voies documentees dans `GUIDE_DEPLOIEMENT.md` :

- **Docker** : `docker compose up --build`, application sur le port 3838
- **shinyapps.io** : publication en ligne via `rsconnect`
- **Shiny Server Open Source** : hebergement sur serveur dedie

## Limites

- Les associations observees sont de nature observationnelle et n'etablissent
  pas de causalite.
- Le plan de sondage est pris en compte dans le module dedie, avec les poids,
  les strates et les unites primaires. Les autres modules restent descriptifs
  et non ponderes : leurs chiffres different donc legerement de ceux du module
  Plan de sondage. Pour une publication, citez les estimations ponderees.
- Le plan compte 15 degres de liberte, soit deux unites primaires par strate
  pour 15 strates. Les intervalles s'appuient sur la loi de Student a ce nombre
  de degres, ce qui les rend plus larges qu'un calcul naif. C'est volontaire :
  c'est ce que le plan impose.
- Les tests multiples ne font pas l'objet d'une correction systematique.
- Le module de survie attend une variable de suivi quantitative et une
  variable d'evenement binaire codee 0/1.

## Licence et sources

Application distribuee sous licence MIT.

Donnees : NHANES 2017-2018, National Center for Health Statistics, Centers for
Disease Control and Prevention. Domaine public.
https://wwwn.cdc.gov/nchs/nhanes/

---

*Joseph ATEBA, Data Scientist*
*atebajoseph047@gmail.com | +237 670 211 522*
