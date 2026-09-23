# =====================================================================
# GUIDE DE DEPLOIEMENT
# Public Health Survey Analytics Dashboard (R Shiny)
# Auteur : Joseph ATEBA, Data Scientist
# =====================================================================

Ce guide couvre l'execution locale, la publication sur GitHub et le
deploiement de l'application. Les instructions sont donnees pour Windows,
macOS et Linux.

## 1. Prerequis

L'application necessite R 4.3 ou superieur.

- **R** : telecharger sur https://cran.r-project.org/
- **RStudio** (recommande) : https://posit.co/download/rstudio-desktop/
- **Git** : https://git-scm.com/downloads
- **Docker** (facultatif, pour le deploiement conteneurise) : https://docs.docker.com/get-docker/

Verifier l'installation de R :

```bash
R --version
```

## 2. Installation des dependances

Depuis la racine du projet, executer :

```r
Rscript dependances.R
```

Cette commande installe l'ensemble des paquets necessaires : Shiny,
shinydashboard, DT, ggplot2, plotly, dplyr, survival, survey, car, rmarkdown.

La compilation de certains paquets peut prendre plusieurs minutes lors du
premier lancement.

## 3. Execution locale

### Depuis RStudio

Ouvrir le fichier `app.R`, puis cliquer sur le bouton **Run App**.

### Depuis la ligne de commande

```r
Rscript -e "shiny::runApp('.', port = 3838, launch.browser = TRUE)"
```

L'application s'ouvre sur http://127.0.0.1:3838.

### Options de lancement utiles

```r
# Mode sans interface graphique, accessible sur le reseau local
shiny::runApp(".", host = "0.0.0.0", port = 3838)
```

## 4. Structure du projet

```
public-health-shiny/
├── app.R                    Interface et point d'entree
├── dependances.R            Installation des paquets R
├── rapport_analyse.Rmd      Modele de rapport
├── Dockerfile               Image de production
├── docker-compose.yml       Orchestration locale
├── data/
│   ├── nhanes_survey.csv    Jeu de donnees NHANES 2017-2018
│   └── codebook.csv         Dictionnaire des variables
├── R/
│   ├── server.R             Logique serveur
│   ├── data_cleaning.R      Chargement et controle qualite
│   ├── descriptive_analysis.R  Statistiques et graphiques
│   ├── regression.R         Regression logistique
│   ├── survival.R           Analyse de survie
│   ├── tests_avances.R      Batterie de tests statistiques
│   ├── survey_analysis.R    Plan de sondage complexe
│   └── report_generation.R  Generation du rapport
├── www/
│   └── styles.css           Feuille de style
└── screenshots/             Captures d'ecran
```

## 5. Publication sur GitHub

### 5.1 Creer le depot

1. Se connecter sur https://github.com
2. Cliquer sur **New repository**
3. Nommer le depot : `public-health-survey-dashboard`
4. Choisir **Public** (ideal pour un portfolio)
5. Ne cocher aucune case d'initialisation, le projet est deja pret
6. Cliquer sur **Create repository**

### 5.2 Envoyer le projet

Depuis la racine du projet :

```bash
git init
git add .
git commit -m "Version initiale du tableau de bord d'enquete en sante publique"
git branch -M main
git remote add origin https://github.com/VOTRE_NOM/public-health-survey-dashboard.git
git push -u origin main
```

Remplacer `VOTRE_NOM` par votre nom d'utilisateur GitHub.

### 5.3 Enrichir le depot

- Ajouter une description courte dans les parametres du depot
- Ajouter les themes : `r`, `shiny`, `public-health`, `data-science`, `epidemiology`
- Activer les GitHub Pages si vous souhaitez heberger le rapport HTML
- Ajouter une licence (MIT recommandee pour un projet academique)

## 6. Deploiement avec Docker

### 6.1 Construction de l'image

```bash
docker build -t public-health-shiny .
```

### 6.2 Lancement

```bash
docker run -d -p 3838:3838 --name public_health public-health-shiny
```

L'application est accessible sur http://localhost:3838.

### 6.3 Avec Docker Compose

```bash
docker compose up --build
```

Pour arreter :

```bash
docker compose down
```

## 7. Deploiement sur shinyapps.io

### 7.1 Installer rsconnect

```r
install.packages("rsconnect")
```

### 7.2 Configurer le compte

1. Creer un compte sur https://www.shinyapps.io/
2. Recuperer le jeton depuis **Account > Tokens > Show**
3. Executer dans R :

```r
rsconnect::setAccountInfo(
  name   = "votre_nom",
  token  = "VOTRE_TOKEN",
  secret = "VOTRE_SECRET"
)
```

### 7.3 Publier

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "public-health-survey-dashboard"
)
```

### 7.4 Limitations du plan gratuit

Le plan gratuit autorise 25 heures d'utilisation par mois. Pour une
demonstration ponctuelle, cela suffit largement. Penser a mettre
l'application en veille apres la soutenance.

## 8. Deploiement sur un serveur Shiny (Shiny Server Open Source)

Sur un serveur Linux avec Shiny Server installe :

```bash
sudo cp -r public-health-shiny /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server/public-health-shiny
```

Configurer `/etc/shiny-server/shiny-server.conf` :

```
server {
  listen 3838;
  location /public-health {
    app_dir /srv/shiny-server/public-health-shiny;
    log_dir /var/log/shiny-server;
  }
}
```

Redemarrer le service :

```bash
sudo systemctl restart shiny-server
```

## 9. Verification apres deploiement

Controles a effectuer :

1. La page d'accueil affiche les quatre indicateurs cles
2. L'onglet **Exploration** liste les variables detectees
3. L'onglet **Visualisations** produit un graphique pour une variable
4. L'onglet **Regression logistique** ajuste un modele et affiche la courbe ROC
5. L'onglet **Import et export** telecharge le rapport Markdown
6. L'import d'un CSV personnalise fonctionne

## 10. Resolution des problemes

### Le paquet `fs` ou `httpuv` ne compile pas

Installer les dependances systeme :

```bash
# Debian ou Ubuntu
sudo apt-get install libuv1-dev libcurl4-openssl-dev libssl-dev libxml2-dev

# macOS (avec Homebrew)
brew install libuv openssl
```

### Le port 3838 est deja utilise

```r
shiny::runApp(".", port = 4040)
```

### Le rapport HTML ne se genere pas

Verifier la presence de pandoc :

```r
rmarkdown::pandoc_available()
```

Si la fonction renvoie FALSE, installer pandoc depuis
https://pandoc.org/installing.html

### Les accents ne s'affichent pas correctement

Verifier que les fichiers sont enregistres en UTF-8. Sur RStudio :
**File > Save with Encoding > UTF-8**.

### Les graphiques plotly n'apparaissent pas

Verifier que le paquet `plotly` est installe et a jour :

```r
install.packages("plotly")
```

## 11. Pour aller plus loin

- Integrer les poids de sondage et le plan de sondage complexe avec le
  paquet `survey`
- Ajouter des modeles mixtes avec `lme4`
- Exporter les rapports en Word avec `officer`
- Ajouter une authentification avec `shinymanager`
- Mettre en place un suivi d'usage avec `shinylogs`

---

*Guide redige par Joseph ATEBA, Data Scientist.*
*Contact : atebajoseph047@gmail.com | +237 670 211 522*
