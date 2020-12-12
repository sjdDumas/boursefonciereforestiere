
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boursefonciereforestiere

<!-- badges: start -->

<!-- badges: end -->

Mise en relation de propriétaires forestiers en vue d’échanges de
parcelles

## Données nécessaires

Placer dans un dossier vierge (par exemple ‘./ma\_bourse’) le shapefile
des parcelles cadastrales nommé BDPARCELLE:  
\- soit 4 fichiers: BDPARCELLE.shp, BDPARCELLE.shx, BDPARCELLE.dbf,
BDPARCELLE.prj  
\- dont la table attributaire contient a minima les champs:  
\- **nom\_com**: nom de la commune, au format texte  
\- **section**: code de la section, au format texte  
\- **numero**: numéro de la parcelle, au format texte.  
\- ce shapefile peut par exemple être télécharger sous le logiciel QGIS,
après inscription sur le site de l’IGN au géo-service WFS.

## paramétrage

  - Définir le dossier des données:  
    \> boursefonciereforestiere::set\_path(“./ma\_bourse”)

  - Renseigner les paramètres du projet:  
    \> boursefonciereforestiere::admin(commune = “CommunePrincipale”,
    parcelle\_ini = “CommunePrincipale\_OA\_0001”, mail =
    “<administrateur@xxx.com>”, host = “smtp.xxx.com”, username\_smtp
    = “<administrateur@xxx.com>”, password\_smtp = “motDePasse”,
    port\_smtp = “993”, adresse = “<http://ma_bourse.fr:8080>”,
    administrateur = “Monsieur X”, titre\_administrateur = “propriétaire
    forestier”, psw\_admin = "er34hy1Az))
    
    où:
    
      - **commune** est le nom de la principale commune où les parcelles
        seront localisées. Des parcelles de communes avoisinantes
        peuvent être ajoutées dans le shapefile BDPARCELLE,  
      - **parcelle\_ini** est le code (au format
        “commune\_section\_numero”) de la parcelle sélectionnée à
        l’ouverture de l’application,  
      - **mail**, **host**, **username\_smtp**, **password\_smtp**,
        **port\_smtp** sont les paramètres du compte de l’adresse mail
        du site, par laquelle les échanges de messages se feront,  
      - **adresse** est l’adresse http sur laquelle vous hébergez
        l’application, terminée par l’indication du port (par exemple
        8080 si vous utilisez *shinyproxy*),  
      - **administrateur** et **titre\_administrateur** sont les noms et
        titre de l’administrateur du site,  
      - **psw\_admin**: le mot de passe administrateur qui permettra
        l’accès à la gestion des données (validation des comptes,
        sauvegardes…).

## Installation du serveur

La démarche explicitée ici utilise DOCKER
(<https://docs.docker.com/get-docker/>) et SHINYPROXY
(<https://www.shinyproxy.io/>).

### construction de l’image DOCKER

Dans un dossier de votre serveur (par ex. /home/img), placez les
fichiers:

  - **Dockerfile**:
    <https://github.com/sjdDumas/boursefonciereforestiere/blob/master/Dockerfile>  
  - **Rprofile.site**:
    <https://github.com/sjdDumas/boursefonciereforestiere/blob/master/Rprofile.site>  
  - le package source de l’application.

Dans la console, exécutez:

> kill -9 $(sudo lsof -t -i:8080)  
> cd /home/img docker -build -t bourse .

### Démarrage de shinyproxy

Dans un dossier (par exemple /home/shinyproxy), copiez:

  - le fichier java executable shinyproxy: *shinyproxy-2.3.1.jar* (selon
    votre version)  
  - le fichier

cd /home/shinyproxy  
java -jar shinyproxy-2.3.1.jar
