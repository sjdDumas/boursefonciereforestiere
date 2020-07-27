#' Paramétrages administrateur
#'
#'
#' @param commune nom de la commune
#' @param parcelle_ini nom de la parcelle à afficher à l'ouverture de la carte (ex: Cheny_00A_0201)
#' @param mail adresse mail de l'administrateur
#' @param host hôte smtp du mail de l'administrateur
#' @param username_smtp compte smtp de l'administrateur
#' @param password_smtp mot de passe compte mail administrateur 
#' @param port_smtp port smtp
#' @param adresse adresse http du site de l'application
#' @param administrateur nom de l'administrateur
#' @param titre_administrateur titre de l'administrateur (ex: 'propriétaire forestier...')
#'
#' @return rien: met à jour le fichier admin.csv
#' @export
#'

admin <- function(commune = "",
                  parcelle_ini = "",
                  mail = "",
                  host = "",
                  username_smtp = "",
                  password_smtp = "",
                  port_smtp = "",
                  adresse = "",
                  administrateur = "",
                  titre_administrateur = ""){
  path <- get_path()
  
  a <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)
  message("paramètres actuels")
  print(a)
  
  ar <- as.list(environment())[-1]
  
  for(i in names(ar)){
    if(ar[[i]] != ""){
      a[[i]] <- ar[[i]]
    }
  }
  
  write.csv(a,file.path(path,"admin.csv"),row.names = FALSE)
  
}


#' Intègre un input à l'objet reactif r
#'
#' @param id nom input 
#' @param r reactive value
#' @param input input
#' @param output output
#' @param session session
#'
#' @return r
#' @export
#'

r_input <- function(id,r,input,output,session) {
  if(is.null(r[[id]])){
    r[[id]] <- input[[id]]
  }else if(r[[id]] != input[[id]]){
    r[[id]] <- input[[id]]
  }
  return(r)
}

#' lecture base de donnée proprietaire
#'
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
read_proprietaire <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  p <- dbReadTable(db,'proprietaire')
  
  p <- p[,!colnames(p) %in% c("couleur_proprietaire","mail","valide","psw")]
  i <- read_identite()
  ia <- ib <- i
  # colnames(ia) <- c("proprietaire","psw","couleur_proprietaire")
  p <- p %>% left_join(ia)
  p$echangeable <- as.logical(p$echangeable)
  p$a_vendre <- as.logical(p$a_vendre)
  dbDisconnect(db)
  return(p)
}

#' lecture base de donnée interet
#'
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
read_interet <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  p <- dbReadTable(db,'interet')
  dbDisconnect(db)
  return(p)
}


#' enregistre base de donnée proprietaire
#'
#' @param pr table
#' @return rien
#' @export
#' 
#' import RSQLite
#' import DBI
#'
write_proprietaire <- function(pr){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  dbWriteTable(db,'proprietaire',pr,overwrite = TRUE,
               field.types=c(parcelle="character",parcelle_groupe="character",
                             proprietaire="character",echangeable="logical",a_vendre="logical",
                             new_proprietaire="character",coeff_valeur="nuneric",couleur_proprietaire="character"))
  dbDisconnect(db)
}

#' enregistre base de donnée interet
#'
#' @param pr table
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
write_interet <- function(pr){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  dbWriteTable(db,'interet',pr,overwrite = TRUE)
  dbDisconnect(db)
}


#' lecture base de donnée identite
#'
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
read_identite <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  p <- dbReadTable(db,'identite')%>%filter(valide==1)
  dbDisconnect(db)
  return(p)
}



#' mise à jour de r$data
#'
#' @param r r
#' @param input input 
#' @param output output
#' @param session session
#' @param ini TRUE pour initialiser r$data
#'
#' @return r
#' @export
#'
#' @import leaflet
#' @import shiny
#' @import dplyr
#' @import  sf htmltools
update_data <- function(r,input,output,session,ini=FALSE){
  
  data <- r$par %>% left_join(read_proprietaire())
  data$proprietaire[is.na(data$proprietaire)] <- "?"
  data$couleur_proprietaire[is.na(data$couleur_proprietaire)] <- "#ffffffff"
  data$echangeable[is.na(data$echangeable)] <- FALSE
  
  isolate(r$data <- data)
  isolate(r$parcelle <- get_parcelle(r,r$parcelle))
  mark <- ico(r)
  
  if(ini){
    
    m <- leaflet()%>%
      addProviderTiles("GeoportailFrance.orthos",group = "ortho",options = tileOptions(minZoom = 12, maxZoom = 20))%>% 
      addProviderTiles("GeoportailFrance.ignMaps",group = "carte",options = tileOptions(minZoom = 12, maxZoom = 20))%>% 
      addTiles(group = "N&B",options = tileOptions(opacity = 0,minZoom = 12, maxZoom = 20)) %>%
      addPolygons(data=r$data,weight = 1,smoothFactor = 0,opacity = 1,
                  label = htmlEscape(r$data$etiquette),
                  fillColor = fillcolor(r), fillOpacity = .5,
                  group = "parcelles",layerId = r$data$parcelle) %>% 
      leaflet::addControl(paste0(
        "<div style='background-color: #ffffff;border-radius:5px;padding:5px;margin:-10px;font-size: 10px;'>",
        "<h4>Légende</h4>",
        "<div><span style='width:50px;height:20px;color:white;border-radius:3px;border:3px solid red; margin-left:5px;margin-right:5px;'>...</span>parcelle sélectionnée</div>",
        
        "<div><span style='width:50px;height:20px;color:white;border-radius:3px;border:1px solid #000; background-color:yellow; margin-left:5px;margin-right:5px;'>....</span><b>Vos parcelles</b></div>",
        "<img style='width:15px;height:15px; margin-left:5px;margin-right:5px;' src='www/echange.png'>proposée à l'échange</img><br>",
        "<img style='width:15px;height:15px; margin-left:5px;margin-right:5px;' src='www/vend.png'>proposée à la vente</img><br>",
        "<b>Les autres propriétés</b><br>",
        "<img style='width:50px;height:50px;margin-bottom:-25px; margin-left:-15px;margin-right:-15px;' src='www/personne.png'>disponible à l'échange</img><br>",
        "<img style='width:50px;height:50px;margin-bottom:-25px; margin-left:-15px;margin-right:-15px;' src='www/interesse.png'>... qui vous intéresse</img><br>",
        "<img style='width:50px;height:50px;margin-bottom:-25px; margin-left:-15px;margin-right:-15px;' src='www/personne_vente.png'>disponible à la vente</img><br>",
        "<img style='width:50px;height:50px;margin-bottom:-25px; margin-left:-15px;margin-right:-15px;' src='www/interesse_achat.png'>... qui vous intéresse</img><br>"
        
      ),
      position="bottomright"
      )
    
    if(!is.null(mark)){
      m <- m %>%  addMarkers(lng = st_coordinates(mark$centro)[,1],
                             lat = st_coordinates(mark$centro)[,2],
                             data=mark$centro,
                             group = "centro",
                             icon = ~icons(
                               paste0("www/",tolower(mark$ico),".png"),
                               iconWidth = mark$centro$size,iconHeight = mark$centro$size
                             )
                             # markerOptions(interactive = FALSE,zIndexOffset = 1000)
      )
    }
    # clearGroup("parcelle_active")%>%
    m <- m %>% addLayersControl(baseGroups = c("carte","ortho","N&B"),overlayGroups = 'parcelles',
                                options = layersControlOptions(collapsed = FALSE)) 
    # hideGroup( c("carte","ortho","N&B")) %>%
    # showGroup(r$map_groups)
    bb <- as.numeric(st_bbox(r$data))
    
    m  %>%
      addPolygons(data=r$par %>% filter(parcelle %in% r$parcelle),
                  color="yellow",fill=FALSE,layerId = "parcelle_active") %>%
      setMaxBounds(bb[1],bb[2],bb[3],bb[4])
    
    r$map <- m
  }else{
    new <- r$data %>% filter(parcelle %in% r$parcelle)
    
    leafletProxy("map",session) %>%
      removeShape(new$parcelle) %>%
      addPolygons(data=r$data,weight = 1,smoothFactor = 0,opacity = 1,
                  label = htmlEscape(r$data$etiquette),
                  fillColor = fillcolor(r), fillOpacity = .5,
                  group = "parcelles",layerId = r$data$parcelle)%>%
      clearGroup("centro")
    
    if(!is.null(mark)){
      leafletProxy("map",session) %>%
        addMarkers(lng = st_coordinates(mark$centro)[,1],
                   lat = st_coordinates(mark$centro)[,2],
                   data=mark$centro,
                   group = "centro",
                   icon = ~icons(
                     paste0("www/",tolower(mark$ico),".png"),
                     iconWidth = mark$centro$size,iconHeight = mark$centro$size
                   )
                   # markerOptions(interactive = FALSE,zIndexOffset = 1000)
        )
    }
  } 
  
  r
}



#' Zoom sur parcelle sélectionnée
#'
#' @param input input
#' @param output output 
#' @param session session
#' @param r r
#'
#' @return coordonnées
#' @export
#'
zoom_parcelle <- function(input,output,session,r){
  parcelle <- paste0(input$select_commune,"_",input$select_section,"_",input$select_parcelle)
  if(parcelle %in% r$data$parcelle){
    
    r$parcelle <- get_parcelle(r,parcelle)
  }
  as.numeric(st_bbox(r$data %>% filter(parcelle %in% r$parcelle)))
}


#' Couleurs de remplissage selon propriétaire
#'
#' @param r r
#'
#' @return vecteur des couleurs des parcelles
#' @export
#'
#' 
fillcolor <- function(r){
  
  if(r$mode=="proprietaire"){
    fc <- rep("#ffffff",nrow(r$data))
    fc[r$data$proprietaire == r$user] <- "yellow"
    
  }else{
    fc <-  r$data$couleur_proprietaire
  }
  fc
}

#' Icones des parcelles
#'
#' @param r r
#'
#' @return liste: centro = sf des centroïdes des parcelles, ico = vecteur des nom des icones
#' @export
#'
#' @import sf
#' @import dplyr
#' 
ico <- function(r){
  
  centro <- r$data%>%filter(echangeable==TRUE)%>% st_transform(2154)%>%
    st_centroid()%>% st_transform(4326)
  if(nrow(centro)==0){
    return(NULL)
  }
  
  int <- read_interet() %>% filter(interet == r$user)
  centro <- centro %>% left_join(int)
  
  centro$size <- 20
  if(r$mode=="proprietaire"){
    centro$size[(centro$proprietaire) != r$user] <- 80  
  }
  if(r$mode=="proprietaire"){
    ico <- centro$echangeable
    ico[centro$proprietaire == r$user] <- "echange" 
    ico[ico == "echange" & as.logical(centro$a_vendre)] <- "vend" 
    ico[!is.na(centro$interet)] <- "interesse"
    ico[ico == "interesse" & as.logical(centro$a_vendre)] <- "interesse_achat"
    ico[ico=="TRUE"] <- "personne"
    ico[ico == "personne" & as.logical(centro$a_vendre)] <- "personne_vente"
  }else{
    ico <- centro$new_proprietaire
  }
  list(centro=centro,ico=ico)
}

#' Montre ou cache les controles selon le contexte
#'
#' @param r r
#' @param input input 
#' @param output output
#' @param session session
#'
#' @return rien
#' @export
#'
#' @import dplyr
#' @import shinyjs
#' 
affiche_controles <- function(r,input,output,session){
  
  if(is.null(r$parcelle)) return(NULL)
  
  p <- r$data%>%
    filter(parcelle %in% r$parcelle)
  print(r$parcelle)
  
  if(r$mode == "proprietaire"){
    # mode proprietaie ...........................................
    hideElement("reunion_proprietaires")
    hideElement("echangeable")
    
    if(p$proprietaire == r$user){
      hideElement("autre_proprio")
      showElement("proprio")
      showElement("is_proprio")
      showElement("est_proprio")
      # showElement("coeff_val")
    }else if(p$proprietaire == "?"){
      hideElement("autre_proprio")
      showElement("proprio")
      showElement("is_proprio")
      hideElement("est_proprio")
      # hideElement("coeff_val")
    }else{ # autre proprietaire
      showElement("autre_proprio")
      hideElement("proprio")
      # showElement("coeff_val")
      # if(p$new_proprietaire == r$user | p$new_proprietaire == "personne"){
      #   showElement("is_interesse")
      #   hideElement("is_prise")
      # }else{
      #   hideElement("is_interesse")
      #   showElement("is_prise")
      # }
    }
  }else{
    # mode réunion .................................
    
    if(p$proprietaire != "?" ){
      showElement("echangeable")
      if(p$echangeable==TRUE){
        showElement("new_proprietaire")
        # showElement("coeff_val")
      }else{
        hideElement("new_proprietaire")
        # hideElement("coeff_val")
      }
    }else{
      hideElement("echangeable")
      hideElement("new_proprietaire")
      # hideElement("coeff_val")
    }
  }
}


#' Table récapitulatives d'un propriétaire
#'
#' @param data issu de r$data
#' @param user issu de r$user
#'
#' @return liste: proprietes = tables des parcelles prorpriétées de user ; 
#' aquisitions = tables des parcelles qui intéressent user;
#' bilan = bilan des échanges
#' @export
#'
#' @import dplyr
bilan_proprietaire <- function(data,user){
  p <- data %>% filter(proprietaire == user)
  
  if(nrow(p)==0){
    showNotification("Vous n'avez encore aucune parcelle disponible",type  = "warning")
  }else{
    
    # parcelles de l'utilisateur
    
    int_par <- read_interet() %>% filter(parcelle %in% p$parcelle)
    int_par <- int_par %>% group_by(parcelle) %>% summarise(intéressé = paste(interet, collapse = ", "))
    
    p <- p %>% left_join(int_par)
    
    p$intéressé[is.na(p$intéressé)] <- "personne"
    
    p$statut <- "à échanger"
    p$statut[p$a_vendre & p$proprietaire == user] <- "à vendre"
    p$statut[!p$echangeable & p$proprietaire == user] <- "à garder"      
    prop <- p %>% as.data.frame() %>%
      mutate(coeff_valeur = surface*coeff_valeur) %>% 
      dplyr::select(statut,nom_com,etiquette,parcelle_groupe,surface,coeff_valeur,intéressé)
    
    prop$parcelle_groupe <- as.factor(prop$parcelle_groupe)
    levels(prop$parcelle_groupe) <- LETTERS[1:length(levels(prop$parcelle_groupe))]
    prop$parcelle_groupe <- as.character(prop$parcelle_groupe)
    prop$parcelle_groupe[is.na(prop$parcelle_groupe)] <- "-"
    
    # parcelles qui l'interessent
    
    p_user <- read_interet() %>% filter(interet == user) %>% pull(parcelle)
    
    p <- read_interet() %>% filter(parcelle %in% p_user) 
    p <- p %>% group_by(parcelle) %>% summarise(intéressé = paste(interet, collapse = ", "))    
    
    p <- p %>% left_join(data)
    p$statut <- rep("",nrow(p))
    p$statut[p$a_vendre] <- "achat"
    
    acq <- p %>% mutate(valeur = surface * coeff_valeur) %>% 
      dplyr::select(statut,nom_com,etiquette,surface,valeur,intéressé)
    
    bil_echange <- c(`surface acquise`=sum(acq$surface[acq$statut=="échange"]),
                     `valeur acquise`=sum(acq$valeur[acq$statut=="échange"]),
                     `surface cédée`=sum(prop$surface[prop$statut=="à échanger" & prop$interessé != "personne"]),
                     `valeur cédée`=sum(prop$valeur[prop$statut=="à échanger" & prop$intéressé != "personne"])
    )
    tab_bilan <- data.frame(cédé = c(bil_echange[3],bil_echange[4]),
                            acquis= c(bil_echange[1],bil_echange[2]),
                            bilan = c(bil_echange[1]-bil_echange[3], 
                                      bil_echange[2]-bil_echange[4])
    )
    row.names(tab_bilan) <- c("surface (ha)","valeur (indice)")
    
    colnames(prop) <- c("statut","commune","parcelle","groupe","surface (ha)","valeur","sont intéressés")
    colnames(acq) <- c("statut","commune","parcelle","surface (ha)","valeur","sont intéressés")
    
    list(proprietes = prop, acquisitions = acq, bilan = tab_bilan)
  }
}



#' Récupère les parcelles du groupe d'une parcelle
#'
#' @param r r
#' @param pc nom de la parcelle
#'
#' @return vecteur des parcelles du même groupe que pc
#' @export
#'
#' @import dplyr
get_parcelle <- function(r,pc){
  p <- r$data %>% filter(parcelle %in% pc)
  gr <- na.omit(unique(p$parcelle_groupe))
  if(length(gr)>0){
    unique(c(p$parcelle,r$data %>% 
               filter(parcelle_groupe %in% gr) %>%
               pull(parcelle)))
    
  }else{
    pc
  }
}

#' Réinitialisation de la table proprietaire
#'
#' @return rien: écrit une sauvegarde de l'ancienne table au format rds, et écrit une nouvelle table vide.
#' @export
#'
init_table_proprio <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  on.exit(dbDisconnect(db))
  
  if("proprietaire" %in% dbListTables(db)){
    saveRDS(p,paste0(file.path(path,"proprietaire_"),Sys.Date(),".rds"))
  }
  
  pr <- data.frame(parcelle="ini",parcelle_groupe=NA,proprietaire="ini",
                   echangeable=FALSE,a_vendre=FALSE,new_proprietaire="personne",coeff_valeur=1,
                   couleur_proprietaire="",stringsAsFactors = FALSE)
  
  write_proprietaire(pr)
}

#' Réinitialisation de la table interet
#'
#' @return rien: écrit une sauvegarde de l'ancienne table au format rds, et écrit une nouvelle table vide.
#' @export
#'
init_table_interet <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  on.exit(dbDisconnect(db))
  
  if("interet" %in% dbListTables(db)){
    saveRDS(p,paste0(file.path(path,"interet_"),Sys.Date(),".rds"))
  }
  df <- data.frame(parcelle="ini",interet="ini")
  
  dbWriteTable(db,'interet',df,overwrite = TRUE)
  
}

#' Réinitialisation de la table identite
#'
#' @return rien: écrit une sauvegarde de l'ancienne table au format rds, et écrit une nouvelle table vide.
#' @export
#'
init_table_identite <- function(){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  on.exit(dbDisconnect(db))
  
  if("identite" %in% dbListTables(db)){
    saveRDS(p,paste0(file.path(path,"identite_"),Sys.Date(),".rds"))
  }
  
  id <- data.frame(proprietaire=c("?"),
                   psw = c("ERtdfg45ttt67"),
                   couleur_proprietaire=c("#ffffffff"),
                   mail = c(""),
                   valide = c(1)
  )
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  dbWriteTable(db,'identite',id,overwrite = TRUE)
  
}

#' Création du fichier sf des parcelles
#'
#' @param path chemin du dossier
#'
#' @return rien : écrit parcelles.rds
#' @export
#'
#' @importFrom stringr str_remove
#' 
setup_parcelles <- function(path){
  p <- st_read(file.path(path,"BDPARCELLE.shp"))
  
  ck <- list()
  for(f in c("nom_com","section","numero")){
    if(f %in% colnames(p)){x <- "présent"}else{x <- "err"}
    ck[[paste0("parcelle_champ_",f)]] <- x
  }
  p$surface <- round(as.numeric(st_area(p))/10000,2)
  
  p$parcelle <- paste0(p$nom_com,"_",p$section,"_",p$numero) 
  p$etiquette <- paste0(str_remove(p$section,"^0{1,}"),str_remove(p$numero,"^0{1,}")) 
  saveRDS(p,file.path(path,"parcelles.rds"))
  
  ck
}


#' Chemin du dossier des données
#'
#' @return chemin
#' @export
#'
get_path <- function(){
  # load(system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
  # sub("\\$HOME",Sys.getenv("HOME"),ini$dir)
  r$dir
}


#' Import des données depuis un dossier externe
#' 
#' Le chemin du dossier externe est indiqué dans le fichier inst/path.txt du package:
#' à défaut, $HOME/boursefonciereforestiere
#'
#' @return rien. Remplace admin.csv et le shapefile BDPARCELLE de data par ceux du dossier externe,
#' et construit parcelles.rds.
#' @export
#'
admin_check <- function(){
  path <- get_path()
  check <- list(admin="err")
  
  lf <- list.files(path)
  
  # check admin
  
  if("admin.csv" %in% lf){
    check$admin <- "présent"
    adm <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)
    arg <- names(as.list(args(admin)))
    arg <- arg[arg != ""]
    for(a in arg){
      if(is.null(adm[[a]])){
        check[[paste0("admin_",a)]] <- "err"
      }else if(adm[[a]]==""){
        check[[paste0("admin_",a)]] <- "err"
      }else{
        check[[paste0("admin_",a)]] <- adm[[a]]
      }
    }
  }
  
  # check BDPARCELLE
  lf_shp <- lf[grep("shp|shx|dbf|prj", lf)]
  ext <- unlist(strsplit(lf_shp,"\\."))
  shapefile_name <- ext[1]
  ext <- ext[!ext == ext[1]]
  ext_tem <- c("dbf","shp","shx","prj")
  for(e in ext_tem){
    if(e %in% ext){x <- "présent"}else{x <- "err"}
    check[[paste0("parcelles_",e)]] <- x
  }
  if(shapefile_name != "BDPARCELLE"){
    file.rename(paste0(path,"/",shapefile_name,".shp"),"BDPARCELLE.shp")
    file.rename(paste0(path,"/",shapefile_name,".shx"),"BDPARCELLE.shx")
    file.rename(paste0(path,"/",shapefile_name,".dbf"),"BDPARCELLE.dbf")
    file.rename(paste0(path,"/",shapefile_name,".prj"),"BDPARCELLE.prj")
  }
  ck <- setup_parcelles(path)
  for(f in names(ck)){check[[f]] <- ck[[f]]}
  
  # check db.sqlite
  
  if("db.sqlite" %in% lf){
    check$db.sqlite <- "présent"
  }else{
    db <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
    on.exit(dbDisconnect(db))
    admin_reset()
    check$db.sqlite <- 'nouvelle'
  }
  
  check
  }

#' Sauvegarde des données
#' 
#' db.sqlite, admin.csv et parcelles.rds sous le dossier indiqué dans path.txt
#'
#' @return copie des fichiers
#' @export
#'
admin_backup <- function(){
  path <- get_path()
  
  new <- file.path(path,paste0("backup_",Sys.time()))
  dir.create(new)
  file.copy(file.path(path,"db.sqlite"),new)
  file.copy(file.path(path,"admin.csv"),new)
  file.copy(file.path(path,"parcelles.rds"),new)
  
  message("Sauvegarde effectuée sous ",new)
}


#' Supprime toutes les données de la base
#'
#' @return rien
#' @export
#'
admin_reset <- function(){
  admin_backup()
    init_table_proprio()
    init_table_interet()
    init_table_identite()
    
    message("reset effectué")
}

#' création objet r fictif pour debugage
#'
#' @return r
#' @export
#'
r_debug <- function(){
  r <- list(user="a")
  r$par = readRDS(file.path(path,"parcelles.rds"))
  data <- r$par %>% left_join(read_proprietaire())
  data$proprietaire[is.na(data$proprietaire)] <- "?"
  data$couleur_proprietaire[is.na(data$couleur_proprietaire)] <- "#ffffffff"
  data$new_proprietaire[is.na(data$new_proprietaire)] <- "personne"
  data$echangeable[is.na(data$echangeable)] <- FALSE
  r$data <- data
  r
}
