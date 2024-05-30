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

admin <- function(path = get_path(),
                  commune = "",
                  parcelle_ini = "",
                  mail = "",
                  host = "",
                  username_smtp = "",
                  password_smtp = "",
                  port_smtp = "",
                  adresse = "",
                  administrateur = "",
                  titre_administrateur = "",
                  psw_admin = ""){
  
  file <- file.path(path,"admin.csv")
  if(file.exists(file)){
    a <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)  
    message("paramètres actuels")
    print(a)
  }else{
    a <- list()
    message("aucun paramètre n'est enregistré")
  }
  

  
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
  
  p <- p[,!colnames(p) %in% c("mail","valide","psw")]
  i <- read_identite()
  ia <- ib <- i
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
write_proprietaire <- function(r){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  
  pr <- r$data %>% as.data.frame() %>% filter(proprietaire != "?") %>% 
    dplyr::select(parcelle,parcelle_groupe,proprietaire,echangeable,a_vendre,coeff_valeur)
  
  dbWriteTable(db,'proprietaire',pr,overwrite = TRUE,
               field.types=c(parcelle="character",parcelle_groupe="character",
                             proprietaire="character",echangeable="logical",a_vendre="logical",
                             coeff_valeur="nuneric"))
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
  id <- paste(pr$parcelle,pr$interet)
  pr <- pr[which(!duplicated(id)),]
  dbWriteTable(db,'interet',pr,overwrite = TRUE)
  dbDisconnect(db)
}


#' lecture base de donnée identite
#'
#' @param inv si TRUE, retourne aussi les inscriptions non validées
#'
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
read_identite <- function(inv=FALSE){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  p <- dbReadTable(db,'identite')
  if(inv==FALSE)
    p <- p %>% filter(valide==1)
  dbDisconnect(db)
  return(p)
}

#' écrit base de donnée identite
#'
#' @return data frame
#' @export
#' 
#' import RSQLite
#' import DBI
#'
write_identite <- function(id){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  dbWriteTable(db,'identite',id,overwrite = TRUE)
  dbDisconnect(db)
}


#' Initialisation reactiveValues
#'
#' @param r reactivevalues
#'
#' @return r
#' @export
#'
r_ini <- function(r){
  
  r$timer <- 0
  r$mode <- "proprietaire"
  # load(system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
  
  # r$dir <- sub("\\$HOME",Sys.getenv("HOME"),ini$dir)
  r$dir <- get_path()
  # saveRDS(path, "path.rds")
  
  r$admin <- read.csv(file.path(r$dir,"admin.csv"),stringsAsFactors = F)
  
  pth_p <- file.path(r$dir,"parcelles.rds")
  
  if(!file.exists(pth_p))
    setup_parcelles(r$dir)
  
  r$par = readRDS(pth_p) %>% 
    select(parcelle,etiquette,surface,numero,section,nom_com)
  r$parcelle <- r$admin$parcelle_ini #"Cheny_0A_0203"
  
  db <- dbConnect(RSQLite::SQLite(), file.path(r$dir,"db.sqlite"))
  p <- dbReadTable(db,'proprietaire')
  dbDisconnect(db)
  
  data <- r$par %>% left_join(p)
  
  data$proprietaire[is.na(data$proprietaire)] <- "?"
  data$echangeable[is.na(data$echangeable)] <- FALSE
  r$data <- data
  
  r
}

modif_parcelle <- function(r,prop=NULL,ech=NULL,ven=NULL,coef=NULL,grou=NULL){
  if(!is.null(prop))
    r$data$proprietaire[r$data$parcelle %in% r$parcelle] <- prop
  if(!is.null(ech))
    r$data$echangeable[r$data$parcelle %in% r$parcelle] <- ech
  if(!is.null(ven))
    r$data$a_vendre[r$data$parcelle %in% r$parcelle] <- ven
  if(!is.null(coef))
    r$data$coeff_valeur[r$data$parcelle %in% r$parcelle] <- coef
  if(!is.null(grou))
    r$data$parcelle_groupe[r$data$parcelle %in% r$parcelle] <- grou  
  r
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
  
  isolate(r$parcelle <- get_parcelle(r,r$parcelle))
  mark <- ico(r)
  
  if(ini){
    
    m <-
      
      leaflet()%>%
      addWMSTiles("https://data.geopf.fr/wms-r?SERVICE=WMS",
                  layers = "ORTHOIMAGERY.ORTHOPHOTOS.IRC", group = "Infrarouge",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                           version = "1.3.0", maxZoom = 22)) %>%
      addWMSTiles("https://data.geopf.fr/wms-r?SERVICE=WMS",
                  layers = "ORTHOIMAGERY.ORTHOPHOTOS", group = "photo",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                           version = "1.3.0", maxZoom = 22)) %>%
        addWMSTiles("https://data.geopf.fr/wms-r?SERVICE=WMS",
                    layers = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2", group = "carte",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                             version = "1.3.0", maxZoom = 22)) %>%
      addPolygons(data=r$data %>% st_transform(4326),weight = 1,smoothFactor = 0,opacity = 1,
                  label = htmlEscape(r$data$etiquette),
                  fillColor = fillcolor(r$data,r$user), fillOpacity = .5,
                  group = "parcelles",layerId = r$data$parcelle) %>%  
      hideGroup(c("carte","photo")) %>%
      leaflet::addControl(paste0(
        "<div style='background-color: #ffffff;border-radius:5px;padding:5px;margin:-10px;font-size: 10px;'>",
        "<h4>Légende</h4>",
        "<div><span style='width:50px;height:20px;color:white;border-radius:3px;border:3px solid red; margin-left:5px;margin-right:5px;'>...</span>parcelle sélectionnée</div>",
        
        "<div><span style='width:50px;height:20px;color:white;border-radius:3px;border:1px solid #0000ff; background-color:yellow; margin-left:5px;margin-right:5px;'>....</span><b>Vos parcelles</b></div>",
        "<img style='width:15px;height:15px; margin-left:5px;margin-right:5px;' src='www/echange.png'>proposée à l'échange</img><br>",
        "<img style='width:15px;height:15px; margin-left:5px;margin-right:5px;' src='www/vend.png'>proposée à la vente</img><br>",
        "<img style='width:15px;height:15px; margin-left:5px;margin-right:5px;' src='www/preneur.png'>... intéresse quelqu'un</img><br>",
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
    m <- m %>% addLayersControl(baseGroups = c("Infrarouge","carte","photo"),overlayGroups = 'parcelles',
                                options = layersControlOptions(collapsed = FALSE)) 
    # hideGroup( c("carte","ortho","Infrarouge")) %>%
    # showGroup(r$map_groups)
    bb <- as.numeric(st_bbox(r$data))
    
    m  %>%
      addPolygons(data=r$par %>% filter(parcelle %in% r$parcelle)%>% st_transform(4326),
                  color="yellow",fill=FALSE,layerId = "parcelle_active") %>%
      setMaxBounds(bb[1],bb[2],bb[3],bb[4])
    
    r$map <- m
  }else{
    
    new <- r$data %>% filter(parcelle %in% r$parcelle)
    
    leafletProxy("map",session) %>%
      removeShape(new$parcelle) %>%
      addPolygons(data = new %>% st_transform(4326),weight = 1,smoothFactor = 0,opacity = 1,
                  label = htmlEscape(new$etiquette),
                  fillColor = fillcolor(new, r$user), fillOpacity = .5,
                  group = "parcelles",layerId = new$parcelle) %>%
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
fillcolor <- function(data,user){
  
  fc <- rep("#ffffff",nrow(data))
  fc[data$proprietaire == user] <- "yellow"
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
  
  # intérêts de l'utilisateur
  int <- read_interet() %>% filter(interet == r$user)
  centro <- centro %>% left_join(int)
  
  # intéret pour les parcelles de l'utilisateur
  p_pro <- centro %>% filter(proprietaire == r$user) %>% pull(parcelle)
  int <- read_interet() %>% filter(interet != r$user & parcelle %in% p_pro)
  centro$preneur <- FALSE
  centro$preneur[centro$parcelle %in% int$parcelle] <- TRUE
  
  centro$size <- 20
  centro$size[(centro$proprietaire) != r$user] <- 80  
  
  ico <- centro$echangeable
  ico[centro$proprietaire == r$user] <- "echange" 
  ico[ico == "echange" & as.logical(centro$a_vendre)] <- "vend" 
  ico[!is.na(centro$interet)] <- "interesse"
  ico[ico == "interesse" & as.logical(centro$a_vendre)] <- "interesse_achat"
  ico[ico=="1"] <- "personne"
  ico[ico == "personne" & as.logical(centro$a_vendre)] <- "personne_vente"
  
  centro_preneur <- centro %>% filter(preneur == TRUE)
  if(nrow(centro_preneur)>0){
    centro_preneur$size <- 25  
  }
  
  ico_preneur <- rep("preneur",nrow(centro_preneur))
  
  centro <- rbind(centro,centro_preneur)
  ico <- c(ico,ico_preneur)
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
  
  if(all(p$proprietaire == r$user)){
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
    hideElement("proprio")
    if(p$echangeable){
      showElement("autre_proprio")
    }else{
      hideElement("autre_proprio")
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
                   echangeable=FALSE,a_vendre=FALSE,coeff_valeur=1,
                   stringsAsFactors = FALSE)
  
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  
  dbWriteTable(db,'proprietaire',pr,overwrite = TRUE,
               field.types=c(parcelle="character",parcelle_groupe="character",
                             proprietaire="character",echangeable="logical",a_vendre="logical",
                             coeff_valeur="nuneric"))
  message("table proprietaire réinitialisée")
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

#' Création d'une notification pour l'utilisateur
#'
#' @param int table propriétaire avec uniquement les lignes les parcelles qui intéressent le destinataire et dont le propriétaire à modifié la destination.
#'
#' @return renseigne db notification
#' @export
#'
notification <- function(int){
  path <- get_path()
  db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
  on.exit(dbDisconnect(db))
  
  
  for(i in unique(int$interet)){
    
    inti <- int %>% filter(interet == i)
    
    df <- data.frame(
      proprietaire = i,
      message = paste("Le propriétaire de la (des) parcelle(s)",
                      paste(int$parcelle,collapse = ", "), 
                      "pour laquelle vous aviez déclaré votre intérêt a opéré des modifications sur cet (ces) parcelles."
      ),
      affiche=TRUE,
      stringsAsFactors = FALSE
    )
    
    if("notification" %in%  dbListTables(db)){
      df <- rbind(dbReadTable(db,"notification"))
    }
    dbWriteTable(db,"notification",df,overwrite=TRUE)
    
    
    admin <- r$admin
    dest <- read_identite() %>% 
      filter(proprietaire == i) %>% 
      pull(mail)
    envoi <- try(send.mail(from = admin$mail,
                           to = dest,
                           subject = "changement de destination d'une parcelle",
                           body =   paste0("Bonjour, \n",
                                           df$message,
                                           "\nNous vous invitons à consulter ses changements sur le site de la bourse: ",
                                           r$admin$adresse, "\n\n",
                                           "Bien cordialement,\n",r$admin$administrateur,", administrateur de la bourse foncière forestière"
                           ),
                           smtp = list(host.name = admin$host, port = admin$port_smtp, 
                                       user.name = admin$username_smtp,            
                                       passwd = admin$password_smtp, ssl = TRUE),
                           authenticate = TRUE,
                           debug = TRUE,
                           send = TRUE))
    
  }
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
                   mail = c(""),
                   valide = c(1),
                   parcelle=""
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
  p <- st_read(file.path(path,"BDPARCELLE.shp")) %>%
    st_transform(4326)
  
  ck <- list()
  for(f in c("nom_com","section","numero")){
    if(f %in% colnames(p)){x <- "présent"}else{x <- "err"}
    ck[[paste0("parcelle_champ_",f)]] <- x
  }
  p$surface <- round(as.numeric(st_area(p))/10000,2)
  
  p$parcelle <- paste0(p$nom_com,"_",p$section,"_",p$numero) 
  p$etiquette <- paste0(str_remove(p$section,"^0{1,}"),str_remove(p$numero,"^0{1,}")) 
  p <- p[,c("numero", "section", "nom_com","surface","parcelle","etiquette")]
  
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
  # readRDS("path.rds")
  path = "~/.boursefonciereforestiere_data"
  
  if(!dir.exists(path)){
    dir.create(path)
  }

  if(!file.exists(file.path(path, "BDPARCELLE.shp"))){
    rep <- rstudioapi::showQuestion("Shapefile des parcelles cadastrales",
                     "Veuillez sélectionner le fichier .shp des parcelles cadatrales forestières de la commune.",
                     "Sélectionner", "Abandonner")
    if(!rep){stop("Abandon")}
    shp <- file.choose()
    files <- paste0(str_remove(shp, ".shp"),
                    c(".shp", ".dbf", ".shx", ".prj")) %>%
      sort()
    
    if(any(!file.exists(files))){
      stop("shapefile invalide: manque ", 
           paste(files[which(!file.exists(files))], collapse = ", "))
    }
    
    file.copy(sort(files), path)
    
    file.rename(file.path(path, basename(files)),
              file.path(
                path,
                paste0("BDPARCELLE.",
                       c("dbf", "prj", "shp", "shx"))
                )
    )
  }  
  
  if(!file.exists(file.path(path, "parcelles.rds")))
    boursefonciereforestiere::setup_parcelles(path)
  
  if(!file.exists(file.path(path, "admin.csv"))){
    
    rep <- rstudioapi::showQuestion("configuration", 
                                    "Fichier de configuration admin.csv absent.",
                                    "Importer un fichier", "configurer manuellement avec la fonction admnin()")
    if(!rep){stop("Config manuelle: admin(...)")}
    
    csv <- file.choose()
    file.copy(csv, file.path(path, "admin.csv"))

    boursefonciereforestiere::admin_reset()
  }
  
  path
}

#' Définir le chemin du dossier des données
#'
#' @param path chemin
#'
#' @return
#' @export
#'
set_path <- function(path=NULL){
  
  if(is.null(path)){
    path <- system.file("data", package = "boursefonciereforestiere")
  }
  # saveRDS(path,"path.rds")
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
  
  admin <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)
  
  envoi <- try(send.mail(from = admin$mail,
                         to = admin$mail,
                         subject = "backup",
                         body =   paste0("Sauvegarde: ", path,"\n",Sys.time()),
                         attach.files = file.path(new,"db.sqlite"),
                         smtp = list(host.name = admin$host, port = admin$port_smtp, 
                                     user.name = admin$username_smtp,            
                                     passwd = admin$password_smtp, ssl = TRUE),
                         authenticate = TRUE,
                         debug = TRUE,
                         send = TRUE)
  )
  
  
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
  r$dir <- path
  # saveRDS(path, "path.rds")
  
  r$par = readRDS(file.path(r$dir,"parcelles.rds")) %>% 
    select(parcelle,etiquette,surface,numero,section,nom_com)
  data <- r$par %>% left_join(
    read_proprietaire() %>% select(proprietaire,parcelle)
  )
  data$proprietaire[is.na(data$proprietaire)] <- "?"
  data$echangeable[is.na(data$echangeable)] <- FALSE
  r$data <- data
  r
}
