#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leafem)
library(sf)
library(dplyr)
library(stars)
library(raster)
library(RSQLite)
library(shinyjs)
library(shinybusy)
library(ggplot2)
library(ggtext)
library(tidyr)
library(htmltools)
library(gridExtra)
library(mailR)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session, mode="reunion") {
  
  # initialisation ----------------------------------------------------------
  
  select <- dplyr::select
  source("fonctions.R")
  r <- reactiveValues()
  r$mode <- "proprietaire"
  
  observeEvent(r,once=TRUE,{
    # authentification --------------------------------------------------------
    
    showModal(
      modalDialog(size="l",
                  title = "Bourse foncière forestière de CHENY",
                  
                  fluidRow(
                    column(6,textInput("login","Nom:",value = "",width = "150px")),
                    column(6,passwordInput("psw", "Mot de passe", value = "",width = "150px")), 
                  ),
                  br(),
                  column(6,actionBttn("enter","Accéder à la bourse",size="xs")),
                  column(6,actionBttn("info_site","Informations",size="xs",color="royal")),
                  br(),br(),
                  h5("Pas encore inscrit ?"),
                  actionBttn("inscription","s'inscrire",size="xs"),
                  
                  hidden(
                    fluidRow(id="form_new_login", style="transform: scale(0.8);border-radius:10px;background-color:#ddddff;margin:-50px;padding:20px",
                             h4("Ouvrir un compte"),
                             h6(style="font-size:12px",HTML(paste(
                               "La bourse d'échange foncière forestière de Cheny est un service collaboratif entre particulier",
                               "Il est fourni gratuitement, sans aucune garantie de résultat. Les démarches d'échange, de vente ou d'achat restent entièrement à la charge des utilisateurs du site.<br>",
                               "En créant un compte, vous acceptez que votre identifiant soit accessibles aux autres utilisateurs du site.",
                               "Ces informations sont enregistrées dans un fichier informatisé par M. Stéphane Dumas afin de permettre les contacts entre utilisateurs. La base légale du traitement est la mission d'intérêt publique.",
                               "Les données collectées seront communiquées aux seuls utilisateurs du site.",
                               "Elles sont conservées pendant une durée de 1 an.",
                               "Vous pouvez accéder aux données vous concernant, les rectifier, demander leur effacement, en ecrivant à l'administrateur du site à l'adresse <a href='mailto:bourse.forestiere.cheny@laposte.net'>bourse.forestiere.cheny@laposte.net</a>",
                               "Consultez le site cnil.fr pour plus d’informations sur vos droits."))),
                             
                             fluidRow(
                               column(12,textInput("new_login","Nom",value=NULL))
                             ),
                             br(),
                             fluidRow(
                               column(6,textInput("new_psw", "Mot de passe", value = NULL)),
                               column(6,textInput("new_psw2", "confirmation mot de passe", value = NULL))
                             ),
                             br(),
                             fluidRow(
                               column(6,textInput("mail", "Adresse mail", value = NULL)),
                               column(6,textInput("mail2", "confirmation adresse mail", value = NULL))
                             ),
                             br(),br(),
                             actionBttn("submit_acces","Demander l'ouverture d'un compte",size="xs")
                    )
                  ),
                  footer = NULL
      )
    )
  })
  
  observeEvent(input$info_site,{
    showModal(
      modalDialog(
        includeHTML("présentation.html"),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
  })
  
  
  observeEvent(input$help,{
    showModal(
      modalDialog(
        includeHTML("présentation.html"),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
  })
  
  observeEvent(input$help,{
    renderUI("présentation.html")
  })
  
  observeEvent(input$enter,{
    id <- read_identite() %>% 
      filter(proprietaire == input$login & psw == input$psw)
    
    if(nrow(id)==0){
      showNotification("Identifiants inconnus",duration = 10,type = "error")
    }else{
      removeModal()
      shinybusy::show_modal_spinner()
      
      if(r$mode=="proprietaire"){
        r$user <- input$login
      }
      
      r$par = readRDS("./data/rdata/parcelles.rds")
      r$parcelle <- "Cheny_0A_0203"
      r <- update_data(r,input,output,session,ini=TRUE)
      
      co <- as.character(unique(r$par$nom_com)[order(unique(r$par$nom_com))])
      updateSelectInput(session,"select_commune",choices = co,selected = "Cheny")  
      
      addResourcePath("img","./img")
      
      pers <- read_identite()$proprietaire
      names(pers) <- paste0("<img src=\'img/",tolower(pers),".png\' width=\'20\' height=\'20\' style=\'background-color:",
                            read_identite()$couleur_proprietaire,";\'> ",pers,"</img>")
      names(pers)[1] <- "?"
      updateRadioGroupButtons(session,"proprietaire",choices =  pers)  
      updateRadioGroupButtons(session,"new_proprietaire",choices = pers)
      affiche_controles(r,input,output,session)
      
      message("event ini OK")
      ate <- TRUE
      # parcelle <- r$data$parcelle[1]
      # r$parcelle <- get_parcelle(r,parcelle)
      
      shinybusy::remove_modal_spinner() 
    }
    
  })
  
  observeEvent(input$inscription,{
    showElement("form_new_login")
  })
  observeEvent(input$submit_acces,{
    if(input$new_login==""){
      showNotification("Vous devez saisir votre nom",type = "error", duration = 10)
    }else if(str_length(input$new_psw)<8){
      showNotification("Le mot de passe doit contenir au moins 8 caractères",type = "error", duration = 10)
      
    }else if(input$new_psw2 != input$new_psw){
      showNotification("La confirmation de votre mot de passe est différente du mot de passe",type = "error", duration = 10)
      
    }else if(! str_detect(input$mail,"@")){
      showNotification("Adresse mail invalide",type = "error", duration = 10)
      
    }else if(input$mail != input$mail2){
      showNotification("Les adresses mail sont différentes",type = "error", duration = 10)
      
    }else if(input$new_login %in% read_identite()$proprietaire){
      showNotification("Ce nom de compte existe déjà. Si vous avez oublié votre mot de passe, merci de contacter par mail sjd.dumas@laposte.net. Sinon, choisissez un autre nom.",type = "error", duration = 10)
      
    }else{
      
      id <- read_identite()
      id <- rbind(id,
                  data.frame(proprietaire=input$new_login,
                             psw=input$new_psw,
                             couleur_proprietaire="",
                             mail=input$mail,
                             valide=1))
      db <- dbConnect(RSQLite::SQLite(), "db.sqlite")
      dbWriteTable(db,'identite',id,overwrite = TRUE)
      dbDisconnect(db)
      
      admin=read.csv("admin.csv",stringsAsFactors = F)
      send.mail(from = admin$mail,
                to = input$mail,
                subject = "bourse foncière forestière de Cheny",
                body = paste("Bonjour,\n Votre compte est ouvert. Vous pouvez y accéder dès maintenant.\n",
                             "Vos identifiants sont:\n   - Nom: ",input$new_login,
                             "\n   - Mot de passe: ",input$new_psw,"\n\n",
                             "Pour toute question, n`hésitez pas à me contacter par retour de mail.\n\n",
                             "Bien cordialement,\nStéphane Dumas, propriétaire forestier sur la commune de Cheny"),
                
                smtp = list(host.name = admin$host, port = admin$port_smtp, 
                            user.name = admin$username_smtp,            
                            passwd = admin$password_smtp, ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      
      showNotification("Inscription enregistrée. Votre compte est ouvert: saisissez votre nom et votre mot de passe...",duration = 20)
      hideElement("form_new_login")
      updateTextInput(session,"login",value="")
      updateTextInput(session,"psw",value="")
      
    }
    
  })
  
  # choix parcelle ----------------------------------------------------------
  
  observeEvent(input$select_commune,{
    req(r$update);
    req(input$select_commune!="")
    message("event input$select_commune ...")
    p <- r$par %>% filter(nom_com == input$select_commune)
    updateSelectInput(session,"select_section",choices = unique(p$section)[order(unique(p$section))] )  
    updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))])  
    
    
  })
  
  observeEvent(input$select_section,{
    req(r$update);
    req(input$select_section!="")
    message("event input$select_section  ...")
    
    p <- r$par %>% filter(nom_com == input$select_commune &
                            section == input$select_section)    
    updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))])  
  })
  
  observeEvent(input$select_parcelle,{
    req(r$update);
    req(input$select_parcelle!="")
    message("event input$select_parcelle ...")
    # 
    # parcelle <- paste0(input$select_commune,"_",input$select_section,"_",input$select_parcelle)
    # if(parcelle %in% r$data$parcelle){
    #   
    #   r$parcelle <- get_parcelle(r,parcelle)
    # }
  })
  
  # carte -------------------------------------------------------------------
  
  
  output$map <- renderLeaflet({
    
    r$map
    
  })
  
  observeEvent(input$map_marker_click,{
    
    pt <- st_as_sf(data.frame(x=input$map_marker_click$lng,y=input$map_marker_click$lat),
                   coords = c("x","y"), crs=4326)
    parcelle <- r$data$parcelle[st_intersects(pt,r$data)[[1]]]
    r$parcelle <- get_parcelle(r,parcelle)
  })
  
  observeEvent(input$map_shape_click,{
    parcelle <- input$map_shape_click$id
    r$parcelle <- get_parcelle(r,parcelle)
  })
  
  observeEvent(input$zoom,{
    parcelle <- paste0(input$select_commune,"_",input$select_section,"_",input$select_parcelle)
    if(parcelle %in% r$data$parcelle){
      
      r$parcelle <- get_parcelle(r,parcelle)
    }
    # bb <- st_bbox(r$data %>% filter(parcelle %in% r$parcelle))
    # 
    # leafletProxy("map",session) %>%
    #   fitBounds(bb[1]-0.01,bb[4]-0.01,bb[3]+0.01,bb[2]+0.01)
  })
  
  output$surface <- renderUI({
    
    req(length(r$parcelle)>0) 
    su <- try(sum(as.numeric(
      r$par %>% filter(parcelle %in% r$parcelle) %>% pull(surface)
    )))
    
    if(class(su) == "try-error") su <- ""
    HTML(paste0(
      
      "Surface: <b>",
      #   "<span style = 'display:inline-table;padding-left:20px;font-size:14px; color: blue'>",
      su,
      " ha</b>"))
    
  })
  
  
  # changement de parcelle --------------------------------------------------
  
  observeEvent(r$parcelle,{
    req(r$parcelle != "__");req(r$data)
    message("event r$parcelle ...")
    
    r$update <- FALSE
    
    # freezeReactiveValue(input, "groupe")
    # maj select inputs
    
    pc <- unlist(str_split(r$parcelle,"_"))
    # updateSelectInput(session,"select_commune", selected = pc[1] )  
    # s <- unique(r$data$section[r$data$nom_com==pc[1]])
    # updateSelectInput(session,"select_section",choices = s[order(s)],selected = pc[2] )
    # n <- unique(r$data$numero[r$data$nom_com==pc[1] & r$data$section==pc[2]])
    # updateSelectInput(session,"select_parcelle",choices = n[order(n)],selected = pc[3])  
    
    # parcelles du groupe de la parcelle cliquée
    # r$parcelle <- get_parcelle(r)
    
    # controles affichés selon caractéristiques de la parcelle
    
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    bb <- as.numeric(st_bbox(p))
    
    
    
    
    leafletProxy("map",session) %>%
      clearGroup("parcelle_active")%>%
      addPolygons(data=p,color="red",fill=FALSE,group = "parcelle_active") %>% 
      fitBounds(bb[1]-0.001,bb[2]-0.001,bb[3]+0.001,bb[4]+0.001)
    
    pro <- r$data %>%
      filter(parcelle %in% r$parcelle)
    
    if(r$mode=="proprietaire"){
      
      if(pro$proprietaire=="?"){
        isolate(updateRadioGroupButtons(session,"is_proprio",selected = "non"))
        isolate(updateNumericInput(session,"coeff_val",value = 1))
        shinyjs::disable("coeff_val")
      }else if(pro$proprietaire != r$user){
        output$info_autre_proprio <- renderUI({
          if(pro$echangeable[1] & pro$a_vendre[1]) x <- "la vendre"
          if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "l'echanger"
          if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "la conserver"
          HTML(paste0(
            "Cette parcelle a été déclarée par un autre propriétaire. Il soutaite ",
            "<a style='color=red'>",x,"</a>"
          ))
          isolate(updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
          shinyjs::disable("coeff_val")
          
        })
        
        if(pro$new_proprietaire %in% c("personne",r$user)){
          # showElement("is_interesse")
          # hideElement("is_prise")
          if(pro$new_proprietaire == r$user){
            ii <- "oui"
          }else{
            ii <- "non"
          }
          isolate(updateRadioGroupButtons(session,"is_interesse",selected = ii))
          isolate(updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
          shinyjs::disable("coeff_val")
          
        }
      }else{
        
        if(pro$echangeable[1] & pro$a_vendre[1]) x <- "vend"
        if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "echange"
        if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "garde"
        showElement("est_proprio")
        
        isolate(updateRadioGroupButtons(session,"is_proprio",selected = 'oui'))
        isolate(updateRadioGroupButtons(session,"choix_proprio",selected = x))
        isolate(updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
        shinyjs::enable("coeff_val")
        
        showElement("proprio")
        hideElement("autre_proprio")
      }
    }else{
      
      # mode réunion
      
      if(nrow(pro)==0){
        isolate(updateRadioGroupButtons(session,"proprietaire",selected = "?"))
        isolate(updateRadioGroupButtons(session,"new_proprietaire",selected = "personne"))
        isolate(updateSwitchInput(session,"echangeable",value = FALSE))
        isolate(updateSwitchInput(session,"groupe",value = FALSE))
      }else{
        if(pro$proprietaire == ""){
          isolate(updateRadioGroupButtons(session,"proprietaire",selected = "?"))
          isolate(updateSwitchInput(session,"echangeable",value = FALSE))
          isolate(updateSwitchInput(session,"groupe",value = FALSE))
        }else{
          isolate(updateRadioGroupButtons(session,"proprietaire",selected = pro$proprietaire[1]))
          isolate(updateSwitchInput(session,"echangeable",value = as.logical(pro$echangeable[1])))
          grouped <- (length(r$parcelle) > 1)
          isolate(updateSwitchInput(session,"groupe",value = grouped))
        }
        if(pro$new_proprietaire == ""){
          isolate(updateRadioGroupButtons(session,"new_proprietaire",selected = "personne"))
        }else{
          isolate(updateRadioGroupButtons(session,"new_proprietaire",selected = pro$new_proprietaire[1]))
        }
      }
      updateNumericInput(session,"coeff_val",value = as.numeric(r$data$coeff_valeur[r$data$parcelle %in% r$parcelle])[1])
    }
    
    affiche_controles (r,input,output,session)
    
    message("event r$parcelle OK")
    r$update <- TRUE
  })
  
  output$commentaires_parcelle <- renderUI({
    req(r$parcelle);req(r$data)
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    
    if(nrow(p)==0){
      HTML("")
    }else if(p$proprietaire == "?"){
      HTML("Personne n'a encore inscrit cette parcelle")
    } else if(p$proprietaire == r$user){
      int <- unique(read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet))
      if(length(int)==0) {
        i <- "Personne n'a déclaré être intéressé"
      }else if(length(int)==1){
        i <- paste(int,"est intéressé")
      }else{
        i <- paste(paste(int,collapse = ", "),"sont intéressés")
      }
      HTML(paste("<a>Vous êtes propriétaire.</a>",i))
    } else {
      if(p$echangeable[1] & p$a_vendre[1]) x <- "la vendre"
      if(p$echangeable[1] & !p$a_vendre[1]) x <- "l'echanger"
      if(!p$echangeable[1] & !p$a_vendre[1]) x <- "la conserver"
      
      # if(p$new_proprietaire != "personne"){
      #   y <- "Quelqu'un s'est déjà déclaré intéressé"
      # }else{
      #   y <- ""
      # }
      HTML(paste0(
        "Cette parcelle a été déclarée par ",p$proprietaire[1],". Il soutaite ",
        "<a style='color=red'>",x,"</a>"
      ))
    }
  })
  
  output$is_prise <- renderUI({
    req(r$parcelle);req(r$data)
    
    int <- unique(read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet))
    
    if(length(int) == 0){
      HTML("Aucun propriétaire encore intéressé")
    }else if(r$user %in% int){
      if(length(int)==1) {
        HTML("Vous êtes le seul propriétaire intéressé")
      }else{
        HTML(paste(paste(int[int != r$user],collapse = ", "),"et vous-même êtes intéressés"))
      }
    }else{
      HTML(paste(length(int)),"propriétaire(s) intéressé(s")
    }
    
    
  })
  # affectation des parcelles mode proprietaire-----------------------------------------------
  
  observeEvent(input$is_proprio,ignoreNULL = TRUE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    message("event input$is_proprio ...")
    pro <- read_proprietaire()
    
    if(input$is_proprio == "oui"){
      shinyjs::enable("coeff_val")
      if(r$parcelle %in% pro$parcelle){
        pro$proprietaire[pro$parcelle %in% r$parcelle] <- r$user
      }else{
        pro <- rbind(pro,
                     c(parcelle=r$parcelle,parcelle_groupe=NA,proprietaire=r$user,
                       echangeable=TRUE,a_vendre=FALSE,
                       new_proprietaire="personne",coeff_valeur=1))
      }
      write_proprietaire(pro)
      r$bounds <- input$map_bounds
      r$map_groups <- input$map_groups
      
      r <- update_data(r,input,output,session,FALSE)
      affiche_controles (r,input,output,session)
    }else{
      int <- read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet)
      if(length(int)>0 & input$is_proprio == "non"){
        
        showNotification("Des propriétaires se sont déclarés intéressés par cette parcelle. Si vous souhaitez vraiment retirer cette parcelle ou modifier son mode de session, merci d'écrire à <a href='mailto:bourse.forestiere.cheny@laposte.net'>bourse.forestiere.cheny@laposte.net</a>",
                         type = "error",duration = 20)
      }else{
        
        pro <- pro %>% filter(! parcelle %in% r$parcelle)
        write_proprietaire(pro)
        r$bounds <- input$map_bounds
        r$map_groups <- input$map_groups
        
        r <- update_data(r,input,output,session,FALSE)
        affiche_controles (r,input,output,session)
        
      }
      
      affiche_controles (r,input,output,session)
    }    
  })
  
  observeEvent(input$choix_proprio,ignoreInit = FALSE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    
    message("event input$choix_proprio ...")
    freezeReactiveValue(input, "groupe")
    
    
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) == r$user)
    
    pro <- pro1 <- read_proprietaire()
    
    if(input$choix_proprio == "vend"){
      pro$echangeable[pro$parcelle %in% r$parcelle] <- TRUE
      pro$a_vendre[pro$parcelle %in% r$parcelle] <- TRUE  
    }
    if(input$choix_proprio == "echange"){
      pro$echangeable[pro$parcelle %in% r$parcelle] <- TRUE
      pro$a_vendre[pro$parcelle %in% r$parcelle] <- FALSE  
    }
    if(input$choix_proprio == "garde"){
      pro$echangeable[pro$parcelle %in% r$parcelle] <- FALSE
      pro$a_vendre[pro$parcelle %in% r$parcelle] <- FALSE
    }
    
    int <- read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet)
    if(length(int)>0 &
       (pro$echangeable[pro$parcelle %in% r$parcelle] != pro1$echangeable[pro$parcelle %in% r$parcelle] |
        pro$a_vendre[pro$parcelle %in% r$parcelle] != pro1$a_vendre[pro$parcelle %in% r$parcelle]
       )){
      
      showNotification("Des propriétaires se sont déclarés intéressés par cette parcelle. Si vous souhaitez vraiment retirer cette parcelle ou modifier son mode de session, merci d'écrire à bourse.forestiere.cheny@laposte.net",
                       type = "error",duration = 20)
    }else{
      write_proprietaire(pro)
      r$bounds <- input$map_bounds
      r <- update_data(r,input,output,session,FALSE)
      
      affiche_controles (r,input,output,session)
    }
  })
  
  observeEvent(input$is_interesse,ignoreNULL = TRUE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    message("event input$is_interesse ...")
    int <- read_interet()
    freezeReactiveValue(input, "groupe")
    
    if(input$is_interesse == "oui"){
      int <- rbind(int,
                   data.frame(parcelle = r$parcelle,
                              interet = rep(r$user,length(r$parcelle)),
                              stringsAsFactors = FALSE))
      
    }else{
      int <- int %>% filter(!(int$parcelle %in% r$parcelle & int$interet == r$user))
      
    }
    write_interet(int)
    r$bounds <- input$map_bounds
    r$map_groups <- input$map_groups
    
    r <- update_data(r,input,output,session,FALSE)
    
    affiche_controles (r,input,output,session)
    
  })
  
  
  # affectation des parcelles mode reunion ------------------------------------------------------------
  
  
  observeEvent(input$proprietaire,ignoreInit = FALSE,handlerExpr = {
    req(r$update);
    req(r$parcelle != "__")
    message("event input$proprietaire ...")
    freezeReactiveValue(input, "groupe")
    
    pro <- read_proprietaire()
    if(r$parcelle %in% pro$parcelle){
      pro$proprietaire[pro$parcelle %in% r$parcelle] <- input$proprietaire
    }else{
      # browser()
      pro <- rbind(pro,
                   data.frame(parcelle=r$parcelle,parcelle_groupe=NA,proprietaire=input$proprietaire,
                              echangeable=TRUE,a_vendre=FALSE,new_proprietaire="personne",coeff_valeur=1,
                              couleur_proprietaire="",stringsAsFactors = FALSE))
    }
    write_proprietaire(pro)
    r$bounds <- input$map_bounds
    r$map_groups <- input$map_groups
    
    r <- update_data(r,input,output,session,FALSE)
    affiche_controles (r,input,output,session)
    
  })  
  
  observeEvent(input$echangeable,ignoreInit = FALSE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    message("event input$echangeable ...")
    freezeReactiveValue(input, "groupe")
    
    
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) != "?")
    
    pro <- read_proprietaire()
    pro$echangeable[pro$parcelle %in% r$parcelle] <- input$echangeable
    write_proprietaire(pro)
    r$bounds <- input$map_bounds
    r <- update_data(r,input,output,session,FALSE)
    affiche_controles (r,input,output,session)
  })
  
  observeEvent(input$new_proprietaire,ignoreInit = TRUE,handlerExpr = {
    req(r$update)
    # freezeReactiveValue(input, "groupe")
    
    message("event input$new_proprietaie ...")
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) != "?")
    
    pro <- read_proprietaire()
    pro$new_proprietaire[pro$parcelle %in% r$parcelle] <- input$new_proprietaire
    write_proprietaire(pro)
    r$bounds <- input$map_bounds
    r <- update_data(r,input,output,session,FALSE)
  })
  
  observeEvent(input$coeff_val,{
    
    invalidateLater(1000, session)
    req(r$update);
    req(r$parcelle)
    message("event input$coeff_val ...")
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) != "?")
    freezeReactiveValue(input, "groupe")
    
    pro <- read_proprietaire()
    pro$coeff_valeur[pro$parcelle %in% r$parcelle] <- as.character(input$coeff_val)
    write_proprietaire(pro)
    r <- update_data(r,input,output,session,FALSE)
    message("event input$coeff_val OK")
  })
  
  # groupes -----------------------------------------------------------------
  
  output$info_groupe <- renderUI({
    gr <- unique(r$data$parcelle_groupe[r$data$parcelle %in% r$parcelle])
    if(length(gr)==0){
      HTML("Aucune parcelle sélectionnée")
    }else{#} if(is.na(gr)){
      
      p <- r$data %>% filter(parcelle %in% r$parcelle)
      pp <- paste(p$nom_com,p$etiquette)
      HTML(
        paste0(
          "<b style='font-size:20px;color:blue'>",
          
          paste(pp,collapse = "<br>"),
          "</b>")
      )
      # }else{
      #   pg <- na.omit(r$data$parcelle[r$data$parcelle_groupe == gr])
      #   HTML(paste0("<b>Parcelle groupée avec les parcelles</b><br><ul><li>",
      #          paste(pg,collapse = "</li><li>"),
      #          "</ul>"
      #          ))
    }
    
  })
  
  observeEvent(input$groupe,ignoreInit = TRUE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    # groupe_parcelles(r,input$groupe)
    # r <- update_data(r,input,output,session,FALSE)
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    
    req(r$user == p$proprietaire)
    
    pp <- read_proprietaire() %>% filter(proprietaire == unique(p$proprietaire))
    ppu <- pp %>% filter(is.na(parcelle_groupe))
    ppg <- pp %>% filter(!is.na(parcelle_groupe))
    
    choices_ppu <- str_replace_all(ppu$parcelle,"_"," ")
    choices_ppu <- choices_ppu[order(choices_ppu)]
    
    choices_ppg <- str_replace_all(ppg$parcelle_groupe,"__"," + ")
    choices_ppg <- str_replace_all(choices_ppg,"_"," ")
    choices_ppg <- choices_ppg[order(choices_ppg)]
    if(length(choices_ppg)==0){
      choices_ppg <- "aucun groupe n'a encore été créé"
    }else{
      choices_ppg <- unique(choices_ppg)
    }
    
    showModal(
      modalDialog(
        title = "Créer un groupe de parcelle",
        h5("Si vous souhaitez céder un ensemble de parcelles conjointes, associez-les dans un groupe."),
        h5("Cette procédure permet d'éviter qu'une partie seulement des parcelles d'un ensemble cohérent soit disloqué suite aux échanges."),
        br(),
        checkboxGroupButtons(
          inputId = "choix_parcelles_groupe",
          label = "sélectionnez les parcelles à grouper",
          choices = choices_ppu,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        ),
        br(),
        actionBttn("creer_groupe","Grouper les parcelles sélectionnées",size="xs"),
        
        h5("Vous pouvez supprimer un groupe existant. Les parcelles seront conservées."),
        br(),
        radioGroupButtons(
          inputId = "choix_rm_groupe",
          label = "Sélectionnez un groupe à supprimer",
          choices = choices_ppg,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        ),
        br(),
        actionBttn("rm_groupe","Dissocier les parcelles du groupe sélectionné",size="xs"),
        
        footer = tagList(
          modalButton("Quitter")
        )
      )
    )
  })
  
  observeEvent(input$creer_groupe,{
    pro <- read_proprietaire()
    pg <-  str_replace_all(input$choix_parcelles_groupe," ","_")
    pro$parcelle_groupe[pro$parcelle %in% pg] <-
      paste(pg,collapse = "__")
    pro$echangeable[pro$parcelle %in% pg] <- TRUE
    pro$new_proprietaire[pro$parcelle %in% pg] <- "personne"
    
    write_proprietaire(pro)
    removeModal()
    showNotification("Groupe créé avec succès")
  })
  
  observeEvent(input$rm_groupe,{
    
    pro <- read_proprietaire()
    pg <-  str_replace_all(input$choix_rm_groupe," \\+ ","__")
    pg <- str_replace_all(pg," ","_")
    
    pro$parcelle_groupe[pro$parcelle_groupe %in% pg] <- NA
    
    write_proprietaire(pro)
    removeModal()
    showNotification("Les parcelles ont été dissociées et sont maintenant disponibles individuellement.")
  })
  
  # bilan -------------------------------------------------------------------
  
  observeEvent(input$rendu_bilan,ignoreInit = T,handlerExpr = ,{
    
    if(r$mode == "reunion"){
      output$bilan_surface <- renderPlot({
        
        message("output$bilan_surface ...")
        
        r$bilan_surface <- bilan_surface(r$data)
      })
    }else{
      b <- bilan_proprietaire(r$data,r$user)
      ide <- read_identite() %>% arrange(proprietaire)
      propri <- ide$proprietaire[-1]
      showModal(
        modalDialog(
          title = "Mes données",
          size = "l", easyClose = TRUE,
          
          h4("Bilan actuel des échanges"),
          tableOutput('bilan_echange'),
          
          # plotOutput("bilan_plot"),
          h4("parcelles dont je suis propriétaire"),
          
          tableOutput("bilan_prop"),
          
          h4("Parcelles pour lesquelles j'ai déclaré être intéressé"),
          
          tableOutput("bilan_acq"),
          
          footer = tagList({
            modalButton("Fermer")
          })
          
        )
      )
      # output$bilan_plot <- renderPlot({
      #   b$gg_bilan
      # })
      
      
      output$bilan_echange <- renderTable({
        b$tab_bilan
      })
      
      output$bilan_prop <- renderTable({
        b$proprietes 
      },spacing = "xs")
      
      output$bilan_acq <- renderTable({
        b$acquisition
      })
    }
  })
  
  observeEvent(input$contact,{
    b <- bilan_proprietaire(r$data,r$user)
    ide <- read_identite() %>% arrange(proprietaire)
    propri <- ide$proprietaire[-1]
    showModal(
      modalDialog(
        title = "Contacter un propriétaire",
        size = "l", easyClose = TRUE,
        
        radioGroupButtons("contacts","Contacter un propriétaire",choices =  propri,selected = NULL),
        br(),
        textAreaInput("text_mail_proprio","Ecrivez ici votre message:",width = "600px"),
        br(),
        actionBttn("send_mail_proprio","Envoyer le mail au propriétaire sélectionné",size="xs"),
        
        footer = tagList(
          modalButton("Fermer")
        )
      )
    )
    
  })
  
  
  observeEvent(input$send_mail_proprio, ignoreInit = TRUE,handlerExpr = {
    admin=read.csv("admin.csv",stringsAsFactors = F)
    dest <- ide$mail[ide$proprietaire == input$contacts]
    send.mail(from = admin$mail,
              to = dest,
              subject = paste("Message de la part de",r$user),
              body = input$text_mail_proprio,
              
              smtp = list(host.name = admin$host, port = admin$port_smtp, 
                          user.name = admin$username_smtp,            
                          passwd = admin$password_smtp, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    
    showNotification(paste("Message envoyé à",dest))
    updateRadioGroupButtons(session,'contacts',selected = NULL)
    
  })
  
  # output$legende <- renderUI({
  #   HTML(paste(
  #     "<div style='position: fixed;top: 200px;background-color: white;right: 0;font-size: 10px;'>",
  #     "<h4>Légende</h4>",
  #     "<div><span style='width:20px;height:20px;border:3px solid red;;'></span>parcelle sélectionnée</div>",
  #     "<b>Vos parcelles</b>",
  #     "<div><span style='width:20px;height:20px;border:1px solid #000; background-color:yellow;'></span>parcelle dont vous êtes propriétaire</div>",
  #      "<img style='width:20px;height:20px;' src='/img/echange.png'>proposée à l'échange</img><br>",
  #     "<img style='width:20px;height:20px;' src='img/vend.png'>proposée à la vente</img><br>",
  #     "<b>Les parcelles des autres propriétaires</b><br>",
  #     "<img style='width:64px;height:64px;margin-bottom:32px;' src='img/personne.png'>disponible à l'échange</img><br>",
  #     "<img style='width:64px;height:64px;margin-bottom:32px;' src='img/interesse.png'>... qui vous intéresse</img><br>",
  #     "<img src='img/personne_vente.png'>disponible à la vente</img><br>",
  #     "<img style='width:64px;height:64px;margin-bottom:32px;' src='img/interesse_achat.png'>... qui vous intéresse</img><br>",
  #     "<img  style='width:64px;height:64px;margin-bottom:32px;'src='img/interesse_achat.png'></img><br>",
  #     "</div>"
  #   ))
  # })  
  # 
})
