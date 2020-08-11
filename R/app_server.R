#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import stringr dplyr sf
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(profvis::profvis_server, "profiler")
  
  r <- reactiveValues()
  r$timer <- 0
  r$mode <- "proprietaire"
  # load(system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
  
  # r$dir <- sub("\\$HOME",Sys.getenv("HOME"),ini$dir)
  r$dir <- path
  saveRDS(path, "path.rds")
  
  IP <- reactive({ input$getIP })
  
  
  observe({
    cat(capture.output(str(IP()), split=TRUE))
    
  })
  
  autoInvalidate <- reactiveTimer(4000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  
  observeEvent(input$quit,{
    showNotification("Merci de votre visite.")
    invalidateLater(2000,session)
    stopApp()
  })
  
  observeEvent(r,once=TRUE,{
    
    if(! file.exists(file.path(r$dir,"parcelles.rds"))){
      callModule(mod_admin_acces_server,"mod")
      showModal(mod_admin_acces_ui("mod"))
    }else{
      r$admin <- read.csv(file.path(r$dir,"admin.csv"),stringsAsFactors = F)
      r$par = readRDS(file.path(r$dir,"parcelles.rds"))
      r$parcelle <- r$admin$parcelle_ini #"Cheny_0A_0203"
      
      co <- as.character(unique(r$par$nom_com)[order(unique(r$par$nom_com))])
      comi <- strsplit(r$admin$parcelle_ini,"_")[[1]][1]
      p <- r$par %>% filter(nom_com == comi)
      
      updateSelectInput(session,"select_commune",choices = co,selected = comi)  
      updateSelectInput(session,"select_section",choices = unique(p$section)[order(unique(p$section))] ,
                        selected = strsplit(r$admin$parcelle_ini,"_")[[1]][2])  
      updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))],
                        selected = strsplit(r$admin$parcelle_ini,"_")[[1]][3])  
      
      # authentification --------------------------------------------------------
      if(demo){
        r$user <-  "Moi"
        r$dir <- paste0(r$dir,"tmp_",sample.int(10000000000,1))
        saveRDS(r$dir,"path.rds")
        dir.create(file.path(r$dir))
        file.copy(file.path(path,"db.sqlite"),r$dir)
        file.copy(file.path(path,"admin.rds"),r$dir)
        file.copy(file.path(path,"parcelles.rds"),r$dir)
        showNotification("Vous êtes en mode démonstration. Votre nom de propriétaire est 'Moi'",
                         duration = 10, type = "message")
        r <- update_data(r,input,output,session,ini=TRUE)  
      }else{
        
        callModule(mod_accueil_server,"mod",r)
        showModal(mod_accueil_ui("mod",r$admin$commune,r))
      }
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
    req(is.null(r$click))
    print("------------marker click-------------")
    r$click <- 1
    update <- FALSE
    pt <- st_as_sf(data.frame(x=input$map_marker_click$lng,y=input$map_marker_click$lat),
                   coords = c("x","y"), crs=4326)
    parcelle <- r$data$parcelle[st_intersects(pt,r$data)[[1]]]
    r$parcelle <- get_parcelle(r,parcelle)
    r$click <- NULL
    update <- TRUE
  })
  
  observeEvent(input$map_shape_click,{
    req(is.null(r$click))
    print("------------shape click-------------")
    
    
    r$click <- 1
    
    update <- FALSE
    parcelle <- input$map_shape_click$id
    r$parcelle <- get_parcelle(r,parcelle)
    r$click <- NULL
    update <- TRUE
  })
  
  observeEvent(input$zoom,{
    bb <- zoom_parcelle(input,output,session,r)
    
    leafletProxy("map",session) %>%
      fitBounds(bb[1]-0.0003,bb[4]-0.0003,bb[3]+0.0003,bb[2]+0.0003)
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
    
    message("------------------------------------------------------")
    message("parcelle:--------------",r$parcelle,"----------")
    message("------------------------------------------------------")
    
    req(r$parcelle != "__");req(r$data);req(r$user)
    message("event r$parcelle ...")
    
    r$update <- FALSE
    pc <- unlist(str_split(r$parcelle,"_"))
    
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    bb <- as.numeric(st_bbox(p))
    
    leafletProxy("map",session) %>%
      clearGroup("parcelle_active")%>%
      addPolygons(data=p,color="red",fill=FALSE,group = "parcelle_active") %>% 
      fitBounds(bb[1]-0.0003,bb[2]-0.0003,bb[3]+0.0003,bb[4]+0.0003)
    
    pro <- r$data %>%
      filter(parcelle %in% r$parcelle)
    
    if(pro$proprietaire=="?"){
      
      # parcelle sans propriétaire
      
      isolate(updateRadioGroupButtons(session,"is_proprio",selected = "non"))
      (updateNumericInput(session,"coeff_val",value = 1))
      shinyjs::disable("coeff_val")
    }else if(pro$proprietaire != r$user){
      
      # parcelle propriété d'un autre uilisteur
      
      output$info_autre_proprio <- renderUI({
        if(pro$echangeable[1] & pro$a_vendre[1]) x <- "la vendre"
        if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "l'echanger"
        if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "la conserver"
        HTML(paste0(
          "Cette parcelle a été déclarée par un autre propriétaire. Il soutaite ",
          "<a style='color=red'>",x,"</a>"
        ))
        
        updateNumericInput(session,"coeff_val",value =  round(weighted.mean(pro$coeff_valeur, pro$surface),1))
        shinyjs::disable("coeff_val")
        
      })
      
      inter <- read_interet() %>% 
        filter(parcelle %in% r$parcelle & interet == r$user)
      
      if(nrow(inter)>0){
        ii <- "oui"
      }else{
        ii <- "non"
      }
      isolate(updateRadioGroupButtons(session,"is_interesse",selected = ii))
      (updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
      shinyjs::disable("coeff_val")
      
    }else{
      
      # proriété de l'utilisteur
      
      if(pro$echangeable[1] & pro$a_vendre[1]) x <- "vend"
      if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "echange"
      if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "garde"
      showElement("est_proprio")
      
      isolate(updateRadioGroupButtons(session,"is_proprio",selected = 'oui'))
      isolate(updateRadioGroupButtons(session,"choix_proprio",selected = x))
      (updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
      shinyjs::enable("coeff_val")
      
      showElement("proprio")
      hideElement("autre_proprio")
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
    }else if(all(p$proprietaire == "?")){
      HTML("Personne n'a encore inscrit cette parcelle")
    } else if(all(p$proprietaire == r$user)){
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
    
    message("------------------------------------------------------")
    message("est propriétaire:--------------",input$is_proprio,"----------")
    message("------------------------------------------------------")
    
    req(r$update);
    req(r$parcelle)
    r$update <- FALSE
    
    message("event input$is_proprio ...")
    pro <- read_proprietaire()
    
    if(input$is_proprio == "oui"){
      
      # je suis propriétaire ........................................
      
      shinyjs::enable("coeff_val")
      if(r$parcelle %in% pro$parcelle){
        
        # je l'avais déjà déclaré ................................
        
        pro$proprietaire[pro$parcelle %in% r$parcelle] <- r$user
      }else{
        
        # je me déclare propriétaire ................................
        
        pro <- rbind(pro,
                     c(parcelle=r$parcelle,parcelle_groupe=NA,proprietaire=r$user,
                       echangeable=FALSE,a_vendre=FALSE,
                       # new_proprietaire="personne",
                       coeff_valeur=1))
        isolate(updateRadioGroupButtons(session,"choix_proprio",selected = "garde"))
        
      }
      write_proprietaire(pro)
      r$bounds <- input$map_bounds
      r$map_groups <- input$map_groups
      
      r <- update_data(r,input,output,session,FALSE)
      affiche_controles (r,input,output,session)
    }else{
      
      # je ne suis pas propriétaire .................................
      
      int <- read_interet() %>% filter(parcelle %in% r$parcelle)
      if(nrow(int)>0 & input$is_proprio == "non"){
        
        # non, je me suis trompé, mais entre temps, d'autres se sont déclarés intéressés .....................................
        
        notification(int)
        showNotification(paste0("Des propriétaires se sont déclarés intéressés par cette parcelle. Ils recevront une notification les informant du changement."),
                         type = "warning",duration = 20)
        
        int <- read_interet() %>% 
          filter(!parcelle %in% r$parcelle)
        write_interet(int)
      }
      
      # non, je me suis trompé, ùais c'est sans conséquence .....................................
      
      pro <- pro %>% filter(! parcelle %in% r$parcelle)
      write_proprietaire(pro)
      r$bounds <- input$map_bounds
      r$map_groups <- input$map_groups
      
      r <- update_data(r,input,output,session,FALSE)
      affiche_controles (r,input,output,session)
      
    }    
    r$update <- TRUE
  })
  
  observeEvent(input$choix_proprio,ignoreInit = FALSE,handlerExpr = {
    
    req(r$update);
    req(r$parcelle)
    r$update <- FALSE
    message("------------------------------------------------------")
    message("choix propriétaire:--------------",input$choix_proprio,"----------")
    message("------------------------------------------------------")
    
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
    
    int <- read_interet() %>% filter(parcelle %in% r$parcelle)
    if(nrow(int)>0 &
       (pro$echangeable[pro$parcelle %in% r$parcelle] != pro1$echangeable[pro$parcelle %in% r$parcelle] |
        pro$a_vendre[pro$parcelle %in% r$parcelle] != pro1$a_vendre[pro$parcelle %in% r$parcelle]
       )){
      notification(int)      
      showNotification(paste0("Des propriétaires se sont déclarés intéressés par cette parcelle. Ils recevront une notification les informant du changement."),
                       type = "warning")
      
      int <- read_interet() %>% 
        filter(!parcelle %in% r$parcelle)
      write_interet(int)
    }
    write_proprietaire(pro)
    r$bounds <- input$map_bounds
    r <- update_data(r,input,output,session,FALSE)
    
    affiche_controles (r,input,output,session)
    r$update <- TRUE
    
  })
  
  observeEvent(input$is_interesse,ignoreNULL = TRUE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    
    r$update <- FALSE
    message("------------------------------------------------------")
    message("est interesse:--------------",input$is_interesse,"----------")
    message("------------------------------------------------------")
    message("event input$is_interesse ...")
    int <- read_interet()
    # freezeReactiveValue(input, "groupe")
    
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
    r$update <- TRUE
    
  })
  
  observeEvent(input$coeff_val,{
    
    req(r$update);
    req(r$parcelle)
    # invalidateLater(100)
    message("------------------------------------------------------")
    message("coeff valeur:--------------",input$coeff_val,"----------")
    message("------------------------------------------------------")
    
    message("event input$coeff_val ...")
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) != "?")
    freezeReactiveValue(input, "groupe")
    
    pro <- read_proprietaire()
    pro$coeff_valeur[pro$parcelle %in% r$parcelle] <- input$coeff_val
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
    }
  })
  
  
  observeEvent(input$groupe,ignoreInit = TRUE,handlerExpr = {
    # req(r$update);
    # req(r$parcelle)
    # groupe_parcelles(r,input$groupe)
    # r <- update_data(r,input,output,session,FALSE)
    # p <- r$data %>% filter(parcelle %in% r$parcelle)
    
    # req(r$user == p$proprietaire)
    
    message("------------------------------------------------------")
    message("-------------------acces groupe------------------------")
    message("------------------------------------------------------")
    
    pp <- read_proprietaire() %>% filter(proprietaire == unique(r$user))
    ppu <- pp %>% filter(is.na(parcelle_groupe))
    ppg <- pp %>% filter(!is.na(parcelle_groupe))
    
    r$choices_ppu <- str_replace_all(ppu$parcelle,"_"," ")
    r$choices_ppu <- r$choices_ppu[order(r$choices_ppu)]
    
    r$choices_ppg <- str_replace_all(ppg$parcelle_groupe,"__"," + ")
    r$choices_ppg <- str_replace_all(r$choices_ppg,"_"," ")
    r$choices_ppg <- r$choices_ppg[order(r$choices_ppg)]
    if(length(r$choices_ppg)==0){
      r$choices_ppg <- "aucun groupe n'a encore été créé"
    }else{
      r$choices_ppg <- unique(r$choices_ppg)
    }
    
    callModule(mod_groupe_server, "mod",r)
    showModal(
      mod_groupe_ui("mod",r)
    )
  })
  
  # bilan -------------------------------------------------------------------
  
  observeEvent(input$rendu_bilan,ignoreInit = T,handlerExpr = ,{
    
    message("------------------------------------------------------")
    message("-------------------acces bilan------------------------")
    message("------------------------------------------------------")
    r$b <- bilan_proprietaire(r$data,r$user)
    ide <- read_identite() %>% arrange(proprietaire)
    propri <- ide$proprietaire[-1]
    
    callModule(mod_bilan_proprio_server, "mod",r)
    showModal(
      mod_bilan_proprio_ui("mod")
    )
  })
  
  
  # contact -----------------------------------------------------------------
  
  observeEvent(input$contact,{
    
    
    callModule(mod_contact_proprio_server, "mod",r)
    showModal(
      mod_contact_proprio_ui("mod")
    )
    
  })
  
  
  # aide --------------------------------------------------------------------
  
  observeEvent(input$help,{
    showModal(
      modalDialog(
        includeMarkdown(file.path(system.file(package = "boursefonciereforestiere"),"aide","manuel_utilisateur.md")),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
  })
  
  
  # output$keepAlive <- renderText({
  #   invalidateLater(4000)
  #   r$timer <- r$timer + 1
  #   r$timer
  # })   
}
