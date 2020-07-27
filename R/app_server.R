#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import stringr dplyr sf
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  r <- reactiveValues()
  r$mode <- "proprietaire"
  
  observeEvent(r,once=TRUE,{
    # authentification --------------------------------------------------------
    callModule(mod_accueil_server,"mod",r)
    showModal(mod_accueil_ui("mod"))
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
 
    pc <- unlist(str_split(r$parcelle,"_"))
    
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
    
    callModule(mod_groupe_server, "mod",r)
    showModal(
      mod_groupe_ui("mod")
    )
  })
  
  # bilan -------------------------------------------------------------------
  
  observeEvent(input$rendu_bilan,ignoreInit = T,handlerExpr = ,{
    
    b <- bilan_proprietaire(r$data,r$user)
    ide <- read_identite() %>% arrange(proprietaire)
    propri <- ide$proprietaire[-1]
    
    callModule(mod_bilan_proprio_server, "mod",r)
    showModal(
      mod_bilan_proprio_ui("mod")
    )
  })
  

# contact -----------------------------------------------------------------

  observeEvent(input$contact,{
    b <- bilan_proprietaire(r$data,r$user)
    ide <- read_identite() %>% arrange(proprietaire)
    propri <- ide$proprietaire[-1]
    
    callModule(mod_bilan_proprio_server, "mod")
    showModal(
      mod_contact_proprio_ui("mod")
    )
    
  })


# aide --------------------------------------------------------------------

  observeEvent(input$help,{
    showModal(
      modalDialog(
        includeHTML("presentation.html"),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
  })
  
}
