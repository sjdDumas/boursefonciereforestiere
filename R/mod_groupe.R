#' groupe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_groupe_ui <- function(id,r){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = "Créer un groupe de parcelle",
      h5("Si vous souhaitez céder un ensemble de parcelles conjointes, associez-les dans un groupe."),
      h5("Cette procédure permet d'éviter qu'une partie seulement des parcelles d'un ensemble cohérent soit disloqué suite aux échanges."),
      br(),
      checkboxGroupButtons(
        inputId = ns("choix_parcelles_groupe"),
        label = "sélectionnez les parcelles à grouper",
        choices = unique(r$choices_ppu),
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"))
      ),
      br(),
      actionBttn(ns("creer_groupe"),"Grouper les parcelles sélectionnées",size="xs"),
      
      h5("Vous pouvez supprimer un groupe existant. Les parcelles seront conservées."),
      br(),
      radioGroupButtons(
        inputId = ns("choix_rm_groupe"),
        label = "Sélectionnez un groupe à supprimer",
        choices = unique(r$choices_ppg),
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"))
      ),
      br(),
      actionBttn(ns("rm_groupe"),"Dissocier les parcelles du groupe sélectionné",size="xs"),
      
      footer = tagList(
        modalButton("Quitter")
      )
    )
    
  )
}

#' groupe Server Function
#'
#' @noRd 
mod_groupe_server <- function(input, output, session,r){
  ns <- session$ns
  
  # créer un groupe ---------------------------------------------------------
  
  observeEvent(input$creer_groupe,ignoreInit = TRUE, handlerExpr = {
    r$update <- FALSE
    
    pro <- read_proprietaire() %>%
      left_join(read_interet())
    
    pg <-  str_replace_all(input$choix_parcelles_groupe," ","_")
    pro$parcelle_groupe[pro$parcelle %in% pg] <-
      paste(pg,collapse = "__")
    
    ech <- pro$echangeable[pro$parcelle %in% pg]
    ven <- pro$a_vendre[pro$parcelle %in% pg]
    int <- pro %>% filter(parcelle %in% pg)
    
    if(length(unique(ech))>1 | length(unique(ven))>1){
      showNotification("Impossible de grouper des parcelles de destinations dfférentes. Modifiez d'abord la destnation de ces parcelles afin qu'eels soient toutes à échanger, à vendre ou à garder ")
    }else{

      if(length(unique(int$interet))>1){
        r$tmp <- list(
          pro=pro,pg=pg,ech=ech,ven=ven,int=int
        )
        showModal(
          modalDialog(
            title = "Ces parcelles n'intéressent pas les mêmes personnes. Si vous les groupez, les déclatations d'intérêt seront supprimées et les intéressés en seront informés",
            footer = tagList(
              actionButton(ns("ok_multi_inter"),"Grouper quand même"),
              modalButton("Ne pas grouper")
            )
          )
        )
      }else{
        mod_groupe_grouper(pro,pg,ech,ven,FALSE,input,output,session)
      }
    }
    
  })
  
  mod_groupe_grouper <- function(pro,pg,ech,ven,rm_int,input,output,session){    
    
    pro$echangeable[pro$parcelle %in% pg] <- unique(ech)
    pro$a_vendre[pro$parcelle %in% pg] <- unique(ven)
    
    write_proprietaire(pro)
    r <- update_data(r,input,output,session,FALSE)
    
    if(rm_int){
      bdint <- read_interet()
    bdint <- bdint %>% filter(! parcelle %in% pg)
    write_interet(bdint)
    }
    removeModal()
    showNotification("Groupe créé avec succès")
    r$update <- TRUE
  
  }
  
  observeEvent(input$ok_multi_inter,{
    
    mod_groupe_grouper(r$tmp$pro,r$tmp$pg,r$tmp$ech,r$tmp$ven,TRUE,input,output,session)
    probs <- na.omit(unique(r$tmp$int$interet))
    for(n in props){
      notification(r$tmp$int %>% filter(interet == n))
    }
  
  })
  
  # supprimer un groupe -----------------------------------------------------
  
  observeEvent(input$rm_groupe,ignoreInit = TRUE,handlerExpr = {
    r$update <- FALSE
    
    pro <- read_proprietaire()
    pg <-  str_replace_all(input$choix_rm_groupe," \\+ ","__")
    pg <- str_replace_all(pg," ","_")
    
    pro$parcelle_groupe[pro$parcelle_groupe %in% pg] <- NA
    
    write_proprietaire(pro)
    removeModal()
    r <- update_data(r,input,output,session,FALSE)
    
    r$update <- TRUE
    
    showNotification("Les parcelles ont été dissociées et sont maintenant disponibles individuellement.")
  })
  
}

## To be copied in the UI
# mod_groupe_ui("groupe_ui_1")

## To be copied in the server
# callModule(mod_groupe_server, "groupe_ui_1")

