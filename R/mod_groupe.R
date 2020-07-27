#' groupe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_groupe_ui <- function(id){
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
        choices = choices_ppu,
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
        choices = choices_ppg,
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
  
}
    
## To be copied in the UI
# mod_groupe_ui("groupe_ui_1")
    
## To be copied in the server
# callModule(mod_groupe_server, "groupe_ui_1")
 
