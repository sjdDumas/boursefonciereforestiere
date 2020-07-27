#' bilan_proprio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bilan_proprio_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = "Mes données",
      size = "l", easyClose = TRUE,
      
      h4("Bilan actuel des échanges"),
      tableOutput(ns('bilan_echange')),
      
      # plotOutput("bilan_plot"),
      h4("parcelles dont je suis propriétaire"),
      
      tableOutput(ns("bilan_prop")),
      
      h4("Parcelles pour lesquelles j'ai déclaré être intéressé"),
      
      tableOutput(ns("bilan_acq")),
      
      footer = tagList({
        modalButton("Fermer")
      })
      
    )
 
  )
}
    
#' bilan_proprio Server Function
#'
#' @noRd 
mod_bilan_proprio_server <- function(input, output, session, r){
  ns <- session$ns
  
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
    
## To be copied in the UI
# mod_bilan_proprio_ui("bilan_proprio_ui_1")
    
## To be copied in the server
# callModule(mod_bilan_proprio_server, "bilan_proprio_ui_1")
 
