#' admin_acces UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_acces_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      passwordInput(ns("psw"),"Saisissez le mot de passe"),
      footer = tagList(
        actionBttn(ns("ok"),"OK",size = "xs"),
        modalButton("Quitter")
      )
    )
  )
}
    
#' admin_acces Server Function
#'
#' @noRd 
mod_admin_acces_server <- function(input, output, session){
  ns <- session$ns
 
  
  observeEvent(input$ok,{
    load(system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
    
    if(input$psw == ini$psw){
      callModule(mod_admin_server,"mod")
      showModal(mod_admin_ui(ns("mod")))  
    }else{
      showNotification("Mot de passe erroné")
    }
    
  })
}
    
## To be copied in the UI
# mod_admin_acces_ui("admin_acces_ui_1")
    
## To be copied in the server
# callModule(mod_admin_acces_server, "admin_acces_ui_1")
 
