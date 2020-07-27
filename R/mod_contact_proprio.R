#' contact_proprio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_contact_proprio_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = "Contacter un propriétaire",
      size = "l", easyClose = TRUE,
      
      radioGroupButtons(ns("contacts"),"Contacter un propriétaire",choices =  propri,selected = NULL),
      br(),
      textAreaInput(ns("text_mail_proprio"),"Ecrivez ici votre message:",width = "600px"),
      br(),
      actionBttn(ns("send_mail_proprio"),"Envoyer le mail au propriétaire sélectionné",size="xs"),
      
      footer = tagList(
        modalButton("Fermer")
      )
    )
    
 
  )
}
    
#' contact_proprio Server Function
#'
#' @noRd
#' @import mailR 
mod_contact_proprio_server <- function(input, output, session, r){
  ns <- session$ns
  
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
  
}
    
## To be copied in the UI
# mod_contact_proprio_ui("contact_proprio_ui_1")
    
## To be copied in the server
# callModule(mod_contact_proprio_server, "contact_proprio_ui_1")
 
