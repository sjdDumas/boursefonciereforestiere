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
      
      radioGroupButtons(ns("contacts"),"Contacter un propriétaire",choices =  "NULL",selected = NULL),
      br(),
      textAreaInput(ns("text_mail_proprio"),"Ecrivez ici votre message:",width = "600px"),
      br(),
      actionBttn(ns("send_mail_proprio"),"Envoyer le mail au propriétaire sélectionné",size="xs"),
  br(),
      uiOutput(ns("contact")),
      
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
  
  r$ide <- read_identite() %>% arrange(proprietaire)
  r$propri <- r$ide$proprietaire[-1]
  
  observe({
    req(r$propri)
    
    updateRadioGroupButtons(session,"contacts",choices = r$propri)
  })
  
  observeEvent(input$send_mail_proprio, ignoreInit = TRUE,handlerExpr = {
  
    if(!demo){  
    admin <- r$admin
    dest <- r$ide$mail[r$ide$proprietaire == input$contacts]
    envoi <- try(send.mail(from = admin$mail,
              to = dest,
              subject = paste("Message de la part de",r$user),
              body = input$text_mail_proprio,
              
              smtp = list(host.name = admin$host, port = admin$port_smtp, 
                          user.name = admin$username_smtp,            
                          passwd = admin$password_smtp, ssl = TRUE),
              authenticate = TRUE,
              debug = TRUE,
              send = TRUE))
    if(class(envoi) == "try-error"){
      showNotification(paste("Désolé, votre message n'a pas pu être envoyé: réessayez. Si l'erreur persiste, contactez: ",
                             r$admin$mail),type = "error")
    }else{
      showNotification(paste("Message envoyé à",input$contacts))  
    }
    }else{
      showNotification(paste("Message envoyé à",input$contacts))
    }
    
    updateRadioGroupButtons(session,'contacts',selected = NULL)
    
  })
  
  
  output$contact <- renderUI({
    tagList(
      a("... ou cliquez ici pour contactez l'administrateur du site", href=paste0("mailto::",r$admin$mail))
    )
  })
  
}
    
## To be copied in the UI
# mod_contact_proprio_ui("contact_proprio_ui_1")
    
## To be copied in the server
# callModule(mod_contact_proprio_server, "contact_proprio_ui_1")
 
