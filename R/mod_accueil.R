#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(size="l",
                title = "Bourse foncière forestière de CHENY",
                
                fluidRow(
                  column(6,textInput(ns("login"),"Nom:",value = "",width = "150px")),
                  column(6,passwordInput(ns("psw"), "Mot de passe", value = "",width = "150px")), 
                ),
                br(),
                column(6,actionBttn(ns("enter"),"Accéder à la bourse",size="xs")),
                column(6,actionBttn(ns("info_site"),"Informations",size="xs",color="royal")),
                br(),br(),
                h5("Pas encore inscrit ?"),
                actionBttn(ns("inscription"),"s'inscrire",size="xs"),
                
                hidden(
                  fluidRow(id=ns("form_new_login"), style="transform: scale(0.8);border-radius:10px;background-color:#ddddff;margin:-50px;padding:20px",
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
                             column(12,textInput(ns("new_login"),"Nom",value=NULL))
                           ),
                           br(),
                           fluidRow(
                             column(6,textInput(ns("new_psw"), "Mot de passe", value = NULL)),
                             column(6,textInput(ns("new_psw2"), "confirmation mot de passe", value = NULL))
                           ),
                           br(),
                           fluidRow(
                             column(6,textInput(ns("mail"), "Adresse mail", value = NULL)),
                             column(6,textInput(ns("mail2"), "confirmation adresse mail", value = NULL))
                           ),
                           br(),br(),
                           actionBttn(ns("submit_acces"),"Demander l'ouverture d'un compte",size="xs")
                  )
                ),
                footer = NULL
    ) 
  )
}
    
#' accueil Server Function
#'
#' @noRd
#' 
#' @import mailR DBI
mod_accueil_server <- function(input, output, session,r){
  ns <- session$ns
  
  observeEvent(input$info_site,{
    showModal(
      modalDialog(
        includeHTML("presentation.html"),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
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
      
      r$par = readRDS("inst/data/rdata/parcelles.rds")
      r$parcelle <- "Cheny_0A_0203"
      r <- update_data(r,input,output,session,ini=TRUE)
      
      co <- as.character(unique(r$par$nom_com)[order(unique(r$par$nom_com))])
      updateSelectInput(session,"select_commune",choices = co,selected = "Cheny")  
      
      addResourcePath("img","inst/img")
      
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
      db <- dbConnect(RSQLite::SQLite(), "inst/db.sqlite")
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
  
}
    
## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")
    
## To be copied in the server
# callModule(mod_accueil_server, "accueil_ui_1")
 