#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id,com,r){
  ns <- NS(id)
  tagList(
    modalDialog(size="l",
                tags$style(paste0("#",ns("admin")," {position:absolute;;right:10px;top:-40px;border:none;}")),
                
                actionButton(ns("admin"),"",icon = icon("cogs"),width = "40px"),
                
                title = paste("Bourse foncière forestière de", toupper(com)),
                # actionBttn(ns("oublie"),"test"),
                fluidRow(
                  column(6,textInput(ns("login"),"Nom:",value = "",width = "150px")),
                  column(6,passwordInput(ns("psw"), "Mot de passe", value = "",width = "150px")), 
                ),
                br(),
                fluidRow(
                  column(6,actionBttn(ns("enter"),"Accéder à la bourse",size="xs")),
                  # column(6,actionBttn(ns("info_site"),"Informations",size="xs",color="royal")),
                  column(6,actionBttn(ns("oublie"),"Mot de passe oublié",size="xs"))
                ),
                br(),
                uiOutput(ns("contact")),
                br(),hr(),
                
                actionBttn(ns("inscription"),"s'inscrire",size="xs"),
                br(),         
                hidden(
                  fluidRow(id=ns("form_new_login"), style="transform: scale(1);border-radius:10px;background-color:#ddddff;margin:-50px;padding:20px",
                           h4("Ouvrir un compte"),
                           h6(style="font-size:12px",HTML(paste(
                             "La bourse d'échange foncière forestière de ",toupper(com)," est un service collaboratif entre particulier",
                             "Il est fourni gratuitement, sans aucune garantie de résultat. Les démarches d'échange, de vente ou d'achat restent entièrement à la charge des utilisateurs du site.<br>",
                             "En créant un compte, vous acceptez que votre identifiant soit accessibles aux autres utilisateurs du site.",
                             "Ces informations sont enregistrées dans un fichier informatisé par M. Stéphane Dumas afin de permettre les contacts entre utilisateurs. La base légale du traitement est la mission d'intérêt publique.",
                             "Les données collectées seront communiquées aux seuls utilisateurs du site.",
                             "Elles sont conservées pendant une durée de 1 an.",
                             "Vous pouvez accéder aux données vous concernant, les rectifier, demander leur effacement, en ecrivant à l'administrateur du site à l'adresse <a href='mailto:",r$admin$mail,"'>",r$admin$mail,"</a>",
                             "Consultez le site cnil.fr pour plus d’informations sur vos droits."))),
                           
                           fluidRow(
                             column(12,textInput(ns("new_login"),"Nom",value=NULL))
                           ),
                           br(),
                           fluidRow(
                             column(6,passwordInput(ns("new_psw"), "Mot de passe", value = NULL)),
                             column(6,passwordInput(ns("new_psw2"), "confirmation mot de passe", value = NULL))
                           ),
                           br(),
                           fluidRow(
                             column(6,textInput(ns("mail"), "Adresse mail", value = NULL)),
                             column(6,textInput(ns("parcelle"), "section et numéro d'une parcelle dont vous être propriétaire sur la commune", value = ""))
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
  callModule(mod_admin_acces_server, "admin_acces_ui_1")
  
  
  output$contact <- renderUI({
    tagList(
      a("Contactez-nous", href=paste0("mailto::",r$admin$mail))
    )
  })
  
  
  # Accès administrateur ----------------------------------------------------
  
  observeEvent(input$admin,{
    showModal(mod_admin_acces_ui(ns("admin_acces_ui_1")))
  })
  
  # page d'aide (retirée: se fait via shinyproxy) ---------------------------
  
  observeEvent(input$info_site,{
    showModal(
      modalDialog(
        includeMarkdown(file.path(system.file(package = "boursefonciereforestiere"),"aide","manuel_utilisateur.md")),
        actionButton(ns("exit"),"Retour à l'authentification"),
        footer = NULL,
        size="l")
    )
  })
  
  observeEvent(input$exit,{
    removeModal()
    callModule(mod_accueil_server,"mod2",r)
    showModal(mod_accueil_ui("mod2",r$admin$commune,r))
  })
  
  
  # Demande d'accès ---------------------------------------------------------
  
  observeEvent(input$enter,{
    
    message("------------------------------------------------------")
    message("demande d'accès:--------------",input$login,"----------")
    message("------------------------------------------------------")
    
    
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
      
      r <- update_data(r,input,output,session,ini=TRUE)  
      
      message("event ini OK")
      ate <- TRUE
      parcelle <- r$admin$parcelle_ini
      r$parcelle <- get_parcelle(r,parcelle)
      
      path <- get_path()
      db <- dbConnect(RSQLite::SQLite(), file.path(path,"db.sqlite"))
      on.exit(dbDisconnect(db))
      if("notification" %in% dbListTables(db)){
        
        not <- dbReadTable(db,"notification") %>% filter(proprietaire == r$user)
        if(nrow(not)>0){
          for(i in 1:nrow(not)){
            showNotification(not$message[i],duration = 1000)
          }
          not <- dbReadTable(db,"notification") %>% filter(proprietaire != r$user)
          dbWriteTable(db,"notification",not,overwrite=TRUE)
          
        }
        
      }
      
      shinybusy::remove_modal_spinner() 
    }
    
  })
  
  # mot de passe oublié -----------------------------------------------------
  
  
  observeEvent(input$oublie,{
    showModal(
      modalDialog(
        title = "Mot de passe oublié",
        textInput(ns("oublie_mail"),"Votre adresse mail:"),
        footer = tagList(
          actionButton(ns("oublie_ok"),"Demander mon mot de passe"),
          modalButton("Annuler")
        )
      )
    )
  })
  
  observeEvent(input$oublie_ok,{
    id <- read_identite() %>% filter(mail == input$oublie_mail)
    
    if(nrow(id)==0){
      showNotification("Désolé, cette adresse mail ne correspond à aucun compte.",
                       type = 'error',duration = 10)
    }else{
      envoi <- try(send.mail(from = r$admin$mail,
                to = id$mail,
                subject = "mot de passe oublié",
                body = paste0("Bonjour,\n\n",
                              "Vos identifiants sont:\n   - Nom: ",id$proprietaire,
                              "\n   - Mot de passe: ",id$psw,"\n\n",
                              "Bien cordialement,\n",r$admin$administrateur,", ",r$admin$titre_administrateur),
                smtp = list(host.name = r$admin$host, port = r$admin$port_smtp, 
                            user.name = r$admin$username_smtp,            
                            passwd = r$admin$password_smtp, ssl = TRUE),
                authenticate = TRUE,
                debug = TRUE,
                send = TRUE
      ))
      
      showNotification("Un mail vous a été envoyé. Quittez cette page et ouvrez-en une nouvelle pour vous connecter.",
                       duration = 30)
      
    }
  })
  
  
  # Demande d'inscription ---------------------------------------------------
  
  # informations
  
  observeEvent(input$inscription,{
    showElement("form_new_login")
  })
  
  # formulaire
  
  observeEvent(input$submit_acces,{
    
    message("demande d'inscription:--------------",input$new_login,"----------")
    if(input$new_login==""){
      showNotification("Vous devez saisir votre nom",type = "error", duration = 10)
    }else if(str_length(input$new_psw)<8){
      showNotification("Le mot de passe doit contenir au moins 8 caractères",type = "error", duration = 10)
    }else if(input$parcelle == ""){
      showNotification("Merci d'indiquer les références cadastrales d'une parcelle dont vous êtes le propriétaire: l'accès à la bourse n'est ouvert qu'aux seuls propriétaires.",
                       type = "error", duration = 10)
      
    }else if(input$new_psw2 != input$new_psw){
      showNotification("La confirmation de votre mot de passe est différente du mot de passe",type = "error", duration = 10)
      
    }else if(! str_detect(input$mail,"@")){
      showNotification("Adresse mail invalide",type = "error", duration = 10)
      
    }else if(input$new_login %in% read_identite()$proprietaire){
      
      showNotification("Ce nom de compte existe déjà. Si vous avez oublié votre mot de passe, merci de contacter par mail sjd.dumas@laposte.net. Sinon, choisissez un autre nom.",type = "error", duration = 10)
      
    }else{
      
      # tentative d'envoi de mail au demandeur. Si échec: adresse mail erronée
      
      admin <- r$admin
      dest <- r$ide$mail[r$ide$proprietaire == input$contacts]
      envoi <- try(send.mail(from = admin$mail,
                             to = input$mail,
                             subject = "Demande d'inscription",
                             body = paste0(
                               "Bonjour\n\n",
                               "Votre demande d'inscription à la bourse foncière forestière de ",
                               admin$commune,
                               " a bien été enregistrée et est en cours de traitement.\n",
                               "Vous recevrez un prochain mail vous informant de l'ouverture de votre compte.\n\n",
                               "Bien cordialement,\n",admin$administrateur,", ",admin$titre_administrateur,", administrateur du site."
                             ),
                             
                             smtp = list(host.name = admin$host, port = admin$port_smtp, 
                                         user.name = admin$username_smtp,            
                                         passwd = admin$password_smtp, ssl = TRUE),
                             authenticate = TRUE,
                             debug = TRUE,
                             send = TRUE
      ))
      
      if(class(envoi) == "try-error"){
        
        showNotification("L'adresse mail saisie ne semble pas être une adresse valide.",
                         type = "error", duration = 10)
      }else{
        
        # tout est correct: enregistrement et mails d'info à l'administrateur'
        
        showNotification("Votre demande d'ouverture de compte a bien été enregistrée et sera traitée aussi vite que possible. Vous pouvez fermer cette page.",
                         duration = 30)
        send.mail(from = admin$mail,
                  to = admin$mail,
                  subject = "Demande d'inscription",
                  body = paste("login: ", input$new_login,"\n\n"),
                  
                  smtp = list(host.name = admin$host, port = admin$port_smtp, 
                              user.name = admin$username_smtp,            
                              passwd = admin$password_smtp, ssl = TRUE),
                  authenticate = TRUE,
                  debug = TRUE,
                  send = TRUE
        )
        
        id <- read_identite()
        id <- rbind(id,
                    data.frame(proprietaire=input$new_login,
                               psw=input$new_psw,
                               mail=input$mail,
                               valide=0,
                               parcelle=input$parcelle))
        db <- dbConnect(RSQLite::SQLite(), file.path(r$dir,"db.sqlite"))
        dbWriteTable(db,'identite',id,overwrite = TRUE)
        dbDisconnect(db)
        
      }
      
      observeEvent(input$exit_insc,{
        stopApp()
      })
      
      # send.mail(from = r$admin$mail,
      #           to = input$mail,
      #           subject = paste("bourse foncière forestière de",r$admin$commune),
      #           body = paste("Bonjour,\n Votre compte est ouvert. Vous pouvez y accéder dès maintenant.\n",
      #                        "Vos identifiants sont:\n   - Nom: ",input$new_login,
      #                        "\n   - Mot de passe: ",input$new_psw,"\n\n",
      #                        "Pour toute question, n`hésitez pas à me contacter par retour de mail.\n\n",
      #                        "Bien cordialement,\n",r$admin$administrateur,", ",r$admin$titre_administrateur),
      #           
      #           smtp = list(host.name = r$admin$host, port = r$admin$port_smtp, 
      #                       user.name = r$admin$username_smtp,            
      #                       passwd = r$admin$password_smtp, ssl = TRUE),
      #           authenticate = TRUE,
      #           send = TRUE)
      # 
      # showNotification("Inscription enregistrée. Votre compte est ouvert: saisissez votre nom et votre mot de passe...",duration = 20)
      # hideElement("form_new_login")
      # updateTextInput(session,"login",value="")
      # updateTextInput(session,"psw",value="")
      
    }
    
  })
  
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# callModule(mod_accueil_server, "accueil_ui_1")

