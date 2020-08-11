#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom purrr map_chr

mod_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = "administration",
      
      column(8,
             textOutput(ns("path"))       
      ),
      column(4,
             actionButton(ns("ok"),"Quitter")
      ),
      
      br(),br(),
      h4("Changer le mot de passe administrateur"),
      fluidRow(
      column(6,
             textInput(ns("new_psw"),"")
      ),
      column(6,
             actionBttn(ns("new_psw_ok"),"mettre à jour le mot de passe administrateur",size = "xs")
      )
      ),
      br(),br(),br(),
      
      h4("Comptes en attente de validation"),
      tableOutput(ns("a_valider")),
      br(),
      fluidRow(
      column(6,
             selectInput(ns("login_a_valider"),"login à valider",choices = NULL)
      ),
      column(6,
             actionBttn(ns("valider_ok"),"Valider",size = "xs")
      )
      ),
    
      h4("Gestion de la base de données"),
      fluidRow(
      column(6,
             actionBttn(ns("backup"),"Sauvegarder les données",size = "xs")
      ),
      column(6,
              actionBttn(ns("init"),"Réinitiaiser les données",size = "xs")
      )
      ),
      br(),br(),
      htmlOutput(ns("check")),
      br(),
      footer = NULL
    ),
    size="l"
    
  )
}

#' admin Server Function
#'
#' @noRd 
mod_admin_server <- function(input, output, session){
  ns <- session$ns
  
  observe({
    
    output$path <- renderText(paste("dossier de la base SQLITE:", path))
    
  })
  
  
  observeEvent(input$new_psw_ok,{
    ini <- list(psw=input$new_psw)
    saveRDS(ini,file.path(path,"ini.rds"))
    showNotification("mot de passe mis à jour")
    
  })
  
  observeEvent(input$backup,{
    admin_backup()
    showNotification("sauvegarde OK")
    
  })

  observeEvent(input$init,{
    admin_reset()
    showNotification("réinitialisation OK")
    
  })
  
  output$a_valider <- renderTable({
    i <- read_identite(inv = TRUE) %>% filter(valide == 0) %>% 
      select(-c(couleur_proprietaire,valide))
    updateSelectInput(session,"login_a_valider",choices = i$proprietaire)
    i
  })
  
  observeEvent(input$valider_ok,{
    
    i <- read_identite(inv = TRUE)
    i$valide[i$proprietaire == input$login_a_valider] <- 1
    
    admin <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)
    
    dest <- i$mail[i$proprietaire == input$login_a_valider]
    envoi <- try(send.mail(from = admin$mail,
                           to = dest,
                           subject = "Validation de votre compte",
                           body =   paste0("Bonjour, \n",
                                           "Votre compte ",input$login_a_valider, " a été validé.\n",
                                           "Vous pouvez maintenant proposer vos parcelles et consulter les parcelles mises à disposition par les autres propriétaires. ",
                                           "Rendez-vous sur le site de la bourse: ",
                                           admin$adresse, "\n\n",
                                           "Bien cordialement,\n",admin$administrateur,", administrateur de la bourse foncière forestière"
                           ),
                           smtp = list(host.name = admin$host, port = admin$port_smtp, 
                                       user.name = admin$username_smtp,            
                                       passwd = admin$password_smtp, ssl = TRUE),
                           authenticate = TRUE,
                           debug = TRUE,
                           send = TRUE)
    )
    
    if(class(envoi)=="try-error"){
      showNotification("Le mail de confirmation au propriétaire n'a pas pu être envoyé",
                       type = "error", duration = 30)
    }else{
      write_identite(i)
      showNotification(("login validé avec succès"))
    }
    
  })
  
  output$check <- renderUI({
    ck <- admin_check()
    cm <- purrr::map_chr(1:length(ck),
                         function(x){
                           if(ck[x]=="err"){
                             paste0("<b>",names(ck)[x],"</b> : <span style='color:red;'>manque</span><br>")
                           }else{
                             paste0("<b>",names(ck)[x],"</b> : <span style='color:green;'>ok</span><br>")
                           }
                         })
    HTML(paste(cm),collapse="")
  }
  
  )
  
  
  observeEvent(input$ok,{
    stopApp()
  })
}

## To be copied in the UI
# mod_admin_ui("admin_ui_1")

## To be copied in the server
# callModule(mod_admin_server, "admin_ui_1")

