#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = "administration",
      
      
      textInput(ns("psw_admin"),"dossier de sauvegarde"),
      br(),
      actionBttn(ns("new_psw"),"mettre à jour le chemin du dossier de sauvegarde",size = "xs"),
      br(),
      textInput(ns("path"),"dossier de sauvegarde"),
      br(),
      actionBttn(ns("new_path"),"mettre à jour le chemin du dossier de sauvegarde",size = "xs"),
      br(),
      actionBttn(ns("backup"),"Sauvegarde",size = "xs"),
      br(),
      htmlOutput(ns("check")),
      br(),
      footer = tagList(
        actionButton(ns("ok"),"Quitter"),
        modalButton(NULL)
      )
    )
    
  )
}

#' admin Server Function
#'
#' @noRd 
mod_admin_server <- function(input, output, session){
  ns <- session$ns
  
  observe({
    load(system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
    
    updateTextInput(session,"path",value = ini$dir)
    
    updateTextInput(session,"psw_admin",value = ini$psw)
  })
  
  observeEvent(input$new_path,{
    ini$dir <- input$path
    save(ini,file = system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
    showNotification("chemin mis à jour")
    
  })
  
  observeEvent(input$new_psw,{
    ini$psw <- input$new_psw
    save(ini,file = system.file("app/www/ini.RData",package = "boursefonciereforestiere"))
    showNotification("mot de passe mis à jour")
    
  })
  
  observeEvent(input$backup,{
    admin_backup()
    showNotification("sauvegarde OK")
    
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
    quit()
  })
}

## To be copied in the UI
# mod_admin_ui("admin_ui_1")

## To be copied in the server
# callModule(mod_admin_server, "admin_ui_1")

