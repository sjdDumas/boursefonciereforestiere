#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {

  
  with_golem_options(
    
    app = shinyApp(
      
      ui = app_ui, 
      server = app_server,
      onStart = function() {
    
        
        # cat(paste(Sys.time(),"\n"),file = trace)
        
        onStop(function() {
          
          # cat(paste(Sys.time(),"\n"),file = trace)
          # close(trace)
          # 
          # ms <- paste(readLines(tracefl),collapse = "\n")
          # admin <- read.csv(file.path(path,"admin.csv"),stringsAsFactors = F)
          # message(admin$mail)
          # send.mail(from = admin$mail,
          #           to = admin$mail,
          #           subject = "Accès",
          #           body = ms,
          # 
          #           smtp = list(host.name = admin$host, port = admin$port_smtp,
          #                       user.name = admin$username_smtp,
          #                       passwd = admin$password_smtp, ssl = TRUE),
          #           authenticate = TRUE,
          #           debug = TRUE,
          #           send = TRUE
          # )
          # 
          # unlink(tracefl)

              if(demo){
                print("unlink")
                unlink(get_path(),recursive = TRUE)
              }
          return(trace)
            })
        }
    ), 
    golem_opts = list(...)
  )
}

