#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinyBS shinyWidgets shinybusy
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$head(
      tags$script(
        '$(document).ready(function(){
          $.get("http://ipinfo.io", function(response) {
          Shiny.onInputChange("getIP", response);
            }, "json");
          });'
      )
      
    ),
    # tags$script(
    #   "(function() {
    #     var timeoutWarningMsecs = 120 * 1000;
    #     var idleTimer;
    # 
    #     function onTimeout() {
    #       alert('Session supsendu pour cause d inactivité');
    #     }
    # 
    #     function startIdleTimer() {
    #       if (idleTimer) clearTimeout(idleTimer);
    #       idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
    #     }
    # 
    #     $(document).on('shiny:message shiny:inputchanged', startIdleTimer);
    # 
    #   })();"
    # ),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
      fluidPage( style = "background-color:#eeeeff",
            
                 div(style="position:absolute;top:50vh;right:62vw", spin_epic("hollow-dots")),
                 
                 
                 fluidRow(style = "background-color:#ddddff; padding-left: 10px; margin-top: -10px;",
                          h3("Bourse foncière forestière communale de Cheny")  
                 ),
                 
                 tags$style("body {padding-right: 0px !important;}"),
                 tags$style("#side {margin-top: 10px;margin-left:0px;}"),
                 tags$style(".bttn {width:150px;}"),
                 tags$style("#quit {background:none; border:none;;}"),
                 
                 div(id = "bouttonQuit", style = "position: absolute; top: 4px; right: 4px;",
                     actionButton("quit","",icon = icon("times-circle"))),
                 
                 column(3, id="side",    
                        # tags$style("th, td {ont-size:10px;padding:1px !important; font-stretch: extra-condensed !important}
                        shinyjs::useShinyjs(),
                        
                        fluidRow(
                          tags$style(".form-group {margin-bottom: -10px;}"),
                          
                          htmlOutput(("info_groupe")),
                          htmlOutput("surface"),
                          h6(style = "margin-bottom:-20px","pondération valeur:"),
                          sliderInput("coeff_val","",min = 0,max = 1,step = 0.1,round = FALSE, 
                                        value = 1,width = "100px"),
                          br()
                        ),
                        
                        fluidRow(style="background-color:#ffffff;margin-left:-30px;padding-left:10px;padding-bottom:50px",
                                 br(),
                                 actionBttn("rendu_bilan","Mes données", size = "xs", icon = icon("table")),
                                 actionBttn("groupe","grouper des parcelles",size = 'xs'),
                                 actionBttn("contact","Contact", size = "xs",  icon = icon("envelope")),                                   
                                 actionBttn("help","Aide", size = "xs",  icon = icon("question-circle")),
                                 h4("Accéder à une parcelle :"),
                                 h5("cliquez sur une pacrcelle, ou sélectionnez:"),
                                 selectInput("select_commune","commune",choices = NULL,width = "150px"),
                                 selectInput("select_section","section",choices = NULL,width = "150px"),
                                 selectInput("select_parcelle","parcelle",choices = NULL,width = "150px"),
                                 actionBttn("zoom","zoomer sur la parcelle",  icon = icon("zoom"),size = 'xs'),
                        )
                 ),
                 
                 column(9,
                        tags$style(".head {margin-top: 10px;}"),
                        tags$style("h5 {margin-top: 0px;}"),
                        fluidRow(style="margin-top:-5px",
                                 column(12,
                                        h4(uiOutput("commentaires_parcelle"))
                                 )
                        ),
                        hidden(fluidRow(id="autre_proprio",
                                        column(6,#style="margin-top:-20px", 
                                               htmlOutput("is_prise")
                                               # div(id='is_prise',
                                               #     h6("quelqu'un s'est déjà déclaré intéressé"))
                                        ),
                                        column(6,style="margin-top:-20px", 
                                               radioGroupButtons(
                                                 inputId = "is_interesse",
                                                 label = "",
                                                 choiceNames = c("je suis intéressé","pas intéressé"),
                                                 choiceValues = c("oui","non"),
                                                 selected="non",
                                                 justified = TRUE, size = "sm",
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"))
                                               )
                                        )
                        )),
                        hidden(fluidRow(id="proprio",
                                        column(3,           
                                               h5("Etes-vous le propriétaire?")
                                        ),
                                        column(3, style="margin-top:-20px", 
                                               radioGroupButtons(
                                                 inputId = "is_proprio",
                                                 label = "",
                                                 choices = c("oui","non"),
                                                 selected="non",
                                                 justified = TRUE, size = "sm",
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"))
                                               )
                                               
                                        ),
                                        div(id="est_proprio",
                                            column(2, h5("Vous souhaitez:")),
                                            column(4, style="margin-top:-20px;padding:0", 
                                                   radioGroupButtons(
                                                     inputId = "choix_proprio",
                                                     label = "",
                                                     choiceNames = c("l'échanger","la vendre","la conserver"),
                                                     choiceValues = c("echange","vend","garde"),
                                                     justified = TRUE, size = "sm",
                                                     selected = "echange",
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"))
                                                   )
                                            )
                                        )
                                        
                        )),
                        bsAlert("alert"),
                        fluidRow(style="margin-top:20px;margin-left:0px;",
                                 leafletOutput("map",height = "80vh"),
                                 htmlOutput("legende")
                        )
                        
                        
                 ),  
                 
                 tags$style(".popover {width: 800px;max-width: 100%;background-color:#ffffee;font-size:14}"),
                 bsPopover("groupe","Gérer les groupe de parcelles","Vous ne souhaitez pas échanger ou vendre une parcelle sans que vos parcelles voisines ne troubent preneur ? Créez un groupe de parcelles qui formeront un lot unique."),  
                 bsPopover("info_groupe","Groupe défini par le propriétaire","Lorsque le proriétaire définit un groupe de parcelles, celles-ci sont indisszociables et ne peuvent être demandées individuellement?"),
                 bsPopover("coeff_val","Coefficient de valeur de la parcelle",
                           paste0("La <b>surface de la parcelle</b> est multipliée par ce coefficient pour obtenir un indice de valeur.<br>",
                                  "Pour vos parcelles proposées en échange merci d`estimer ce coefficient de la manière suivante:<br>",
                                  "<ul><li><b>Cas général</b>terrain à peuplier des vallées de l Yonne et de l Armançon ou des sources:1</li>",
                                  "<li>Terrains sableux, secs, anciennes sablières, ou marégageux: 0.5",
                                  "<li>Autres terrains non alimentés par une nappe d`eau: 0.75</li></ul>",
                                  "Le boisement situé dans la parcelle ne rentre pas en compte dans le calcul de cet indice. ",
                                  "Il est suspetible cependant d entraîner une compensation financière (à voir au cours de la négociation finale)."
                           ),
                           placement = "right",
                           options = list(container = "body",
                                          style = "width:300px;font-size:14px")
                 ),
                 bsPopover("surface","surface géographique (peut diverger légèrement de la surface cadastrale)"),
                 bsPopover("rendu_bilan","Récaitulatif de vos parcelles","Accédez aux tabeaux des parcelles que vous avez enregistées ou pour lesquelles vous avez déclaré votre intérêt"),
                 bsPopover("contact","Envoyer un mail à un propriétaire"),
                 bsTooltip("quit","Quitter"),
                 
                 
      )
    )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'bourse'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

