
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(shinyWidgets)
library(stringr)
library(shinyjs)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage( style = "background-color:#eeeeff",
                   
                   fluidRow(style = "background-color:#ddddff; padding-left: 10px; margin-top: -10px;",
                            h3("Bourse foncière forestière communale de Cheny")  
                   ),
                   
                   tags$style("#side {margin-top: 10px;margin-left:0px;}"),
                   tags$style(".bttn {width:150px;}"),
                   
                   column(3, id="side",    
                          # tags$style("th, td {ont-size:10px;padding:1px !important; font-stretch: extra-condensed !important}
                          shinyjs::useShinyjs(),
                          
                          fluidRow(
                            # box(width=3,
                            #     
                            #     
                            # 
                            #     # style = "backgroud-color:#cceeee",
                            #     plotOutput("bilan_plot"),
                            #     tableOutput("bilan_prop"),
                            #     tableOutput("bilan_acq")
                            # ),
                            # 
                            # box(width = 12,
                            tags$style(".form-group {margin-bottom: -10px;}"),
                            # tags$style(".form-group {display:inline-block;margin-top:-20px;margin-bottom:-20px}"),
                            # tags$style("#proprio.form-group {display:block;}"),
                            # tags$style(".shiny-text-output {display:inline-block;padding-left:0px;font-size:14px; color: blue}"),
                            # tags$style("div.menu {display:inline-block;}"),
                            # tags$style(".action-button {vertical-align:top}"),
                            # tags$style(".col-sm-3, .col-sm-6, .col-sm-9, .col-sm-12 {padding-left:5px;}"),
                            
                            htmlOutput(("info_groupe")),
                            htmlOutput("surface"),
                            h6(style = "margin-bottom:-20px","pondération valeur:"),
                            numericInput("coeff_val","",min = 0,max = 10,step = 0.1,value = 1,width = "100px"),
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
                          
                          # 
                          # hidden(fluidRow(
                          #     hidden(switchInput(
                          #         inputId = "echangeable",
                          #         label = "Echangeable", 
                          #         labelWidth = "80px",
                          #         onLabel = "Oui",
                          #         offLabel = "Non",
                          #         size="mini"
                          #         # )
                          #     )))
                          
                   ),
                   
                   column(9,
                          tags$style(".head {margin-top: 10px;}"),
                          tags$style("h5 {margin-top: 0px;}"),
                          fluidRow(style="margin-top:-5px",
                                   column(12,
                                          h4(uiOutput("commentaires_parcelle"))
                                   )
                                   
                          ),
                          # div(id="reunion_proprietaires",style="margin-top:15px;backgroud-color:#ccffccc",
                          #     column(2,
                          #            h4("propriétaire:"),
                          #            h4("repris par:")
                          #     ),
                          #     column(10,style="margin-top:-20px", 
                          #            fluidRow(style="margin-top:20px",
                          #                     hidden(radioGroupButtons(
                          #                         inputId = "proprietaire",
                          #                         label = NULL,
                          #                         choices = "?",
                          #                         justified = TRUE,
                          #                         checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                          #                     ))
                          #                     
                          #            ),
                          #            fluidRow(style="margin-top:20px",
                          #                     hidden(radioGroupButtons(
                          #                         inputId = "new_proprietaire",
                          #                         label = NULL,
                          #                         choices = "?",
                          #                         justified = TRUE,
                          #                         checkIcon = list(
                          #                             yes = icon("ok", 
                          #                                        lib = "glyphicon"))
                          #                     )
                          #                     )
                          #            )
                          #     )
                          # ),
                          # 
                          fluidRow(id="autre_proprio",
                                   
                                   # column(6,style="margin-top:-4px", 
                                   #        htmlOutput("info_autre_proprio")
                                   # ),
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
                          ),
                          fluidRow(id="proprio",
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
                                                checkIcon = list(
                                                  yes = icon("ok", 
                                                             lib = "glyphicon"))
                                              )
                                       )
                                              )
                                   
                          ),
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
                   bsTooltip("","")
))