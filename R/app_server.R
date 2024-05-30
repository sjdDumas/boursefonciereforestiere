#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import stringr dplyr sf
#' @noRd
app_server <- function( input, output, session, demo = FALSE) {

  tags$script('
              var shinyBS = {inputBindings: {}};

shinyBS.inputBindings.toggle = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.toggle, {
  find: function(scope) {
    return $(scope).find(".sbs-toggle-button");
  },
  getValue: function(el) {
    return $(el).hasClass("active");
  },
  subscribe: function(el, callback) {
    $(el).on("click", function(e) {
      $(el).toggleClass("active").blur();
      callback();
    })
  },
  unsubscribe: function(el) {
    $(el).off("click");
  }
});
Shiny.inputBindings.register(shinyBS.inputBindings.toggle)

shinyBS.inputBindings.modal = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.modal, {
  find: function(scope) {
    return $(scope).find(".sbs-modal");
  },
  getValue: function(el) {
    return $(el).hasClass("in");
  },
  subscribe: function(el, callback) {
    $(el).on("hidden.bs.modal shown.bs.modal", callback)
  },
  unsubscribe: function(el) {
    $(el).off("hidden.bs.modal shown.bs.modal")
  },
  receiveMessage: function(el, data) {
    if(data.hasOwnProperty("toggle")) {
      if(data.toggle == "show") {
        $(el).modal("show");
      } else if(data.toggle == "hide") {
        $(el).modal("hide");
      } else {
        $(el).modal("toggle");
      }
    };
  },
  initialize: function(el) {
    $("#" + $(el).attr("data-sbs-trigger")).attr({"data-toggle": "modal", "data-target": "#" + $(el).attr("id")});
  }
});
Shiny.inputBindings.register(shinyBS.inputBindings.modal);

shinyBS.inputBindings.collapse = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.collapse, {
  find: function(scope) {
    return $(scope).find(".sbs-panel-group");
  },
  getValue: function(el) {
    return $(el).data("sbs-value");
  },
  receiveMessage: function(el, data) {
    var $el = $(el);
/* I would think this code should work, but it doesnt for some reason so I am 
              commenting it out.
              if(data.hasOwnProperty("multiple")) {
                if(data.multiple) {
                  $el.find(".collapse").each(function(i) {$(this).collapse({parent: false, toggle: false})});
                } else {
                  $el.find(".collapse").each(function(i) {$(this).collapse({parent: "#"+$el.attr("id"), toggle: false})});
                }
              }
              */
                if(data.hasOwnProperty("style")) {
                  var panels = Object.keys(data.style)
                  for(var i = 0; i < panels.length; i++) {
                    var $p = $el.find("div[value=\'" + panels[i] + "\']")
                    $p
                    .removeClass("panel-primary panel-danger panel-warning panel-error panel-info panel-success")
                    .addClass("panel-" + data.style[panels[i]]);
                  }
                }
              if(data.hasOwnProperty(\'open\')) {
                if(!Array.isArray(data.open)) {
                  data.open = [data.open]
                }
                data.open.forEach(function(value, index, array) {
                  $el.find("div[value=\'" + value + "\'] > .panel-collapse").collapse("show");
                })
              }
              if(data.hasOwnProperty("close")) {
                if(!Array.isArray(data.close)) {
                  data.close = [data.close];
                }
                data.close.forEach(function(value, index, array) {
                  $el.find("div[value=\'" + value + "\'] > .panel-collapse").collapse("hide");
                })
              }
},
subscribe: function(el, callback) {
  $(el).find(".collapse").on("shown.bs.collapse hidden.bs.collapse", callback);
},
initialize: function(el) {
  var $el = $(el);
  var $panels = $el.children(".panel");
  var val = [];
  $panels.each(function(i) {
    if($(this).children("div.panel-collapse.collapse").hasClass("in")) {
      val.push($(this).attr("value"));
    }
    var $pan = $(this).children("div.panel-collapse.collapse");
    if($el.attr("data-sbs-multi") == "FALSE") {
      var par = "#" + $el.attr("id");
    } else {
      var par = false;
    }
    $pan.collapse({parent: par, toggle: false});
  });
  $el.data("sbs-value", val);
  $panels.on("show.bs.collapse", function(event) {
    var val = $el.data("sbs-value");
    val.push($(this).attr("value"));
    $el.data("sbs-value", val)
  });
  $panels.on("hide.bs.collapse", function(event) {
    var val = $el.data("sbs-value");
    var i = val.indexOf($(this).attr("value"))
    if(i != -1) {
      val.splice(i, 1);
      $el.data("sbs-value", val);
    }
  });
}
})
  Shiny.inputBindings.register(shinyBS.inputBindings.collapse);
  
  
  Shiny.addCustomMessageHandler("bsAlertCreate", function(data) {
    
    var create = true;
    
    if(data.hasOwnProperty("alertId")) {
      if($("#" + data.alertId).length > 0) {
        create = false;
      }
    }
    
    if(create) {
      
      var $alert = $("<div class = \'alert\'></div>");
      
      if(data.hasOwnProperty(\'style\')) {
        $alert.addClass("alert-" + data.style);
      } else {
        $alert.addClass("alert-info");
      }
      
      if(data.hasOwnProperty("dismiss")) {
        $alert.addClass("alert-dismissable");
      }
      
      if(data.hasOwnProperty("alertId")) {
        $alert.attr("id", data.alertId);
      }
      
      if(data.hasOwnProperty(\'dismiss\')) {
        if(data.dismiss == true) {
          $alert.append("<button type=\'button\' class=\'close\' data-dismiss=\'alert\'>&times;</button>")
        }
      }
      
      if(data.hasOwnProperty(\'title\')) {
        $alert.append("<h4>" + data.title + "</h4>");
      }
      
      if(data.hasOwnProperty("content")) {
        $alert.append(data.content);
      }
      
      if(data.append == true) {
        $alert.appendTo("#" + data.id);
      } else {
        $("#" + data.id).html($alert);
      }
      
    }
    
  });
  
  Shiny.addCustomMessageHandler("bsAlertClose", function(alertId) {
    $("#" + alertId).alert(\'close\');
  });
  
  // The following function refer to tooltips but are used in the creation of 
  // tooltips and popovers because there structure is so similar. type="popover"
  // will create a popover.
  
  shinyBS.addTooltip = function(id, type, opts) {
    var $id = shinyBS.getTooltipTarget(id);
    var dopts = {html: true};
    opts = $.extend(opts, dopts);
    
    if(type == "tooltip") {
      $id.tooltip("destroy");
      $id.tooltip(opts);
    } else if(type == "popover") {
      $id.popover("destroy");
      $id.popover(opts);
    }
    
  }
  
  shinyBS.removeTooltip = function(id, type) {
    var $id = shinyBS.getTooltipTarget(id);
    if(type == "tooltip") {
      $id.tooltip("destroy");
    } else if(type == "popover") {
      $id.popover("destroy");
    }
  }
  
  // Makes adjustments to the tooltip and popover targets for specialized 
  // shiny inputs/outputs
  shinyBS.getTooltipTarget = function(id) {
    
    var $id = $("#" + id).closest(".shiny-input-container, .shiny-bound-output, .btn, .shiny-download-link");
    
    /*  
      if($id.hasClass("js-range-slider")) {
        $id = $id.parent();
      } else if($id.hasClass("selectized")) {
        $id = $id.siblings("div.selectize-control")
      }
    */
      
      return $id;
    
  }
  
  Shiny.addCustomMessageHandler("updateTooltipOrPopover", function(data) {
    if(data.action == "add") {
      shinyBS.addTooltip(data.id, data.type, data.options);
    } else if(data.action == "remove") {
      shinyBS.removeTooltip(data.id, data.type)
    }
  })
  
  Shiny.addCustomMessageHandler("bsButtonUpdate", function(data) {
    
    var btn = $("button#" + data.id);
    var ico = btn.find("i");
    
    if(ico.length > 0) {
      ico = ico[0].outerHTML;
    } else {
      ico = "";
    };
    
    if(data.hasOwnProperty("label")) {
      btn.html(ico + data.label);
    };
    
    if(data.hasOwnProperty("icon")) {
      var ch = btn.children();
      if(ch.length == 0) {
        btn.prepend(data.icon);
      } else {
        btn.find("i").replaceWith(data.icon);
      };
    };
    
    if(data.hasOwnProperty("value")) {
      if(btn.hasClass("sbs-toggle-button")) {
        if(data.value != btn.hasClass("active")) {
          btn.trigger("click");
        };
      };
    };
    
    if(data.hasOwnProperty("style")) {
      btn
      .removeClass("btn-default btn-primary btn-success btn-info btn-warning btn-danger btn-link")
      .addClass("btn-" + data.style);
    };
    
    if(data.hasOwnProperty("size")) {
      btn.removeClass("btn-lg btn-sm btn-xs")
      if(data.size != "default") {
        btn.addClass(data.size);
      };
    };
    
    if(data.hasOwnProperty("block")) {
      btn.toggleClass("btn-block", data.block);
    };
    
    if(data.hasOwnProperty("disabled")) {
      if(data.disabled) {
        btn.attr("disabled", "disabled")
      } else {
        btn.attr("disabled", false)
      };
    };
    
  })$( document ).ready(function() {
    
  });
  
              ')
  tags$head(tags$style(
    ".sbs-alert-hover {
      position: absolute;
      z-index: 1000;
      width: 30%;
      margin-left: 35%;
      margin-top: 10px;
      opacity: 0.7;
    }
    
    .sbs-alert-hover:hover {
      opacity: 1.0;
    }"
  ))
  
  IP <- reactive({ input$getIP })
  
  
  observe({
    cat(capture.output(str(IP()), split=TRUE))
  })
  
  
  # autoInvalidate <- reactiveTimer(4000)
  # observe({
  #   autoInvalidate()
  #   cat(".")
  # })
  # 
  r <- reactiveValues(origine = TRUE)
  
  
  
  observeEvent(input$quit,{
    
    showNotification("Merci de votre visite.")
    # invalidateLater(2000,session)
    # stopApp()
  })

  observeEvent(r,once=TRUE,{
    r <- r_ini(r)  
    
    if(! file.exists(file.path(r$dir,"parcelles.rds"))){
      callModule(mod_admin_acces_server,"mod")
      showModal(mod_admin_acces_ui("mod"))
    }else{
      
      co <- as.character(unique(r$par$nom_com)[order(unique(r$par$nom_com))])
      comi <- strsplit(r$admin$parcelle_ini,"_")[[1]][1]
      p <- r$par %>% filter(nom_com == comi)
      
      updateSelectInput(session,"select_commune",choices = co,selected = comi)  
      updateSelectInput(session,"select_section",choices = unique(p$section)[order(unique(p$section))] ,
                        selected = strsplit(r$admin$parcelle_ini,"_")[[1]][2])  
      updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))],
                        selected = strsplit(r$admin$parcelle_ini,"_")[[1]][3])  

      # authentification --------------------------------------------------------
      if(demo){
        
        cat(paste("\n\n........Mode DEMO............"))
        
        r$user <-  "Moi"
        r$dir <- paste0(r$dir,"tmp_",sample.int(10000000000,1))
        saveRDS(r$dir,"path.rds")
        dir.create(file.path(r$dir))
        file.copy(file.path(path,"db.sqlite"),r$dir)
        file.copy(file.path(path,"admin.rds"),r$dir)
        file.copy(file.path(path,"parcelles.rds"),r$dir)
        showNotification("Vous êtes en mode démonstration. Votre nom de propriétaire est 'Moi'",
                         duration = 10, type = "message")

        r <- update_data(r,input,output,session,ini=TRUE)

      }else{
        cat(paste("\n\nUtilisateur:...............",r$user))
        
        callModule(mod_accueil_server,"mod",r)
        showModal(mod_accueil_ui("mod",r$admin$commune,r))
      }
    }
    
  })

  output$nom_commune <- renderUI({
    paste("Bourse foncière forestière communale de ", r$admin$commune)
  })
  
  # choix parcelle ----------------------------------------------------------
  
  observeEvent(input$select_commune,{
    req(r$update);
    req(input$select_commune!="")
    message("event input$select_commune ...")
    p <- r$par %>% filter(nom_com == input$select_commune)
    updateSelectInput(session,"select_section",choices = unique(p$section)[order(unique(p$section))] )  
    updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))])  
    
  })
  
  observeEvent(input$select_section,{
    req(r$update);
    req(input$select_section!="")
    message("event input$select_section  ...")
    
    p <- r$par %>% filter(nom_com == input$select_commune &
                            section == input$select_section)    
    updateSelectInput(session,"select_parcelle",choices = unique(p$numero)[order(unique(p$numero))])  
  })
  
  observeEvent(input$select_parcelle,{
    req(r$update);
    req(input$select_parcelle!="")
    message("event input$select_parcelle ...")
    # 
    # parcelle <- paste0(input$select_commune,"_",input$select_section,"_",input$select_parcelle)
    # if(parcelle %in% r$data$parcelle){
    #   
    #   r$parcelle <- get_parcelle(r,parcelle)
    # }
  })
  
  # carte -------------------------------------------------------------------
  
  
  output$map <- renderLeaflet({
    r$map
  })
  
  observeEvent(input$map_marker_click,{
    req(is.null(r$click))
    print("------------marker click-------------")
    r$click <- 1
    update <- FALSE
    pt <- st_as_sf(data.frame(x=input$map_marker_click$lng,y=input$map_marker_click$lat),
                   coords = c("x","y"), crs=4326)
    parcelle <- r$data$parcelle[st_intersects(pt,r$data)[[1]]]
    r$parcelle <- get_parcelle(r,parcelle)
    r$click <- NULL
    update <- TRUE
  })
  
  observeEvent(input$map_shape_click,{
    req(is.null(r$click))
    print("------------shape click-------------")
    
    
    r$click <- 1
    
    update <- FALSE
    parcelle <- input$map_shape_click$id
    r$parcelle <- get_parcelle(r,parcelle)
    r$click <- NULL
    update <- TRUE
  })
  
  observeEvent(input$zoom,{
    bb <- zoom_parcelle(input,output,session,r)
    
    leafletProxy("map",session) %>%
      fitBounds(bb[1]-0.0003,bb[4]-0.0003,bb[3]+0.0003,bb[2]+0.0003)
  })
  
  output$surface <- renderUI({
    
    req(length(r$parcelle)>0) 
    su <- try(sum(as.numeric(
      r$par %>% filter(parcelle %in% r$parcelle) %>% pull(surface)
    )))
    
    if(class(su) == "try-error") su <- ""
    HTML(paste0(
      
      "Surface: <b>",
      #   "<span style = 'display:inline-table;padding-left:20px;font-size:14px; color: blue'>",
      su,
      " ha</b>"))
    
  })
  
  
  # changement de parcelle --------------------------------------------------
  
  observeEvent(r$parcelle,{
    
    message("------------------------------------------------------")
    message("parcelle:--------------",r$parcelle,"----------")
    message("------------------------------------------------------")
    
    req(r$parcelle != "__");req(r$data);req(r$user)
    freezeReactiveValue(input, "is_proprio")
    freezeReactiveValue(input, "choix_proprio")
    freezeReactiveValue(input, "groupe")
    freezeReactiveValue(input, "is_interesse")
    freezeReactiveValue(input, "coeff_val")
    
    message("event r$parcelle ...")
    
    r$update <- FALSE
    pc <- unlist(str_split(r$parcelle,"_"))
    
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    bb <- as.numeric(st_bbox(p))
    
    leafletProxy("map",session) %>%
      clearGroup("parcelle_active")%>%
      addPolygons(data=p,color="red",fill=FALSE,group = "parcelle_active") %>% 
      fitBounds(bb[1]-0.0003,bb[2]-0.0003,bb[3]+0.0003,bb[4]+0.0003)
    
    pro <- r$data %>%
      filter(parcelle %in% r$parcelle)
    
    if(pro$proprietaire[1]=="?"){
      
      # parcelle sans propriétaire
      
      isolate(updateRadioGroupButtons(session,"is_proprio",selected = "non"))
      (updateNumericInput(session,"coeff_val",value = 1))
      shinyjs::disable("coeff_val")
    }else if(pro$proprietaire[1] != r$user){
      
      # parcelle propriété d'un autre uilisteur
      
      output$info_autre_proprio <- renderUI({
        if(pro$echangeable[1] & pro$a_vendre[1]) x <- "la vendre"
        if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "l'echanger"
        if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "la conserver"
        HTML(paste0(
          "Cette parcelle a été déclarée par un autre propriétaire. Il soutaite ",
          "<a style='color=red'>",x,"</a>"
        ))
        
        updateNumericInput(session,"coeff_val",value =  round(weighted.mean(pro$coeff_valeur, pro$surface),1))
        shinyjs::disable("coeff_val")
        
      })
      
      inter <- read_interet() %>% 
        filter(parcelle %in% r$parcelle & interet == r$user)
      
      if(nrow(inter)>0){
        ii <- "oui"
      }else{
        ii <- "non"
      }
      isolate(updateRadioGroupButtons(session,"is_interesse",selected = ii))
      (updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
      shinyjs::disable("coeff_val")
      
    }else{
      
      # proriété de l'utilisteur
      
      if(pro$echangeable[1] & pro$a_vendre[1]) x <- "vend"
      if(pro$echangeable[1] & !pro$a_vendre[1]) x <- "echange"
      if(!pro$echangeable[1] & !pro$a_vendre[1]) x <- "garde"
      showElement("est_proprio")
      
      isolate(updateRadioGroupButtons(session,"is_proprio",selected = 'oui'))
      isolate(updateRadioGroupButtons(session,"choix_proprio",selected = x))
      (updateNumericInput(session,"coeff_val",value = pro$coeff_valeur[1]))
      shinyjs::enable("coeff_val")
      
      showElement("proprio")
      hideElement("autre_proprio")
    }
    
    affiche_controles (r,input,output,session)
    
    message("event r$parcelle OK")
    r$update <- TRUE
  })
  
  output$commentaires_parcelle <- renderUI({
    req(r$parcelle);req(r$data)
    p <- r$data %>% filter(parcelle %in% r$parcelle)
    
    if(nrow(p)==0){
      HTML("")
    }else if(all(p$proprietaire[1] == "?")){
      HTML("Personne n'a encore inscrit cette parcelle")
    } else if(all(p$proprietaire == r$user)){
      int <- unique(read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet))
      if(length(int)==0) {
        i <- "Personne n'a déclaré être intéressé"
      }else if(length(int)==1){
        i <- paste(int,"est intéressé")
      }else{
        i <- paste(paste(int,collapse = ", "),"sont intéressés")
      }
      HTML(paste("<a>Vous êtes propriétaire.</a>",i))
    } else {
      if(p$echangeable[1] & p$a_vendre[1]) x <- "la vendre"
      if(p$echangeable[1] & !p$a_vendre[1]) x <- "l'echanger"
      if(!p$echangeable[1] & !p$a_vendre[1]) x <- "la conserver"
      
      HTML(paste0(
        "Cette parcelle a été déclarée par ",p$proprietaire[1],". Il soutaite ",
        "<a style='color=red'>",x,"</a>"
      ))
    }
  })
  
  output$is_prise <- renderUI({
    req(r$parcelle);req(r$data)
    
    int <- unique(read_interet() %>% filter(parcelle %in% r$parcelle) %>% pull(interet))
    
    if(length(int) == 0){
      HTML("Aucun propriétaire encore intéressé")
    }else if(r$user %in% int){
      if(length(int)==1) {
        HTML("Vous êtes le seul propriétaire intéressé")
      }else{
        HTML(paste(paste(int[int != r$user],collapse = ", "),"et vous-même êtes intéressés"))
      }
    }else{
      HTML(paste(length(int)),"propriétaire(s) intéressé(s")
    }
    
    
  })
  
  # affectation des parcelles mode proprietaire-----------------------------------------------
  
  observeEvent(input$is_proprio,ignoreNULL = TRUE,ignoreInit = F,handlerExpr = {
    
    req(r$update);
    req(r$parcelle)
    
    message("------------------------------------------------------")
    message("est propriétaire:--------------",input$is_proprio,"----------")
    message("------------------------------------------------------")
    
    r$update <- FALSE
    
    message("event input$is_proprio ...")
    # pro <- read_proprietaire()
    
    if(input$is_proprio[1] == "oui"){
      
      # je suis propriétaire ........................................
      
      shinyjs::enable("coeff_val")
      if(all(r$data$proprietaire[r$data$parcelle %in% r$parcelle] == r$user)){
        
        # je l'avais déjà déclaré ................................
        
      }else{
        
        # je me déclare propriétaire ................................
        
        r <- modif_parcelle(r,r$user,FALSE,FALSE,1,NA)
        
        isolate(updateRadioGroupButtons(session,"choix_proprio",
                                        selected = "garde"))
        
      }
      write_proprietaire(r)
      
      r$bounds <- input$map_bounds
      r$map_groups <- input$map_groups
      
      r <- update_data(r,input,output,session,FALSE)
      affiche_controles (r,input,output,session)
    
    }else{
      
      # je ne suis pas propriétaire .................................
      
      int <- read_interet() %>% filter(parcelle %in% r$parcelle)
      if(nrow(int)>0 & input$is_proprio[1] == "non"){
        
        # non, je me suis trompé, mais entre temps, d'autres se sont déclarés intéressés .....................................
        
        notification(int)
        showNotification(paste0("Des propriétaires se sont déclarés intéressés par cette parcelle. Ils recevront une notification les informant du changement."),
                         type = "warning",duration = 20)
        
        int <- read_interet() %>% 
          filter(!parcelle %in% r$parcelle)
        write_interet(int)
      }
      
      # non, je me suis trompé, ùais c'est sans conséquence .....................................
      
      r <- modif_parcelle(r,"?",FALSE,FALSE,1,NA)
      
      write_proprietaire(r)
      r$bounds <- input$map_bounds
      r$map_groups <- input$map_groups
      
      r <- update_data(r,input,output,session,FALSE)
      affiche_controles (r,input,output,session)
      
    }    
    r$update <- TRUE
  })
  
  observeEvent(input$choix_proprio,ignoreInit = FALSE,handlerExpr = {
    
    req(r$update);
    req(r$parcelle)
    r$update <- FALSE
    message("------------------------------------------------------")
    message("choix propriétaire:--------------",input$choix_proprio,"----------")
    message("------------------------------------------------------")
    
    message("event input$choix_proprio ...")
    freezeReactiveValue(input, "groupe")
    
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) == r$user)
    
    pro <- pro1 <- read_proprietaire()
    
    if(input$choix_proprio[1] == "vend"){
      r <- modif_parcelle(r,ech=TRUE,ven = TRUE)
    }
    if(input$choix_proprio[1] == "echange"){
      r <- modif_parcelle(r,ech=TRUE,ven = FALSE)
    }
    if(input$choix_proprio[1] == "garde"){
      r <- modif_parcelle(r,ech=FALSE,ven = FALSE)
    }
    
    int <- read_interet() %>% filter(parcelle %in% r$parcelle)
    pro_avant <- read_proprietaire()
    
    if(nrow(int)>0 &
       any(r$data$echangeable[r$data$parcelle %in% r$parcelle] != pro_avant$echangeable[r$data$parcelle %in% r$parcelle] |
        r$data$a_vendre[r$data$parcelle %in% r$parcelle] != pro_avant$a_vendre[r$data$parcelle %in% r$parcelle]
       )){
      notification(int)      
      showNotification(paste0("Des propriétaires se sont déclarés intéressés par cette parcelle. Ils recevront une notification les informant du changement."),
                       type = "warning")
      
      int <- read_interet() %>% 
        filter(!parcelle %in% r$parcelle)
      write_interet(int)
    }
    write_proprietaire(r)
    r$bounds <- input$map_bounds
    r <- update_data(r,input,output,session,FALSE)
    
    affiche_controles (r,input,output,session)
    r$update <- TRUE
    
  })
  
  observeEvent(input$is_interesse,ignoreNULL = TRUE,handlerExpr = {
    req(r$update);
    req(r$parcelle)
    
    r$update <- FALSE
    message("------------------------------------------------------")
    message("est interesse:--------------",input$is_interesse,"----------")
    message("------------------------------------------------------")
    message("event input$is_interesse ...")
    int <- read_interet()
    # freezeReactiveValue(input, "groupe")
    
    if(input$is_interesse[1] == "oui"){
      int <- rbind(int,
                   data.frame(parcelle = r$parcelle,
                              interet = rep(r$user,length(r$parcelle)),
                              stringsAsFactors = FALSE))
      
    }else{
      int <- int %>% filter(!(int$parcelle %in% r$parcelle & int$interet == r$user))
      
    }
    write_interet(int)
    r$bounds <- input$map_bounds
    r$map_groups <- input$map_groups
    
    r <- update_data(r,input,output,session,FALSE)
    
    affiche_controles (r,input,output,session)
    r$update <- TRUE
    
  })
  
  observeEvent(input$coeff_val,{
    
    req(r$update);
    req(r$parcelle)
    # invalidateLater(100)
    message("------------------------------------------------------")
    message("coeff valeur:--------------",input$coeff_val,"----------")
    message("------------------------------------------------------")
    
    message("event input$coeff_val ...")
    req( r$data %>% filter(parcelle %in% r$parcelle) %>% pull(proprietaire) != "?")
    freezeReactiveValue(input, "groupe")
    
    r <- modif_parcelle(r, coef = input$coeff_val)
    write_proprietaire(r)
    r <- update_data(r,input,output,session,FALSE)
    message("event input$coeff_val OK")
    
  })
  
  # groupes -----------------------------------------------------------------
  
  output$info_groupe <- renderUI({
    gr <- unique(r$data$parcelle_groupe[r$data$parcelle %in% r$parcelle])
    if(length(gr)==0){
      HTML("Aucune parcelle sélectionnée")
    }else{#} if(is.na(gr)){
      
      p <- r$data %>% filter(parcelle %in% r$parcelle)
      pp <- paste(p$nom_com,p$etiquette)
      HTML(
        paste0(
          "<b style='font-size:20px;color:blue'>",
          paste(pp,collapse = "<br>"),
          "</b>")
      )
    }
  })
  
  
  observeEvent(input$groupe,ignoreInit = TRUE,handlerExpr = {
    # req(r$update);
    # req(r$parcelle)
    # groupe_parcelles(r,input$groupe)
    # r <- update_data(r,input,output,session,FALSE)
    # p <- r$data %>% filter(parcelle %in% r$parcelle)
    
    # req(r$user == p$proprietaire)
    
    message("------------------------------------------------------")
    message("-------------------acces groupe------------------------")
    message("------------------------------------------------------")
    
    pp <- read_proprietaire() %>% filter(proprietaire == unique(r$user))
    ppu <- pp %>% filter(is.na(parcelle_groupe))
    ppg <- pp %>% filter(!is.na(parcelle_groupe))
    
    r$choices_ppu <- str_replace_all(ppu$parcelle,"_"," ")
    r$choices_ppu <- r$choices_ppu[order(r$choices_ppu)]
    
    r$choices_ppg <- str_replace_all(ppg$parcelle_groupe,"__"," + ")
    r$choices_ppg <- str_replace_all(r$choices_ppg ,"_"," ")
    r$choices_ppg <- r$choices_ppg[order(r$choices_ppg)]
    if(length(r$choices_ppg)==0){
      r$choices_ppg <- "aucun groupe n'a encore été créé"
    }else{
      r$choices_ppg <- unique(r$choices_ppg)
    }
    
    callModule(mod_groupe_server, "mod",r)
    showModal(
      mod_groupe_ui("mod",r)
    )
  })
  
  # bilan -------------------------------------------------------------------
  
  observeEvent(input$rendu_bilan,ignoreInit = T,handlerExpr = ,{
    
    message("------------------------------------------------------")
    message("-------------------acces bilan------------------------")
    message("------------------------------------------------------")
    r$b <- bilan_proprietaire(r$data,r$user)
    ide <- read_identite() %>% arrange(proprietaire)
    propri <- ide$proprietaire[-1]
    
    callModule(mod_bilan_proprio_server, "mod",r)
    showModal(
      mod_bilan_proprio_ui("mod")
    )
  })
  
  
  # contact -----------------------------------------------------------------
  
  observeEvent(input$contact,{
    
    
    callModule(mod_contact_proprio_server, "mod",r)
    showModal(
      mod_contact_proprio_ui("mod")
    )
    
  })
  
  
  # aide --------------------------------------------------------------------
  
  observeEvent(input$help,{
    showModal(
      modalDialog(
        includeMarkdown(file.path(system.file(package = "boursefonciereforestiere"),"aide","manuel_utilisateur.md")),
        footer = tagList(modalButton("Quitter l'aide")),
        size="l",easyClose=TRUE)
    )
  })
  
  
  # output$keepAlive <- renderText({
  #   invalidateLater(4000)
  #   r$timer <- r$timer + 1
  #   r$timer
  # })   
}
