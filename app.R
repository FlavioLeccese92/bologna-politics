library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(htmltools)
library(scales)
library(reactablefmtr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(viridis)
library(waiter)
library(knitr)
library(shinyjs)
##
library(echarts4r)
library(tidyr)
library(jsonlite)
# library(dqshiny)


# try({dev.off()})
try({pdf(NULL)})

addResourcePath("app_www", paste0(getwd(), "/app_www"))
addResourcePath("data", paste0(getwd(), "/data"))

github_prefix = "https://raw.githubusercontent.com/FlavioLeccese92/bologna-politics/main/"

### path tables ###
tree_table = readRDS("data/general-porpuse/tree_table.rds")
nome_lista_conciliazione = readRDS("data/general-porpuse/nome_lista_conciliazione.rds")
contrassegni = readRDS("data/general-porpuse/contrassegni.rds")

### geographical hierarchies ###
quartieri = readRDS("data/general-porpuse/quartieri.rds")
zone = readRDS("data/general-porpuse/zone.rds")
aree_statistiche = readRDS("data/general-porpuse/aree_statistiche.rds")
sezioni = readRDS("data/general-porpuse/sezioni.rds")
civici_sezioni = readRDS("data/general-porpuse/civici_sezioni.rds")

### demographic data ###
pop_z_eta_sesso_civile  = readRDS("data/general-porpuse/pop_z_eta_sesso_civile.rds")

### polygons ###
sezioni_polygons = readRDS("data/polygons/sezioni_polygons.rds")

### palette ###
red = "#ef5350"
green = "#27b376"
yellow = "#ffca28"
blue = "#0099F9"
darkblue = "#15354a"

nome_lista_conciliazione = nome_lista_conciliazione %>%
  mutate(id_lista = case_when(nome_lista_eligendo %in%
                                  c("PARTITO DEMOCRATICO",
                                    "DEMOCRATICI SINISTRA",
                                    "L'ULIVO",
                                    "PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA") ~ "PD",
                                grepl("5 STELLE", nome_lista_eligendo) ~ "M5S",
                                grepl("\\+EUROPA|PIU EUROPA", nome_lista_eligendo) ~ "+EU",
                                grepl("FORZA ITALIA", nome_lista_eligendo) ~ "FI",
                                nome_lista_eligendo == "ALLEANZA NAZIONALE" ~ "AN",
                                nome_lista_eligendo == "IL POPOLO DELLA FAMIGLIA" ~ "PDF",
                                nome_lista_eligendo == "COMUNISTI ITALIANI" ~ "PDCI",
                                nome_lista_eligendo == "RIFONDAZIONE COMUNISTA" ~ "RFC",
                                nome_lista_eligendo %in%
                                  c("PARTITO COMUNISTA DEI LAVORATORI",
                                    "PER UNA SINISTRA RIVOLUZIONARIA") ~ "PCL",
                                nome_lista_eligendo == "PARTITO COMUNISTA" ~ "PCI",
                                nome_lista_eligendo == "FORZA NUOVA" ~ "FN",
                                grepl("L'ARCOBALENO|SINISTRA ECOLOGIA LIBERTA|LIBERI E UGUALI|VERDI E SINISTRA", nome_lista_eligendo) ~ "SI",
                                grepl("COALIZIONE CIVICA", nome_lista_eligendo) ~ "CC",
                                grepl("LEGA", nome_lista_eligendo) ~ "LN",
                                grepl("FRATELLI D'ITALIA", nome_lista_eligendo) ~ "FDI",
                                grepl("UNIONE POPOLARE|POTERE AL POPOLO", nome_lista_eligendo) ~ "PAP",
                                grepl("PSDI|PARTITO SOCIALISTA", nome_lista_eligendo) ~ "PSI",
                                grepl("POPOLO DELLA LIBERTA", nome_lista_eligendo) ~ "FI",
                                grepl("UNIONE DI CENTRO|UDC", nome_lista_eligendo) ~ "UDC",
                                grepl("LIBERALE", nome_lista_eligendo) ~ "PLI",
                                grepl("ITALIA DEI VALORI|DIPIETRO", nome_lista_eligendo) ~ "IDV",
                                grepl("ALTERNATIVA SOCIALE", nome_lista_eligendo) ~ "AS",
                                grepl("UDEUR|U\\.D\\.EUR", nome_lista_eligendo) ~ "UDEUR",
                                grepl("PARTITO REPUBBLICANO ITALIANO|PRI", nome_lista_eligendo) ~ "PRI",
                                grepl("CASAPOUND", nome_lista_eligendo) ~ "CPI",
                                grepl("LA DESTRA|DESTRE", nome_lista_eligendo) ~ "DDX",
                                grepl("FIAMMA TRICOLORE", nome_lista_eligendo) ~ "F3C",
                                grepl("VERD", nome_lista_eligendo) ~ "VRD",
                                grepl("PSDI", nome_lista_eligendo) ~ "PSDI",
                                grepl("europee", key) ~ nome_lista_eligendo,
                                TRUE ~ "-")) %>%
  mutate(id_coalizione = case_when(nome_coalizione_eligendo %in%
                                     c("GUAZZALOCA GIORGIO",
                                       "BERLUSCONI SILVIO",
                                       "SILVIO BERLUSCONI",
                                       "CAZZOLA ALFREDO",
                                       "SCARANO PAOLA FRANCESCA / ROCCELLA EUGENIA MARIA",
                                       "BRUNELLI ELISABETTA",
                                       "BERNARDINI MANES",
                                       "BORGONZONI LUCIA",
                                       "BATTISTINI FABIO",
                                       "ANSALONE DALILA JOLANDA",
                                       "SGARBI VITTORIO"
                                     ) ~ "CDX",
                                   nome_coalizione_eligendo %in%
                                     c("WALTER VELTRONI",
                                       "PRODI ROMANO",
                                       "DELBONO FLAVIO",
                                       "COFFERATI SERGIO GAETANO",
                                       "PIER LUIGI BERSANI",
                                       "MEROLA VIRGINIO",
                                       "DE MARIA ANDREA / BENAMATI GIANLUCA",
                                       "CASINI PIER FERDINANDO",
                                       "BONACCINI STEFANO",
                                       "LEPORE MATTEO") ~ "CSX",
                                   nome_coalizione_eligendo %in%
                                     c("FAUSTO BERTINOTTI",
                                       "ERRANI VASCO",
                                       "FALCONE ANNA / BONORA PAOLA") ~ "SI",
                                   nome_coalizione_eligendo %in%
                                     c("OLADEJO OLAWALE JELILI / VIA MARIARACHELE",
                                       "COLLOT MARTA",
                                       "FORTUZZI FRANCESCA",
                                       "FERLINI SIMONA") ~ "PAP",
                                   nome_coalizione_eligendo %in%
                                     c("MARIO MONTI",
                                       "UNIONE DI CENTRO",
                                       "PIER FERDINANDO CASINI")  ~ "CCX",
                                   TRUE ~ id_lista
  ))

source("app_functions/functions.R")

ui = fluidPage(
    useShinyjs(),
    # useWaiter(),
    autoWaiter(html = div(class = "loading", tags$i(), tags$i(), tags$i(), tags$i())),
    includeCSS("app_www/styles.css"),
    includeCSS("app_www/loader.css"),
    tags$head(tags$script(src = "app_www/methods.js"),
              tags$script(src = "app_www/script.js")),
    tags$link(href="https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&amp;display=swap", rel="stylesheet"),
    tags$main(class = "dashboard",
              tags$header(class = "dashboard-header",
                          tags$h2(class = "dashboard-heading",
                                  tags$a(href="https://shiny.rstudio.com/",
                                         class="logo logo-shiny", target="_blank",
                                         rel="nofollow noreferrer", `aria-label`="RStudio Shiny Website",
                                         tags$svg(class="logo-svg", viewBox="0 0 100 68",
                                                  tags$use(href="app_www/icons-sprite-map.svg#shiny-logo"))),
                                  tags$span(class="dashboard-title", "Bologna Politics"))#,
                          ),
              tags$section(class = "dashboard-panels",
                           tags$div(class = "dashboard-filters-vertical",
                                             selectizeInput("gen_input_votazione", "Elezioni",
                                                            choices = list("<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='app_www/bandiera-comune-bologna.png' width=30px>Comunali</img></div>" = "comunali",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='app_www/bandiera-emilia-romagna.png' width=30px>Regionali</img></div>" = "regionali",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='app_www/bandiera-italia.png'         width=30px>Politiche</img></div>" = "politiche",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='app_www/bandiera-unione-europea.png' width=30px>Europee</img></div>" = "europee"
                                                            ),
                                                            selected = "politiche",
                                                            options = list(
                                                              render = I("{item: function(item) { return item.label; },
                                                    option: function(item) { return item.label; }}"))
                                             ),
                                             selectizeInput("gen_input_anno", "Anno", "2022"),
                                             selectizeInput("gen_input_organo", "Organo", "camera")
                           ),
                           uiOutput("sum_affluenza"),
                           uiOutput("sum_partito"),
                           uiOutput("sum_coalizione")
                           ),
              tags$section(class = "dashboard-panels",
                           tags$div(class = "dashboard-filters-vertical",
                                    selectizeInput("gen_level", "Livello",
                                                   c("Quartieri", "Zone", "Aree statistiche", "Indirizzi"), "Quartieri"),
                                    autocomplete_input("gen_level_value", "Valore", value = "",
                                                       placeholder = "Tutto",
                                                       c("", quartieri$nome_quartiere %>% sort()),
                                                       contains = TRUE,
                                                       max_options = 500)),
                           div(), div(), div()),
              tags$section(class = "dashboard-panels-2col",
                           reactableOutput("table_partiti"),
                           leafletOutput("map_liste")
                           ),
              tags$section(class = "dashboard-panels panel-300",
                           uiOutput("pop_maschi"),
                           uiOutput("pop_femmine")
              )

    )
)

server = function(input, output, session) {

  r = reactiveValues()

  observeEvent(input$gen_input_votazione, {
    # waiter_show(html = div(class = "loading", tags$i(), tags$i(), tags$i(), tags$i()))

    organo = tree_table %>% filter(votazione == input$gen_input_votazione) %>%
      pull(organo) %>% unique()
    names(organo) = organo %>% gsub("_", " ", .) %>% str_to_title()
      updateSelectizeInput(session = session, "gen_input_organo", choices = organo, selected = organo[1])

    anno = tree_table %>% filter(votazione == input$gen_input_votazione) %>%
      pull(anno) %>% unique()
    updateSelectizeInput(session = session, "gen_input_anno", choices = anno, selected = anno[1])

    # waiter_hide()
  })

  observeEvent(c(input$gen_input_anno, input$gen_input_organo), {
    # waiter_show(html = div(class = "loading", tags$i(), tags$i(), tags$i(), tags$i()))

    r$tree_table =
      tree_table %>%
      filter(votazione == input$gen_input_votazione,
             anno == input$gen_input_anno,
             organo == input$gen_input_organo) %>%
      mutate(temp_html = gsub("\\./", "", temp_html),
             temp_files = gsub("\\./", "", temp_files),
             temp_rds = gsub("\\./", "", temp_rds),
             temp_affluenza_rds = gsub("\\./", "", temp_affluenza_rds)
             ) %>% {if (nrow(.) == 0) NULL else .}

    r$nome_lista_conciliazione =
      nome_lista_conciliazione %>%
      filter(key == r$tree_table$key, organo == input$gen_input_organo)

    r$contrassegni =
      contrassegni %>%
      filter(key == r$tree_table$key, organo == input$gen_input_organo)

    r$tree_table_prev =
      tree_table %>%
      filter(votazione == input$gen_input_votazione,
             organo == input$gen_input_organo) %>%
      arrange(anno) %>%
      filter(lead(anno == input$gen_input_anno)) %>%
      mutate(temp_html = gsub("\\./", "", temp_html),
             temp_files = gsub("\\./", "", temp_files),
             temp_rds = gsub("\\./", "", temp_rds),
             temp_affluenza_rds = gsub("\\./", "", temp_affluenza_rds)
      ) %>% {if (nrow(.) == 0) NULL else .}

    r$nome_lista_conciliazione_prev =
      nome_lista_conciliazione %>%
      {if (is.null(r$tree_table_prev$key)) NULL else
        filter(., key == r$tree_table_prev$key, organo == input$gen_input_organo)}

    r$gerarchia_sezioni = sezioni %>%
      left_join(aree_statistiche, by = "id_area_statistica") %>%
      left_join(zone, by = "id_zona") %>%
      left_join(quartieri, by = "id_quartiere")

    r$gerarchia_indirizzi = civici_sezioni %>%
      left_join(aree_statistiche, by = "id_area_statistica") %>%
      left_join(zone, by = "id_zona") %>%
      left_join(quartieri, by = "id_quartiere") %>%
      select(id_civico, indirizzo, id_area_statistica, nome_area_statistica,
             id_zona, nome_zona, id_quartiere, nome_quartiere, id_sezione)

    # waiter_hide()
    })

  observeEvent(input$gen_level, {
    req(r$gerarchia_indirizzi)
    options = r$gerarchia_indirizzi %>%
      {if(input$gen_level == "Quartieri") distinct(., nome_quartiere) %>% pull() %>% na.omit() %>% c("", .) else
        if(input$gen_level == "Zone") distinct(., nome_zona) %>% pull() %>% na.omit() %>% c("", .)  else
          if(input$gen_level == "Aree statistiche") distinct(., nome_area_statistica) %>% pull() %>% na.omit() %>% c("", .) else
            if(input$gen_level == "Indirizzi") distinct(., indirizzo) %>% pull() %>% na.omit() %>% c("", .) else NULL
      }
    update_autocomplete_input(session = session, id = "gen_level_value", label = "Valore", options = options, value = "")
  })

  ####--- Sezione ---####
  sezione = reactive({
    (if(nchar(input$gen_level_value)>0){
      r$gerarchia_indirizzi %>%
        {if(input$gen_level == "Quartieri") filter(., nome_quartiere == input$gen_level_value) else
          if(input$gen_level == "Zone") filter(., nome_zona == input$gen_level_value) else
            if(input$gen_level == "Aree statistiche") filter(., nome_area_statistica == input$gen_level_value) else
              if(input$gen_level == "Indirizzi") filter(., indirizzo == input$gen_level_value) else .} %>%
        distinct(., id_sezione) %>% pull(id_sezione)
    } else {NULL})
  })

  ####--- gerarchia_indirizzi ---####
  gerarchia_indirizzi = reactive({
    (if(nchar(input$gen_level_value)>0){
      r$gerarchia_indirizzi %>%
        {if(input$gen_level == "Quartieri") filter(., nome_quartiere == input$gen_level_value) else
          if(input$gen_level == "Zone") filter(., nome_zona == input$gen_level_value) else
            if(input$gen_level == "Aree statistiche") filter(., nome_area_statistica == input$gen_level_value) else
              if(input$gen_level == "Indirizzi") filter(., indirizzo == input$gen_level_value) else .}
    } else {NULL})
  })

  ####--- Affluenza ---####

  affluenza = reactive({
    if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_affluenza_rds) %>%
        { if(length(sezione())>0) filter(., id_sezione %in% sezione()) else
          select(., everything()) }
      }else{NULL}
    })

  affluenza_prev = reactive({
    if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_affluenza_rds) %>%
        { if(length(sezione())>0) filter(., id_sezione %in% sezione()) else
          select(., everything()) }
    }else{NULL}
    })

  output$sum_affluenza = renderUI({
    metric = sum(affluenza()$totale_votanti)/sum(affluenza()$iscritti)

    detail1 = sum(affluenza()$totale_votanti)
    detail2 = sum(affluenza()$iscritti)

    delta_class = "positive-change"
    if(!is.null(affluenza_prev())){
      affluenza_prev = sum(affluenza_prev()$totale_votanti)/sum(affluenza_prev()$iscritti)
      delta = metric - affluenza_prev
      if(delta<0){delta_class = "negative-change"}
      delta = delta %>%
        label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
      delta_label = paste0("(cfr ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    panel_ui("Affluenza", metric, metric_type = "perc",
             "Totale voti", detail1,
             "Aventi diritto", detail2,
             delta, delta_label, delta_class,
             vec_src = "app_www/affluenza.svg#affluenza")

    })

  ####--- voti ---####

  voti_sezioni = reactive({
    (if(!is.null(r$tree_table)){
      readRDS(r$tree_table$temp_rds) %>%
        { if(length(sezione())>0) mutate(., sezione_selected = id_sezione %in% sezione()) else
          mutate(., sezione_selected = TRUE) } %>%
        left_join(r$nome_lista_conciliazione,
                  by = c("nome_lista" = "nome_lista_opendata")) %>%
        select(id_sezione,
               nome_lista = nome_lista_eligendo,
               voti_validi,
               nome_coalizione = nome_coalizione_eligendo,
               id_lista, id_coalizione,
               sezione_selected) %>%
        group_by(id_sezione, sezione_selected) %>%
        mutate(tot_voti_validi = sum(voti_validi)) %>%
        ungroup()
    }else{NULL})})

  voti_partito = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
        filter(sezione_selected) %>%
        group_by(nome_lista, id_lista, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  voti_coalizione = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
        filter(sezione_selected) %>%
        group_by(nome_coalizione, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop",
                  nome_lista = nome_lista %>% unique() %>% list()) %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  ####--- _prev ---####

  voti_sezioni_prev = reactive({
    (if(!is.null(r$tree_table_prev)){
      readRDS(r$tree_table_prev$temp_rds) %>%
        { if(length(sezione())>0) mutate(., sezione_selected = id_sezione %in% sezione()) else
          mutate(., sezione_selected = TRUE) } %>%
        left_join(r$nome_lista_conciliazione_prev,
                  by = c("nome_lista" = "nome_lista_opendata")) %>%
        select(id_sezione,
               nome_lista = nome_lista_eligendo,
               voti_validi,
               nome_coalizione = nome_coalizione_eligendo,
               id_lista, id_coalizione,
               sezione_selected) %>%
        group_by(id_sezione, sezione_selected) %>%
        mutate(tot_voti_validi = sum(voti_validi)) %>%
        ungroup()
    }else{NULL})})

  voti_partito_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
        filter(sezione_selected) %>%
        group_by(nome_lista, id_lista, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  voti_coalizione_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
        filter(sezione_selected) %>%
        group_by(nome_coalizione, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  ####--- Voti partito ---####

  output$sum_partito = renderUI({

    metric = voti_partito() %>% slice(1) %>% pull(nome_lista)

    detail1 = voti_partito() %>% slice(1) %>% pull(voti_perc)
    detail2 = voti_partito() %>% slice(1) %>% pull(voti_validi)

    delta_class = "positive-change"
    if(!is.null(voti_partito_prev())){

      delta_voti = voti_partito() %>% slice(1) %>% select(id_lista, voti_perc) %>%
        inner_join(voti_partito_prev() %>%
                     filter(id_lista != "-") %>%
                     select(id_lista, voti_perc_prev = voti_perc),
                   by = "id_lista")
      if(nrow(delta_voti)>0){
        delta = delta_voti$voti_perc - delta_voti$voti_perc_prev
      }
      if(delta<0){delta_class = "negative-change"}
      delta = delta %>%
        label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
      delta_label = paste0("(cfr ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    img_src = r$contrassegni %>% filter(nome_lista_eligendo == metric) %>%
      mutate(img_src = paste0("data/", key, "/", image)) %>% pull(img_src)

    panel_ui("Primo Partito", metric, metric_type = NULL,
             "Risultato", detail1,
             "Voti validi", detail2,
             delta, delta_label, delta_class,
             img_src)

  })

  ####--- Voti coalizione ---####

  output$sum_coalizione = renderUI({
    metric = voti_coalizione() %>% slice(1) %>% pull(id_coalizione)

    detail1 = voti_coalizione() %>% slice(1) %>% pull(voti_perc)
    detail2 = voti_coalizione() %>% slice(1) %>% pull(voti_validi)

    delta_class = "positive-change"
    if(!is.null(voti_coalizione_prev())){

      delta_voti = voti_coalizione() %>% slice(1) %>% select(id_coalizione, voti_perc) %>%
        inner_join(voti_coalizione_prev() %>%
                     filter(id_coalizione != "-") %>%
                     select(id_coalizione, voti_perc_prev = voti_perc),
                   by = "id_coalizione")
      if(nrow(delta_voti)>0){
        delta = delta_voti$voti_perc - delta_voti$voti_perc_prev
      }
      if(delta<0){delta_class = "negative-change"}
      delta = delta %>%
        label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
      delta_label = paste0("(cfr ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    nome_lista = voti_coalizione() %>% slice(1) %>% pull(nome_lista) %>% unlist()
    img_src = r$contrassegni %>% filter(nome_lista_eligendo %in% nome_lista) %>%
      mutate(img_src = paste0("data/", key, "/", image)) %>% pull(img_src)

    panel_ui("Prima Coalizione", metric, metric_type = NULL,
             "Risultato", detail1,
             "Voti validi", detail2,
             delta, delta_label, delta_class,
             img_src)

  })

  #### Statistiche ####

  ###--- pop_maschi ---###
  observe({

    output$pop_maschi = renderUI({
      temp = pop_z_eta_sesso_civile %>%
        filter(anno == input$gen_input_anno) %>%
        {if(!is.null(gerarchia_indirizzi()))
          filter(., id_zona %in% (gerarchia_indirizzi() %>% distinct(., id_zona) %>% pull(id_zona))) else .}

      plot = eta_bar_chart(temp,
                           orientation = "right", color = blue, MF = "Maschi",
                           height = "268px")

      div(class = "panel panel-metric", #style = "height: 300px;",
          div(style = "height: 32px; min-height: 0px; border-radius: 6px 6px 0px 0px;
                  padding: 0.4375rem 0.9375rem; background-color: rgb(21, 53, 74); color: white;",
              h4("Età maschi")),
          plot
      )
    })
  })

  ###--- pop_femmine ---###
  observe({
    output$pop_femmine = renderUI({
      temp = pop_z_eta_sesso_civile %>%
        filter(anno == input$gen_input_anno) %>%
        {if(!is.null(gerarchia_indirizzi()))
          filter(., id_zona %in% (gerarchia_indirizzi() %>% distinct(., id_zona) %>% pull(id_zona))) else .}

      plot = eta_bar_chart(temp,
                           orientation = "left", color = red, MF = "Femmine",
                           height = "268px")

      div(class = "panel panel-metric", #style = "height: 300px;",
          div(style = "height: 32px; min-height: 0px; border-radius: 6px 6px 0px 0px;
                padding: 0.4375rem 0.9375rem; background-color: rgb(21, 53, 74); color: white;",
              h4("Età femmine")),
          plot
      )
    })
  })

  ####--- map partiti sezioni ---####
  voti_sezioni_map = reactive({
    (if(!is.null(voti_sezioni()) && length(table_partiti_selected()) == 1){
      voti_sezioni() %>%
        {if (!is.null(table_partiti_selected()))
          filter(., nome_lista %in% (voti_partito() %>%
                   slice(table_partiti_selected()) %>%
                   pull(nome_lista))) else .} %>%
        group_by(id_sezione, id_lista, sezione_selected) %>%
        summarise(voti_validi = sum(voti_validi),
                  tot_voti_validi = first(tot_voti_validi),
                  risultato = voti_validi/tot_voti_validi, .groups = "drop") %>%
        {if (!is.null(voti_sezioni_prev()))
          left_join(., voti_sezioni_prev() %>%
                      filter(id_lista != "-") %>%
                      group_by(id_sezione, id_lista)  %>%
                      summarise(voti_validi_prev = sum(voti_validi),
                                tot_voti_validi_prev = first(tot_voti_validi),
                                risultato_prev = voti_validi_prev/tot_voti_validi_prev, .groups = "drop"),
                    by = c("id_sezione", "id_lista"))
          else mutate(., voti_validi_prev = NA, tot_voti_validi_prev = NA, risultato_prev = NA)} %>%
        {if(input$gen_level == "Quartieri") left_join(., r$gerarchia_indirizzi %>% distinct(id_sezione, nome_quartiere) %>%
                                                        rename(id_level = nome_quartiere), by = "id_sezione") else
          if(input$gen_level == "Zone") left_join(., r$gerarchia_indirizzi %>% distinct(id_sezione, nome_zona) %>%
                                                    rename(id_level = nome_zona), by = "id_sezione")  else
            if(input$gen_level == "Aree statistiche") left_join(., r$gerarchia_indirizzi %>% distinct(id_sezione, nome_area_statistica) %>%
                                                                  rename(id_level = nome_area_statistica), by = "id_sezione")else
              if(input$gen_level == "Indirizzi") mutate(., id_level = id_sezione) else NULL
        } %>%
        {if (input$gen_level != "Indirizzi")
          group_by(., id_level) %>%
            mutate(risultato = sum(voti_validi)/sum(tot_voti_validi),
                   risultato_prev = sum(voti_validi_prev)/sum(tot_voti_validi_prev)) %>%
            ungroup() else . } %>%
        mutate(delta = risultato - risultato_prev) %>%
        left_join(sezioni_polygons, by = "id_sezione") %>%
        st_set_geometry("geometry")
    }else{NULL})
  })

  output$map_liste = renderLeaflet({
    # waiter_show(html = div(class = "loading", tags$i(), tags$i(), tags$i(), tags$i()))

    map = leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 10, maxZoom = 15, className = "dark-map-tiles")) %>%
      setView(lng = 11.3493292, lat = 44.492245, zoom = 12) %>%
      setMaxBounds(lng1 = 11.12966 , lat1 = 44.32111 , lng2 = 11.53371 , lat2 = 44.6562) %>%
      addPolygons(data = sezioni_polygons %>% mutate(id_sezione = as.character(id_sezione)),
                  layerId = ~id_sezione,
                  weight = 0.5,
                  #opacity = 1, fillOpacity = 0.5,
                  smoothFactor = 0.1,
                  fillColor = "transparent", color = "transparent", label = "",
                  highlight = highlightOptions(color = "white", weight = 2.5,
                                               bringToFront = TRUE, sendToBack = TRUE)) %>%
      addBootstrapDependency() %>%
      addEasyButtonBar(
        easyButton(id = "map_liste_density_btn",
          icon = tags$svg(id = "map_liste_density_icon",
                          class = "icon", viewBox="0 0 31 30",
                          style = "width: 100%; height: 100%; fill: #fff",
                          tags$use(href="app_www/density.svg#density")),
          onClick = JS(easy_button_js)),
        easyButton(id = "map_liste_variation_btn",
          icon = tags$svg(id = "map_liste_variation_icon",
                          class = "icon", viewBox="0 0 31 30",
                          style = "width: 100%; height: 100%; fill: #15354a",
                          tags$use(href="app_www/variation.svg#variation")),
          onClick = JS(easy_button_js)))

    # waiter_hide()
    map
  })

  observeEvent(c(voti_sezioni_map(), input$map_liste_type), {
    map_data = voti_sezioni_map()

    if(all(is.na(map_data$delta))){
      shinyjs::disable("map_liste_variation_btn")
    }else{shinyjs::enable("map_liste_variation_btn")}

    if(all(is.na(map_data$delta)) && input$map_liste_type == "variation"){
      runjs('$("#map_liste_density_btn").click()')
    } else if(!is.null(map_data)){
      level_text = ifelse(input$gen_level == "Indirizzi", "Sezioni", input$gen_level)
      if(input$map_liste_type == "density"){
        ris_min = ceiling(100*min(map_data$risultato))/100; ris_max = floor(100*max(map_data$risultato))/100
        title = "Risultato %"; style_positive = "none"
        pal = colorNumeric(viridis_pal(option = "G", direction = -1)(10), c(ris_min, ris_max))

        map_data = map_data %>%
          mutate(val_col = risultato %>% ifelse(. <= ris_min, ris_min, .) %>% ifelse(. >= ris_max, ris_max, .)) %>%
          left_join(r$gerarchia_sezioni %>% distinct(id_sezione, id_seggio, indirizzo), by = "id_sezione") %>%
          mutate(label = paste0(
            "Sezione: <strong>", id_sezione, "</strong><br>",
            "Seggio: <strong>", id_seggio, "</strong><br>",
            "Indirizzo: <strong>", indirizzo, "</strong><br>",
            "Risultato: <strong>", 100*round(risultato, 4), "%</strong> (su ", level_text,")")) %>%
          mutate(id_sezione = as.character(id_sezione))  %>%
          mutate(highlight_col = ifelse(sezione_selected, 0.7, 0.2))

        }else{
          ris_max = ceiling(100*max(abs(map_data$delta)))/100; ris_min = -ris_max
          title = "Delta %"; style_positive = "plus"
          pal = colorNumeric(c(red, red, yellow, green, green), c(ris_min, ris_max))
          map_data = map_data %>%
            mutate(val_col = delta %>% ifelse(. <= ris_min, ris_min, .) %>% ifelse(. >= ris_max, ris_max, .)) %>%
            left_join(r$gerarchia_sezioni %>% distinct(id_sezione, id_seggio, indirizzo), by = "id_sezione") %>%
            mutate(label = paste0(
              "Sezione: <strong>", id_sezione, "</strong><br>",
              "Seggio: <strong>", id_seggio, "</strong><br>",
              "Indirizzo: <strong>", indirizzo, "</strong><br>",
              "Risultato: <strong>", 100*round(risultato, 4), "%</strong><br>",
              "Risultato precedente: <strong>", 100*round(risultato_prev, 4), "%</strong><br>",
              "Delta: <strong>", label_percent(accuracy = 0.01, style_positive = "plus")(delta), "%</strong> (su ", level_text, ")")) %>%
            mutate(highlight_col = ifelse(sezione_selected, 0.7, 0.2))
        }

      if(all(map_data$sezione_selected)){
        focus = c(xmin = 11.22966, xmax = 11.43371, ymin = 44.42111, ymax = 44.55620)
        }else{
          focus = map_data %>% filter(sezione_selected) %>% st_bbox()
        }

      seggio = r$gerarchia_sezioni %>%
        {if(length(sezione()>0)) filter(., id_sezione %in% sezione()) else . } %>%
        group_by(., indirizzo, latitude, longitude) %>%
        summarise(id_seggio = paste0(unique(id_seggio), collapse = ", "), .groups = "drop") %>%
        mutate(label = paste0(
                    "Seggio: <strong>", id_seggio, "</strong><br>",
                    "Indirizzo: <strong>", indirizzo, "</strong>")) %>%
        select(latitude, longitude, label)
      seggio$label = seggio$label %>% lapply(htmltools::HTML)

      leafletProxy("map_liste") %>%
        clearControls() %>%
        clearGroup(group = "Seggio") %>%
        flyToBounds(lng1 = focus[["xmin"]], lng2 = focus[["xmax"]], lat1 = focus[["ymin"]], lat2 = focus[["ymax"]]) %>%
        setShapeStyle(data = map_data, layerId = ~id_sezione, smoothFactor = 0.1,
                      weight = 0.5, opacity = ~highlight_col, fillOpacity = ~highlight_col,
                      color = ~pal(val_col), fillColor = ~pal(val_col),
                      options = list(
                        highlight = highlightOptions(color = "white", weight = 2.5, opacity = 1,
                                                     bringToFront = TRUE, sendToBack = TRUE))) %>%
        setShapeLabel(data = map_data, layerId = ~id_sezione, label = ~label) %>%
        addLegendNumeric(data = map_data, pal = pal, values = ~val_col, title = title,
                         orientation = 'horizontal', fillOpacity = 1, width = 150,
                         height = 20, position = 'bottomright',
                         numberFormat = label_percent(accuracy = 1, style_positive = style_positive)) %>%
        addMarkers(data = seggio, lat = ~latitude, lng = ~longitude, label = ~label, icon = marker_icon,
                   group = "Seggio") %>%
        addLayersControl(overlayGroups = c("Seggio"), options = layersControlOptions(collapsed = TRUE)) %>%
        {if (length(sezione())==0) hideGroup(., "Seggio") else  showGroup(., "Seggio")}
    }
    })

  ####--- table partiti ---####

  table_partiti_selected = reactive({
    getReactableState("table_partiti", "selected")
  })

  output$table_partiti = renderReactable({

    voti_partito() %>%
      {if (is.null(voti_partito_prev())) mutate(.,voti_perc_prev = NA) else
        left_join(., voti_partito_prev() %>%
                    filter(id_lista != "-") %>%
                    select(id_lista, voti_perc_prev = voti_perc),
                  by = "id_lista")} %>%
      left_join(r$contrassegni %>% rename(nome_lista = nome_lista_eligendo) %>%
                  mutate(img_src = paste0("data/", key, "/", image)),
                by = "nome_lista") %>%
      mutate(risultato_bar = voti_perc,
             delta = ifelse(!is.na(voti_perc_prev), voti_perc - voti_perc_prev, NA),
             delta_col = cut(ifelse(is.na(delta), 0, delta),
                             breaks = c(-1, -0.2, -0.1, -0.05, -0.01, -0.005, 0,
                                        0.005, 0.01, 0.05, 0.1, 0.2, 1),
                             labels = c(colorRampPalette(c(red, yellow))(6),
                                        colorRampPalette(c(yellow, green))(6))) %>% as.character(),
             delta_col = ifelse(is.na(delta), "transparent", delta_col)) %>%
      select(img_src, nome_lista, id_coalizione,
             voti_perc, risultato_bar, delta, delta_col, voti_validi) %>%
      reactable(.,
                pagination = FALSE, highlight = TRUE,
                style = list(fontSize = "0.9rem"),
                selection = "single", onClick = "select", defaultSelected = 1,
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = darkblue,
                                          color = "white", borderRadius = "6px"),
                  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
                ),
                columns = list(
                  .selection = colDef(show = FALSE),
                  img_src = colDef(name = "", width = 45, sticky = "left",
                                   cell = function(value) {
                                     img_src = image_uri(value)
                                     image = img(src = img_src, style = "height: 30px;", alt = "")
                                     div(style = "display: inline-block; width: 30px", image)
                  }),
                  nome_lista = colDef(name = "Lista", minWidth = 250,
                                      footer = "Total"
                  ),
                  id_coalizione = colDef(show = FALSE),
                  voti_perc = colDef(name = "Risultato",
                    format = colFormat(percent = TRUE, digits = 2),
                    footer = label_percent(accuracy = 0.01)(sum(.$voti_perc))),
                  delta = colDef(name = "Delta", width = 80, align = "center",
                    cell = color_tiles(.,
                                       number_fmt = function(x){
                                         if(is.na(x)){""
                                           }else{label_percent(accuracy = 0.01, style_positive = "plus")(x)}},
                                       color_ref = "delta_col")
                  ),
                  delta_col = colDef(show = FALSE),
                  risultato_bar = colDef(name = "",
                    cell = data_bars(.,
                                     fill_color = blue,
                                     fill_gradient = FALSE,
                                     text_position = "none")
                  ),
                  voti_validi = colDef(name = "Voti validi",
                    format = colFormat(separators = TRUE, digits = 0),
                    footer = label_number(big.mark = ",")(sum(.$voti_validi)))
                )
      )
  })
}

shinyApp(ui = ui, server = server)
