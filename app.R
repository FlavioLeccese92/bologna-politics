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
library(viridis)

addResourcePath("app_www", paste0(getwd(), "/app_www"))

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

### polygons ###
sezioni_polygons = readRDS("data/polygons/sezioni_polygons.rds")

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
                                grepl("POPOLO DELLA LIBERTA", nome_lista_eligendo) ~ "PDL",
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
    includeCSS("app_www/styles.css"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&amp;display=swap", rel="stylesheet"),
    tags$main(class = "dashboard",
              tags$header(class = "dashboard-header",
                          tags$h2(class = "dashboard-heading",
                                  tags$a(href="https://shiny.rstudio.com/",
                                         class="logo logo-shiny", target="_blank",
                                         rel="nofollow noreferrer", `aria-label`="RStudio Shiny Website",
                                         tags$svg(class="logo-svg", viewBox="0 0 100 68",
                                                  tags$use(href="www/icons-sprite-map.svg#shiny-logo"))),
                                  tags$span(class="dashboard-title", "Bologna Politics"))#,
                          ),
              tags$section(class = "dashboard-panels",
                           tags$div(class = "dashboard-filters-vertical",
                                             selectizeInput("gen_input_votazione", "Elezioni",
                                                            choices = list("<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='www/bandiera-comune-bologna.png' width=30px>Comunali</img></div>" = "comunali",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='www/bandiera-emilia-romagna.png' width=30px>Regionali</img></div>" = "regionali",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='www/bandiera-italia.png'         width=30px>Politiche</img></div>" = "politiche",
                                                                           "<div> <img style = 'padding-right: 5px; padding-bottom: 2px;' src='www/bandiera-unione-europea.png' width=30px>Europee</img></div>" = "europee"
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
              tags$section(class = "dashboard-panels-2col",
                           reactableOutput("table_partiti"),
                           leafletOutput("map_partiti"))
    )
)

server = function(input, output, session) {

  r = reactiveValues()

  observeEvent(input$gen_input_votazione, {
    organo = tree_table %>% filter(votazione == input$gen_input_votazione) %>%
      pull(organo) %>% unique()
    names(organo) = organo %>% gsub("_", " ", .) %>% str_to_title()
      updateSelectizeInput(session = session, "gen_input_organo", choices = organo, selected = organo[1])

    anno = tree_table %>% filter(votazione == input$gen_input_votazione) %>%
      pull(anno) %>% unique()
    updateSelectizeInput(session = session, "gen_input_anno", choices = anno, selected = anno[1])
  })

  observeEvent(c(input$gen_input_anno, input$gen_input_organo), {

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

    ### in attesa di mettere filtri su sezioni ###
    r$sezioni = sezioni %>%
      left_join(aree_statistiche, by = "id_area_statistica") %>%
      left_join(zone, by = "id_zona") %>%
      left_join(quartieri, by = "id_quartiere")

    })

  ####--- Affluenza ---####

  affluenza = reactive({if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_affluenza_rds)}else{NULL}})
  affluenza_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_affluenza_rds)}else{NULL}})

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
             vec_src = "www/affluenza.svg#affluenza")

    })

  ####--- voti ---####

  voti_sezioni = reactive({
    (if(!is.null(r$tree_table)){
      readRDS(r$tree_table$temp_rds) %>%
        left_join(r$nome_lista_conciliazione,
                  by = c("nome_lista" = "nome_lista_opendata"))%>%
        select(id_sezione,
               nome_lista = nome_lista_eligendo,
               voti_validi,
               nome_coalizione = nome_coalizione_eligendo,
               id_lista, id_coalizione)
    }else{NULL})})

  voti_partito = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
        group_by(nome_lista, id_lista, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  voti_coalizione = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
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
        left_join(r$nome_lista_conciliazione_prev,
                  by = c("nome_lista" = "nome_lista_opendata"))%>%
        select(id_sezione,
               nome_lista = nome_lista_eligendo,
               voti_validi,
               nome_coalizione = nome_coalizione_eligendo,
               id_lista, id_coalizione)
    }else{NULL})})

  voti_partito_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
        group_by(nome_lista, id_lista, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  voti_coalizione_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
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
      mutate(img_src = paste0(github_prefix, "/data/", key, "/", image)) %>% pull(img_src)

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
      mutate(img_src = paste0(github_prefix, "/data/", key, "/", image)) %>% pull(img_src)

    panel_ui("Prima Coalizione", metric, metric_type = NULL,
             "Risultato", detail1,
             "Voti validi", detail2,
             delta, delta_label, delta_class,
             img_src)

  })

  ####--- map partiti sezioni ---####

  voti_sezioni_map = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni()  %>%
        group_by(id_sezione) %>%
        mutate(tot_voti_validi = sum(voti_validi)) %>%
        ungroup() %>%
        {if (!is.null(table_partiti_selected()))
          filter(., nome_lista %in% (voti_partito() %>%
                   slice(table_partiti_selected()) %>%
                   pull(nome_lista))) else .} %>%
        group_by(id_sezione) %>%
        summarise(risultato = sum(voti_validi)/first(tot_voti_validi), .groups = "drop") %>%
        left_join(sezioni_polygons, by = "id_sezione") %>%
        st_set_geometry("geometry")
    }else{NULL})})

  output$map_partiti = renderLeaflet({
    if(!is.null(table_partiti_selected())){

    map_data = voti_sezioni_map()
    risultato_discrete = seq(floor(100*min(map_data$risultato)),
                             ceiling(100*max(map_data$risultato)),
                             length = 8) %>% floor()/100

    legend_color = viridis_pal(option = "G", direction = 1)(length(risultato_discrete)-1)
    legend_labels = paste0(label_percent()(risultato_discrete)[-length(risultato_discrete)], " - ",
                           label_percent()(risultato_discrete)[-1]) %>% rev()

    pal = colorNumeric(viridis_pal(option = "G", direction = -1)(20),
                       range(risultato_discrete))

    map_data = map_data %>%
      mutate(risultato_col = risultato %>% ifelse(. <= min(risultato_discrete), min(risultato_discrete), .) %>%
               ifelse(. >= max(risultato_discrete), max(risultato_discrete), .))

    leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 11, maxZoom = 16, className = "dark-map-tiles")) %>%
      setView(lng = 11.3493292, lat = 44.492245, zoom = 12) %>%
      setMaxBounds(lng1 = 11.22814, lat1 = 44.41880, lng2 = 11.43364, lat2 = 44.55736) %>%
      addPolygons(data = map_data, group = "Sezioni density",
                  weight = 0.5, opacity = 1, fillOpacity = 0.5,
                  color = ~pal(risultato_col),
                  smoothFactor = 0.1, # label  = ~affluenza_label,
                  highlight = highlightOptions(weight = 1, fillOpacity = 1, color = "white",
                                               bringToFront = TRUE,sendToBack = TRUE)) %>%
      addLegend("bottomright",
                colors = legend_color,
                labels = legend_labels,
                opacity = 0.7, title = "Risultato %")
}
  })

  ####--- table partiti ---####
  table_partiti_selected = reactive({
    getReactableState("table_partiti", "selected")
  })

  output$table_partiti = renderReactable({

    voti_partito() %>%
      left_join(voti_partito_prev() %>%
                   filter(id_lista != "-") %>%
                   select(id_lista, voti_perc_prev = voti_perc),
                 by = "id_lista") %>%
      left_join(r$contrassegni %>% rename(nome_lista = nome_lista_eligendo) %>%
                  mutate(img_src = paste0(github_prefix, "/data/", key, "/", image)),
                by = "nome_lista") %>%
      mutate(risultato_bar = voti_perc,
             delta = ifelse(!is.na(voti_perc_prev), voti_perc - voti_perc_prev, NA),
             delta_col = case_when(is.na(delta) ~ "transparent",
                                   delta > 0 ~ "#27b376", TRUE ~ "#FF364A")) %>%
      select(img_src, nome_lista, id_coalizione,
             voti_perc, risultato_bar, delta, delta_col, voti_validi) %>%
      reactable(.,
                pagination = FALSE, highlight = TRUE,
                style = list(fontSize = "0.9rem"),
                selection = "single", onClick = "select", defaultSelected = 1,
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#15354a",
                                          color = "white", borderRadius = "6px"),
                  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
                ),
                columns = list(
                  .selection = colDef(show = FALSE),
                  img_src = colDef(name = "", width = 35, sticky = "left",
                                   cell = embed_img(height = 30, width = 30)),
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
                                           }else{scales::label_percent(accuracy = 0.01, style_positive = "plus")(x)}},
                                       color_ref = "delta_col")
                  ),
                  delta_col = colDef(show = FALSE),
                  risultato_bar = colDef(name = "",
                    cell = data_bars(.,
                                     fill_color = "#0099F9",
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
