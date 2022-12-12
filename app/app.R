library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(scales)

addResourcePath("www", paste0(getwd(), "/www"))

github_prefix = "https://raw.githubusercontent.com/FlavioLeccese92/bologna-politics/main/"

tree_table = readRDS("../data/general-porpuse/tree_table.rds")
nome_lista_conciliazione = readRDS("../data/general-porpuse/nome_lista_conciliazione.rds")
contrassegni = readRDS("../data/general-porpuse/contrassegni.rds")

nome_lista_conciliazione = nome_lista_conciliazione %>%
  mutate(id_partito = case_when(nome_lista_eligendo %in%
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
                                   TRUE ~ id_partito
  ))

source("functions.R")

ui = fluidPage(
    includeCSS("www/styles.css"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&amp;display=swap", rel="stylesheet"),
    tags$main(class = "dashboard",
              tags$header(class = "dashboard-header",
                          tags$h2(class = "dashboard-heading",
                                  tags$a(href="https://shiny.rstudio.com/",
                                         class="logo logo-shiny", target="_blank",
                                         rel="nofollow noreferrer", `aria-label`="RStudio Shiny Website",
                                         tags$svg(class="logo-svg", viewBox="0 0 100 68",
                                                  tags$use(href="www/icons-sprite-map.svg#shiny-logo"))),
                                  tags$span(class="dashboard-title", "Bologna Politics")),
                          tags$div(class = "dashboard-filters",
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

                                   )),
              tags$section(class = "dashboard-panels",
                           uiOutput("sum_affluenza"),
                           uiOutput("sum_partito"),
                           uiOutput("sum_coalizione")
                           ))
)

server = function(input, output, session) {
  r = reactiveValues(tree_table = NULL, tree_table_prev = NULL)

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
      mutate(temp_html = paste0(".", temp_html),
             temp_files = paste0(".", temp_files),
             temp_rds = paste0(".", temp_rds),
             temp_affluenza_rds = paste0(".", temp_affluenza_rds)
             ) %>% {if (nrow(.) == 0) NULL else .}

    r$tree_table_prev =
      tree_table %>%
      filter(votazione == input$gen_input_votazione,
             organo == input$gen_input_organo) %>%
      arrange(anno) %>%
      filter(lead(anno == input$gen_input_anno)) %>%
      mutate(temp_html = paste0(".", temp_html),
             temp_files = paste0(".", temp_files),
             temp_rds = paste0(".", temp_rds),
             temp_affluenza_rds = paste0(".", temp_affluenza_rds)
      ) %>% {if (nrow(.) == 0) NULL else .}

    r$nome_lista_conciliazione =
      nome_lista_conciliazione %>%
      filter(key == r$tree_table$key, organo == input$gen_input_organo)

    r$contrassegni =
      contrassegni %>%
      filter(key == r$tree_table$key, organo == input$gen_input_organo)
    })

  affluenza = reactive({if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_affluenza_rds)}else{NULL}})

  voti_sezioni = reactive({
    (if(!is.null(r$tree_table)){
    readRDS(r$tree_table$temp_rds) %>%
        left_join(nome_lista_conciliazione %>%
                    filter(organo == input$gen_input_organo),
                  by = c("nome_lista" = "nome_lista_opendata"))%>%
        select(id_sezione,
               nome_lista = nome_lista_eligendo,
               voti_validi,
               nome_coalizione = nome_coalizione_eligendo)
      }else{NULL})})

  voti_partito = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
        group_by(nome_lista) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  affluenza_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_affluenza_rds)}else{NULL}})
  voti_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_rds)}else{NULL}})

  ####--- Affluenza ---####

  output$sum_affluenza = renderUI({

    metric = sum(affluenza()$totale_votanti)/sum(affluenza()$iscritti)
    img_src = r$contrassegni %>% filter(nome_lista_eligendo == metric) %>%
      mutate(img_src = paste0("../data/", key, "/", image)) %>% pull(img_src)

    detail1 = sum(affluenza()$totale_votanti)
    detail2 = sum(affluenza()$iscritti)

    delta_class = "positive-change"
    if(!is.null(affluenza_prev())){
      affluenza_prev = sum(affluenza_prev()$totale_votanti)/sum(affluenza_prev()$iscritti)
      delta = metric - affluenza_prev
      if(delta<0){delta_class = "negative-change"}
      delta = delta %>%
        label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
      delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    panel_ui("Affluenza", metric,
             "Votanti", detail1,
             "Iscritti", detail2,
             delta, delta_label, delta_class,
             img_src)

    })

  ####--- Voti partito ---####

  output$sum_partito = renderUI({

    metric = voti_partito() %>% slice(1) %>% pull(nome_lista)
    detail1 = voti_partito() %>% slice(1) %>% pull(voti_validi)
    detail2 = 0

    delta_class = "positive-change"; delta = "-"; delta_label = "-"
    # if(!is.null(affluenza_prev())){
    #   affluenza_prev = sum(affluenza_prev()$totale_votanti)/sum(affluenza_prev()$iscritti)
    #   delta = affluenza - affluenza_prev
    #   if(delta<0){delta_class = "negative-change"}
    #   delta = delta %>%
    #     label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
    #   delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    # }else{delta = "-"; delta_label = "-"}

    panel_ui("Primo Partito", metric,
             "Voti", detail1,
             "detail2", detail2,
             delta, delta_label, delta_class)

  })
}


shinyApp(ui = ui, server = server)
library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(scales)

addResourcePath("www", paste0(getwd(), "/www"))

tree_table = readRDS("../data/general-porpuse/tree_table.rds")
contrassegni = readRDS("../data/general-porpuse/contrassegni.rds")
source("functions.R")

ui = fluidPage(
    includeCSS("www/styles.css"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&amp;display=swap", rel="stylesheet"),
    tags$main(class = "dashboard",
              tags$header(class = "dashboard-header",
                          tags$h2(class = "dashboard-heading",
                                  tags$a(href="https://shiny.rstudio.com/",
                                         class="logo logo-shiny", target="_blank",
                                         rel="nofollow noreferrer", `aria-label`="RStudio Shiny Website",
                                         tags$svg(class="logo-svg", viewBox="0 0 100 68",
                                                  tags$use(href="www/icons-sprite-map.svg#shiny-logo"))),
                                  tags$span(class="dashboard-title", "Bologna Politics")),
                          tags$div(class = "dashboard-filters",
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

                                   )),
              tags$section(class = "dashboard-panels",
                           uiOutput("sum_affluenza"),
                           uiOutput("sum_partito"),
                           uiOutput("sum_coalizione")
                           ))
)

server = function(input, output, session) {
  r = reactiveValues(tree_table = NULL, tree_table_prev = NULL)

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
      mutate(temp_html = paste0(".", temp_html),
             temp_files = paste0(".", temp_files),
             temp_rds = paste0(".", temp_rds),
             temp_affluenza_rds = paste0(".", temp_affluenza_rds)
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
      mutate(temp_html = paste0(".", temp_html),
             temp_files = paste0(".", temp_files),
             temp_rds = paste0(".", temp_rds),
             temp_affluenza_rds = paste0(".", temp_affluenza_rds)
      ) %>% {if (nrow(.) == 0) NULL else .}

    r$nome_lista_conciliazione_prev =
      nome_lista_conciliazione %>%
      {if (is.null(r$tree_table_prev$key)) NULL else
        filter(., key == r$tree_table_prev$key, organo == input$gen_input_organo)}

    })


  ####--- Affluenza ---####

  affluenza = reactive({if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_affluenza_rds)}else{NULL}})
  affluenza_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_affluenza_rds)}else{NULL}})
  voti_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_rds)}else{NULL}})

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
      delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    panel_ui("Affluenza", metric,
             "Totale voti", detail1,
             "Aventi diritto", detail2,
             delta, delta_label, delta_class)

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
               id_partito, id_coalizione)
    }else{NULL})})

  voti_partito = reactive({
    (if(!is.null(voti_sezioni())){
      voti_sezioni() %>%
        group_by(nome_lista, id_partito) %>%
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
               id_partito, id_coalizione)
    }else{NULL})})

  voti_partito_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
        group_by(nome_lista, id_partito) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop") %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  voti_coalizione_prev = reactive({
    (if(!is.null(voti_sezioni_prev())){
      voti_sezioni_prev() %>%
        group_by(nome_coalizione, id_coalizione) %>%
        summarise(voti_validi = sum(voti_validi), .groups = "drop",) %>%
        mutate(voti_perc = voti_validi/sum(.$voti_validi)) %>%
        arrange(desc(voti_validi))
    }else{NULL})})

  output$sum_partito = renderUI({

    metric = voti_partito() %>% slice(1) %>% pull(nome_lista)

    detail1 = voti_partito() %>% slice(1) %>% pull(voti_perc)
    detail2 = voti_partito() %>% slice(1) %>% pull(voti_validi)

    delta_class = "positive-change"
    if(!is.null(voti_partito_prev())){

      delta_voti = voti_partito() %>% slice(1) %>% select(id_partito, voti_perc) %>%
        inner_join(voti_partito_prev() %>%
                     filter(id_partito != "-") %>%
                     select(id_partito, voti_perc_prev = voti_perc),
                   by = "id_partito")
      if(nrow(delta_voti)>0){
        delta = delta_voti$voti_perc - delta_voti$voti_perc_prev
      }
      if(delta<0){delta_class = "negative-change"}
      delta = delta %>%
        label_percent(accuracy  = 0.01, suffix = " %", style_positive = "plus")(.)
      delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    img_src = r$contrassegni %>% filter(nome_lista_eligendo == metric) %>%
      mutate(img_src = paste0(github_prefix, "/data/", key, "/", image)) %>% pull(img_src)

    panel_ui("Primo Partito", metric,
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
      delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    }else{delta = "-"; delta_label = "-"}

    nome_lista = voti_coalizione() %>% slice(1) %>% pull(nome_lista) %>% unlist()
    img_src = r$contrassegni %>% filter(nome_lista_eligendo %in% nome_lista) %>%
      mutate(img_src = paste0(github_prefix, "/data/", key, "/", image)) %>% pull(img_src)

    panel_ui("Prima Coalizione", metric,
             "Risultato", detail1,
             "Voti validi", detail2,
             delta, delta_label, delta_class,
             img_src)

  })
}


shinyApp(ui = ui, server = server)
