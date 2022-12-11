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
                           uiOutput("sum_partito")
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
addResourcePath("data", paste0(getwd(), "/data"))

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
                           uiOutput("sum_partito")
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
    img_src = r$contrassegni %>% filter(nome_lista_eligendo == metric) %>%
      mutate(img_src = paste0("data/", key, "/", image)) %>% pull(img_src)
print(img_src)
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
             delta, delta_label, delta_class,
             img_src)

  })
}


shinyApp(ui = ui, server = server)
