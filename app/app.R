library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(scales)

addResourcePath("www", paste0(getwd(), "/www"))

tree_table = readRDS("../data/general-porpuse/tree_table.rds")
tree_table = readRDS("../data/general-porpuse/tree_table.rds")

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
                           uiOutput("sum_affluenza")

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
    })

  affluenza = reactive({if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_affluenza_rds)}else{NULL}})
  voti = reactive({readRDS(if(!is.null(r$tree_table)){readRDS(r$tree_table$temp_rds)}else{NULL})})

  affluenza_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_affluenza_rds)}else{NULL}})
  voti_prev = reactive({if(!is.null(r$tree_table_prev)){readRDS(r$tree_table_prev$temp_rds)}else{NULL}})

  output$sum_affluenza = renderUI({

    affluenza = sum(affluenza()$totale_votanti)/sum(affluenza()$iscritti)
    votanti = sum(affluenza()$totale_votanti) %>% label_number(big.mark = ",", decimal.mark = ".")(.)
    iscritti = sum(affluenza()$iscritti) %>% label_number(big.mark = ",", decimal.mark = ".")(.)

    print(affluenza_prev())
    if(!is.null(affluenza_prev())){
      affluenza_prev = sum(affluenza_prev()$totale_votanti)/sum(affluenza_prev()$iscritti)
      delta_affluenza = (affluenza - affluenza_prev) %>% label_percent(accuracy  = 0.01, suffix = " %",
                                                                       )(.)
      delta_label = paste0("(rispetto ", r$tree_table_prev$anno, ")")
    }else{delta_affluenza = "-"; delta_label = "-"}

    affluenza = affluenza %>% label_percent(accuracy  = 0.01, suffix = " %")(.)

    div(class = "panel panel-metric",
        div(style = "min-height: 0px; border-radius: 6px 6px 0px 0px;
            padding: 0.4375rem 0.9375rem; background-color: rgb(21, 53, 74); color: white;",
            h4("Affluenza")),
        div(class = "shiny-html-output shiny-bound-output",
            tags$span(class = "metric", affluenza),
            tags$svg(class = "icon", viewBox="0 0 50 50",
                     style = "width: 100%; height: 100%; fill: rgb(179, 184, 186)",
                     tags$use(href="www/affluenza.svg#affluenza")),
            div(class = "detail-value",
                div(style = "display: flex; gap: 0.625rem; align-items: center;", h5("Votanti"), votanti),
                div(style = "display: flex; gap: 0.625rem; align-items: center;", h5("Iscritti"), iscritti)
            ),
            tags$span(class = "change-value",
                      h4(delta_affluenza), h6(delta_label))
            )
        )
    })
}


shinyApp(ui = ui, server = server)
