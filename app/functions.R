panel_ui = function(metric_name, metric,
                    detail1_name, detail1,
                    detail2_name, detail2,
                    delta_label, delta, delta_class,
                    img_src = NULL){
print(img_src)
  require(dplyr)
  require(scales)
  require(shiny)

  metric_style = ""
  if(is.character(metric)){
  metric_style = "font-size: 1.3rem!important; overflow: hidden;
  display: -webkit-box; -webkit-line-clamp: 2;
  -webkit-box-orient: vertical; height: max-content;"
  icon = tags$div(class = "icon", style = "width: 100%;",
                  tags$image(
                    style = "width: auto; height: 100%;",
                    src=img_src))
  }else{
    metric = metric %>% label_percent(accuracy  = 0.01, suffix = " %")(.)
    icon = tags$svg(class = "icon", viewBox="0 0 50 50",
                    style = "width: 100%; height: 100%; fill: rgb(179, 184, 186)",
                    tags$use(href="www/affluenza.svg#affluenza"))
  }
  detail1 = detail1 %>% label_number(big.mark = ",", decimal.mark = ".")(.)
  detail2 = detail2 %>% label_number(big.mark = ",", decimal.mark = ".")(.)

  div(class = "panel panel-metric",
      div(style = "min-height: 0px; border-radius: 6px 6px 0px 0px;
              padding: 0.4375rem 0.9375rem; background-color: rgb(21, 53, 74); color: white;",
          h4(metric_name)),
      div(class = "shiny-html-output shiny-bound-output",
          tags$span(class = "metric", style = metric_style, metric),
          icon,
          div(class = "detail-value",
              div(style = "display: flex; gap: 0.625rem; align-items: center;", h5(detail1_name), detail1),
              div(style = "display: flex; gap: 0.625rem; align-items: center;", h5(detail2_name), detail2)
          ),
          tags$span(class = "change-value", class = delta_class,
                    h4(delta_label), h6(delta))
      )
  )
}
