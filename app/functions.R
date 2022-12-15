panel_ui = function(metric_name, metric, metric_type = "perc",
                    detail1_name, detail1,
                    detail2_name, detail2,
                    delta_label, delta, delta_class,
                    img_src = "", vec_src = NULL){

  require(dplyr)
  require(scales)
  require(shiny)

  metric_style = ""
  if(is.character(metric)){
  metric_style = "font-size: 1.3rem!important; overflow: hidden;
  display: -webkit-box; -webkit-line-clamp: 2;
  -webkit-box-orient: vertical; height: max-content;"
    if(length(img_src)==1){
    icon = tags$div(class = "icon", style = "width: 100%;",
                    tags$image(
                      style = "width: auto; height: 140%;",
                      src=img_src))
    }else{
      img_src_list = list()
      for(i in 1:length(img_src)){
        img_src_list[[i]] = tags$image(style = "width: 20px; height: auto;", src=img_src[i])
      }
      icon = tags$div(class = "icon", img_src_list,
      style = "width: 100%; max-width: 95px; align-content: center;
      heigth: 100%; display: flex; flex-wrap: wrap;")
    }
  }else{
    if(metric_type == "perc"){metric = metric %>% label_percent(accuracy  = 0.01, suffix = " %")(.)}
    if(metric_type == "num"){metric = metric %>% label_number(big.mark = ",", decimal.mark = ".")(.)}
    icon = tags$svg(class = "icon", viewBox="0 0 35 35",
                    style = "width: 100%; height: 100%; fill: #15354a",
                    tags$use(href=vec_src))
  }

  if(detail1>1){
    detail1 = detail1 %>% label_number(big.mark = ",", decimal.mark = ".")(.)}else{
      detail1 = detail1 %>% label_percent(accuracy  = 0.01, suffix = " %")(.)
    }
  if(detail2>1){
    detail2 = detail2 %>% label_number(big.mark = ",", decimal.mark = ".")(.)}else{
      detail2 = detail2 %>% label_percent(accuracy  = 0.01, suffix = " %")(.)
    }

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

