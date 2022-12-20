### add_loader ###

add_loader = function (ui_element, type = "html", loader = "dnaspin", proxy.height = if (grepl("height:\\s*\\d",
                                                                                  ui_element)) NULL else "400px"){
  require(dplyr)
  require(shiny)
  require(glue)

  stopifnot(type %in% c("html", "image", "text"))
  proxy_element = tagList()
  if (!is.null(proxy.height)) {
    proxy_element = div(style = glue("height:{ifelse(is.null(proxy.height),'100%',proxy.height)}"),
                        class = "shiny-loader-placeholder")
  }

  htmlfile = paste0("app_www/", loader, ".html")
  tagList(singleton(tags$head(tags$link(rel = "stylesheet",
                                        href = "assets/imgcustom-loader.css"))),
          singleton(tags$script(src = "assets/imgcustom-loader.js")),
          singleton(tags$head(tags$link(rel = "stylesheet",
                                        href = "css-loaders/css/imgcustom-fallback.css"))),
          singleton(tags$head(tags$link(rel = "stylesheet",
                                        href = paste0("app_www/", loader, ".css")))),
          div(class = "shiny-loader-output-container",
              div(class = "load-container", includeHTML(htmlfile)),
              proxy_element, ui_element)
          )

}

### panel_ui ###

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

### LEAFLET ADDITION ###
# https://github.com/rstudio/leaflet/pull/598/files

setCircleMarkerRadius = function(map, layerId, radius, data=getMapData(map)){
  options = list(layerId = layerId, radius = radius)
  # evaluate all options
  options = evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options = do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  leaflet::invokeMethod(map, data, "setRadius", options$layerId, options$radius)
}

#' @describeIn map-layers Change style of existing circle markers.
#' @export
setCircleMarkerStyle = function(map, layerId
                                 , radius = NULL
                                 , stroke = NULL
                                 , color = NULL
                                 , weight = NULL
                                 , opacity = NULL
                                 , fill = NULL
                                 , fillColor = NULL
                                 , fillOpacity = NULL
                                 , dashArray = NULL
                                 , options = NULL
                                 , data = getMapData(map)
){
  if (!is.null(radius)){
    setCircleMarkerRadius(map, layerId = layerId, radius = radius, data = data)
  }

  options = c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray
               )))

  if (length(options) < 2) { # no style options set
    return()
  }
  # evaluate all options
  options = evalFormula(options, data = data)

  # make them the same length (by building a data.frame)
  options = do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId = options[[1]]
  style = options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "marker", layerId, style);
}

setCircleMarkerStyle = function(map, layerId
                                 , radius = NULL
                                 , stroke = NULL
                                 , color = NULL
                                 , weight = NULL
                                 , opacity = NULL
                                 , fill = NULL
                                 , fillColor = NULL
                                 , fillOpacity = NULL
                                 , dashArray = NULL
                                 , options = NULL
                                 , data = getMapData(map)
){
  if (!is.null(radius)){
    setCircleMarkerRadius(map, layerId = layerId, radius = radius, data = data)
  }

  options = c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray
               )))

  if (length(options) < 2) { # no style options set
    return()
  }
  # evaluate all options
  options = evalFormula(options, data = data)

  # make them the same length (by building a data.frame)
  options = do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId = options[[1]]
  style = options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "marker", layerId, style);
}

setShapeStyle = function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL){
  options = c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options = evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options = do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId = options[[1]]
  style = options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setShapeLabel <- function( map, data = getMapData(map), layerId,
                           label = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  label <- options[-1] # drop layer column

  # typo fixed in this line
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}

### easy_button_js ###

easy_button_js = 'function map_custom_button(btn, map){
  var map_id = map.id;
  var shiny_input = map.id + "_type";
  var btn_id = btn.button.id;
  var layers = ["density", "variation"]
  for(i in layers){
    var btn_id_i = map_id + "_" + layers[i] + "_btn"
    var icon_id_i = map_id + "_" + layers[i] + "_icon"
    if(btn_id_i == btn_id){
      document.getElementById(btn_id_i).style.backgroundColor = "#15354a";
      document.getElementById(icon_id_i).style.fill = "#fff";
      Shiny.onInputChange(shiny_input, layers[i]);
    }else{
      document.getElementById(btn_id_i).style.backgroundColor = "#fff";
      document.getElementById(icon_id_i).style.fill = "#15354a";
    }
  }
}'
