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

### eta_bar_chart ###
eta_bar_chart = function(data,
                         orientation = c("left", "right"), color = c("blue", "red"), MF = c("Maschi", "Femmine"),
                         width = NULL, height = NULL){
  require(dplyr)
  require(tidyr)
  require(echarts4r)

  if(nrow(data)==0){
    return(
    div(style = paste0("width: 100%; height:", height,"; display: flex; justify-content: center;
                       align-content: center; flex-direction: column; padding: 10%;"),
        h5(style = "text-align: center;", "Dati di popolazione non presenti per l'anno selezionato"))
    )
  }

  temp = data %>%
    group_by(eta_quinquennali, sesso) %>%
    summarise(n = sum(residenti), .groups = "drop") %>%
    pivot_wider(names_from = sesso, values_from = n) %>%
    right_join(tibble(eta_quinquennali = unique(data$eta_quinquennali)),
               by = "eta_quinquennali")

  max_x = max(c(temp$Maschi, temp$Femmine)) %>% round(.,-2)
  if(orientation == "right"){inverse = TRUE; left = "3%"; right = "20"; label_position = "left"}
  if(orientation == "left"){inverse = FALSE; left = "3%"; right = "20"; label_position = "right"}

  series = list()
  series[[1]] = list(name = MF, type = "bar", stack = "Total", data = temp[[MF]], color = color,
                     label = list(show = TRUE, position = 'inside'),
                     emphasis = list(focus = "none"))

  opts = list(
    legend = list(show = FALSE),
    xAxis =  list(show = TRUE, type = "value", inverse = inverse, max = max_x),
    yAxis = list(show = TRUE, data = temp$eta_quinquennali, type = 'category', axisLabel = list(interval = 0),
                 axisTick = list(show = FALSE)),
    tooltip = list(trigger = 'axis', textStyle = list(fontSize = 10), padding = 2, backgroundColor = "rgba(255,255,255,0.95)",
                   axisPointer = list(type = 'shadow'),
                   formatter = htmlwidgets::JS(paste0("
                         function(params){
                         return('Fascia di eta <strong>' + params[0].name + '</strong>' +
                         '<br />' + params[0].marker + ' ", MF, " <strong>' + params[0].value + '</strong>')}"))),
    grid = list(right = right, bottom = "3%", left = left, top = "3%", containLabel = TRUE),
    textStyle = list(fontFamily = '"Maven Pro", sans-serif', color = "#525252"),
    series = series)

  plot = e_charts(width = width, height = height) %>% e_list(opts) %>% e_group("pop") %>% e_connect_group("pop")

  return(plot)
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

### svg_paths ###
maschi_svg_path = "path://M 178.77,21.50
           C 189.38,21.50 204.26,22.56 212.09,27.90
             230.70,40.59 237.61,41.60 237.61,41.60
             237.61,41.60 228.52,53.52 225.40,53.52
             225.29,53.52 248.74,67.92 248.74,77.23
             248.74,77.23 232.51,77.23 232.51,77.23
             232.51,77.23 244.57,92.76 245.04,105.09
             245.45,116.21 244.36,155.84 230.43,155.84
             228.95,155.84 227.33,155.39 225.55,154.42
             225.55,154.42 229.72,203.29 201.89,211.97
             201.89,211.97 210.24,254.44 210.24,254.44
             210.24,254.44 247.69,260.73 247.69,260.73
             254.91,261.95 261.52,265.62 266.41,271.17
             266.41,271.17 311.75,322.50 311.75,322.50
             311.75,322.50 21.50,322.50 21.50,322.50
             21.50,322.50 66.84,271.17 66.84,271.17
             71.75,265.62 78.35,261.95 85.56,260.73
             85.56,260.73 123.01,254.44 123.01,254.44
             123.01,254.44 131.36,211.97 131.36,211.97
             103.53,203.29 107.70,154.42 107.70,154.42
             105.92,155.39 104.31,155.84 102.82,155.84
             88.89,155.84 87.80,116.21 88.21,105.09
             88.69,92.76 100.75,77.23 100.75,77.23
             100.75,77.23 84.52,77.23 84.52,77.23
             84.52,67.92 107.97,53.52 107.87,53.52
             104.74,53.52 95.65,41.60 95.65,41.60
             95.65,41.60 102.57,40.60 121.17,27.90
             128.98,22.56 143.87,21.50 154.48,21.50
             161.49,21.50 166.63,21.96 166.63,21.96
             166.63,21.96 178.77,21.50 178.77,21.50 Z
           M 166.62,10.75
           C 164.71,10.61 160.16,10.75 154.48,10.75
             136.40,10.75 123.14,13.53 115.10,19.03
             100.63,28.91 94.26,30.87 93.60,31.06
             90.05,31.74 87.08,34.17 85.70,37.54
             84.26,41.06 84.79,45.09 87.10,48.12
             88.45,49.89 89.71,51.47 90.90,52.87
             80.35,61.04 73.77,68.69 73.77,77.23
             73.77,82.30 77.28,86.56 82.02,87.69
             79.61,92.99 77.70,98.94 77.49,104.68
             77.32,108.97 76.29,146.97 89.52,160.70
             91.68,162.95 94.15,164.57 96.83,165.54
             97.40,180.79 101.18,206.40 119.22,218.05
             119.22,218.05 113.91,245.07 113.91,245.07
             113.91,245.07 83.79,250.13 83.79,250.13
             74.17,251.74 65.30,256.69 58.79,264.05
             58.79,264.05 13.45,315.38 13.45,315.38
             10.64,318.55 9.01,323.07 10.75,326.92
             12.49,330.78 17.28,333.25 21.50,333.25
             21.50,333.25 311.75,333.25 311.75,333.25
             315.97,333.25 319.81,330.78 321.55,326.92
             323.30,323.06 322.61,318.55 319.81,315.38
             319.81,315.38 274.47,264.05 274.47,264.05
             267.97,256.69 259.09,251.74 249.48,250.13
             249.48,250.13 219.35,245.07 219.35,245.07
             219.35,245.07 214.04,218.05 214.04,218.05
             232.08,206.40 235.87,180.79 236.44,165.54
             239.11,164.57 241.58,162.95 243.76,160.70
             256.98,146.97 255.95,108.97 255.79,104.68
             255.57,98.94 253.65,92.99 251.25,87.69
             255.98,86.56 259.50,82.30 259.50,77.23
             259.50,68.69 252.93,61.04 242.37,52.87
             243.55,51.46 244.82,49.89 246.17,48.12
             248.49,45.09 249.01,41.06 247.57,37.54
             246.20,34.17 243.22,31.74 239.67,31.05
             239.00,30.86 232.64,28.90 218.17,19.02
             210.11,13.53 196.86,10.75 178.77,10.75
             178.77,10.75 168.54,10.61 166.62,10.75 Z
           M 211.14,254.59
           C 211.14,254.59 224.16,256.77 224.16,256.77
             224.16,256.77 158.43,322.50 158.43,322.50
             158.43,322.50 143.23,322.50 143.23,322.50
             143.23,322.50 211.14,254.59 211.14,254.59 Z
           M 229.23,322.50
           C 229.23,322.50 244.43,322.50 244.43,322.50
             244.43,322.50 280.18,286.76 280.18,286.76
             280.18,286.76 273.05,278.69 273.05,278.69
             273.05,278.69 229.23,322.50 229.23,322.50 Z
           M 186.23,322.50
           C 186.23,322.50 201.43,322.50 201.43,322.50
             201.43,322.50 258.98,264.96 258.98,264.96
             255.60,262.90 251.85,261.49 247.93,260.81
             247.93,260.81 186.23,322.50 186.23,322.50 Z
           M 272.23,322.50
           C 272.23,322.50 287.43,322.50 287.43,322.50
             287.43,322.50 300.34,309.59 300.34,309.59
             300.34,309.59 293.22,301.52 293.22,301.52
             293.22,301.52 272.23,322.50 272.23,322.50 Z
           M 131.36,211.97
           C 131.36,211.97 129.26,222.68 129.26,222.68
             129.26,222.68 245.07,106.87 245.07,106.87
             245.06,106.23 245.05,105.61 245.02,105.09
             244.90,101.62 243.85,97.88 242.41,94.32
             242.41,94.32 126.80,209.94 126.80,209.94
             128.24,210.72 129.71,211.45 131.36,211.97 Z
           M 57.23,322.50
           C 57.23,322.50 72.43,322.50 72.43,322.50
             72.43,322.50 225.34,169.59 225.34,169.59
             226.19,160.79 225.53,154.41 225.53,154.41
             227.31,155.38 228.93,155.83 230.42,155.83
             236.85,155.83 240.53,147.33 242.61,137.13
             242.61,137.13 57.23,322.50 57.23,322.50 Z
           M 85.56,260.73
           C 78.34,261.95 71.73,265.62 66.84,271.17
             66.84,271.17 21.50,322.50 21.50,322.50
             21.50,322.50 29.43,322.50 29.43,322.50
             29.43,322.50 92.33,259.60 92.33,259.60
             92.33,259.60 85.56,260.73 85.56,260.73 Z
           M 199.69,23.24
           C 195.28,22.35 190.50,21.94 186.02,21.71
             186.02,21.71 88.46,119.27 88.46,119.27
             88.72,123.66 89.17,128.41 89.90,133.03
             89.90,133.03 199.69,23.24 199.69,23.24 Z
           M 242.32,66.63
           C 239.76,64.08 236.85,61.72 234.10,59.64
             234.10,59.64 109.86,183.87 109.86,183.87
             110.78,187.76 112.09,191.55 113.77,195.17
             113.77,195.17 242.32,66.63 242.32,66.63 Z
           M 100.23,322.50
           C 100.23,322.50 115.43,322.50 115.43,322.50
             115.43,322.50 205.84,232.09 205.84,232.09
             205.84,232.09 203.35,219.39 203.35,219.39
             203.35,219.39 100.23,322.50 100.23,322.50 Z
           M 228.18,37.75
           C 225.59,36.41 222.44,34.57 218.59,32.14
             218.59,32.14 97.37,153.37 97.37,153.37
             98.92,154.92 100.75,155.84 102.83,155.84
             104.31,155.84 105.93,155.40 107.71,154.42
             107.71,154.42 107.60,155.96 107.53,158.42
             107.53,158.42 228.18,37.75 228.18,37.75 Z
           M 96.56,83.38
           C 96.56,83.38 158.38,21.55 158.38,21.55
             157.14,21.53 155.89,21.50 154.48,21.50
             150.91,21.50 146.84,21.64 142.74,22.01
             142.74,22.01 87.50,77.23 87.50,77.23
             87.50,77.23 100.75,77.23 100.75,77.23
             100.75,77.23 98.86,79.70 96.56,83.38 Z"

femmine_svg_path = "path://M 15.75,2.00
           C 16.68,2.00 18.53,2.07 19.51,2.56
             21.82,3.73 22.75,5.75 23.27,7.46
             24.17,10.48 22.55,16.70 24.86,19.88
             24.86,19.88 23.66,19.99 22.17,19.99
             21.00,19.99 19.64,19.92 18.53,19.70
             18.53,19.70 19.33,23.65 19.33,23.65
             19.33,23.65 22.91,24.24 22.91,24.24
             23.60,24.35 24.23,24.69 24.70,25.21
             24.70,25.21 29.00,30.00 29.00,30.00
             29.00,30.00 2.00,30.00 2.00,30.00
             2.00,30.00 6.30,25.21 6.30,25.21
             6.77,24.69 7.40,24.35 8.09,24.24
             8.09,24.24 11.67,23.65 11.67,23.65
             11.67,23.65 12.47,19.70 12.47,19.70
             11.36,19.92 10.00,19.99 8.83,19.99
             7.34,19.99 6.14,19.88 6.14,19.88
             8.45,16.70 6.83,10.48 7.73,7.46
             8.25,5.74 9.19,3.73 11.50,2.56
             12.47,2.07 14.32,2.00 15.25,2.00
             15.25,2.00 15.75,2.00 15.75,2.00 Z
           M 15.25,1.00
           C 13.93,1.00 12.13,1.12 11.04,1.67
             8.27,3.08 7.28,5.48 6.78,7.17
             6.38,8.50 6.41,10.24 6.44,12.09
             6.49,14.69 6.54,17.63 5.33,19.30
             5.12,19.59 5.08,19.97 5.23,20.30
             5.38,20.63 5.70,20.85 6.06,20.88
             6.11,20.89 7.31,20.99 8.83,20.99
             9.70,20.99 10.50,20.95 11.21,20.89
             11.21,20.89 10.83,22.78 10.83,22.78
             10.83,22.78 7.93,23.25 7.93,23.25
             7.02,23.40 6.18,23.86 5.56,24.54
             5.56,24.54 1.25,29.33 1.25,29.33
             0.99,29.63 0.92,30.05 1.09,30.41
             1.25,30.77 1.60,31.00 2.00,31.00
             2.00,31.00 29.00,31.00 29.00,31.00
             29.39,31.00 29.75,30.77 29.91,30.41
             30.07,30.05 30.01,29.63 29.74,29.33
             29.74,29.33 25.45,24.54 25.45,24.54
             24.82,23.86 23.98,23.40 23.07,23.25
             23.07,23.25 20.17,22.78 20.17,22.78
             20.17,22.78 19.79,20.89 19.79,20.89
             20.50,20.95 21.30,20.99 22.17,20.99
             23.69,20.99 24.89,20.89 24.94,20.88
             25.30,20.85 25.61,20.63 25.76,20.30
             25.91,19.97 25.88,19.59 25.67,19.30
             24.46,17.63 24.51,14.69 24.56,12.09
             24.59,10.24 24.62,8.50 24.22,7.17
             23.72,5.48 22.72,3.07 19.96,1.67
             18.87,1.12 17.07,1.00 15.75,1.00
             15.75,1.00 15.25,1.00 15.25,1.00 Z
           M 23.61,15.77
           C 23.64,16.20 23.69,16.62 23.76,17.03
             23.76,17.03 20.84,19.95 20.84,19.95
             20.40,19.93 19.95,19.90 19.53,19.85
             19.53,19.85 23.61,15.77 23.61,15.77 Z
           M 10.79,30.00
           C 10.79,30.00 18.96,21.83 18.96,21.83
             18.96,21.83 18.72,20.66 18.72,20.66
             18.72,20.66 9.38,30.00 9.38,30.00
             9.38,30.00 10.79,30.00 10.79,30.00 Z
           M 17.38,30.00
           C 17.38,30.00 18.79,30.00 18.79,30.00
             18.79,30.00 24.10,24.70 24.10,24.70
             23.79,24.50 23.46,24.36 23.10,24.27
             23.10,24.27 17.38,30.00 17.38,30.00 Z
           M 13.38,30.00
           C 13.38,30.00 14.79,30.00 14.79,30.00
             14.79,30.00 20.89,23.91 20.89,23.91
             20.89,23.91 19.67,23.71 19.67,23.71
             19.67,23.71 13.38,30.00 13.38,30.00 Z
           M 24.86,19.88
           C 24.66,19.62 24.50,19.33 24.36,19.02
             24.36,19.02 23.41,19.96 23.41,19.96
             24.27,19.93 24.86,19.88 24.86,19.88 Z
           M 25.38,30.00
           C 25.38,30.00 26.79,30.00 26.79,30.00
             26.79,30.00 27.96,28.84 27.96,28.84
             27.96,28.84 27.29,28.09 27.29,28.09
             27.29,28.09 25.38,30.00 25.38,30.00 Z
           M 21.38,30.00
           C 21.38,30.00 22.79,30.00 22.79,30.00
             22.79,30.00 26.07,26.73 26.07,26.73
             26.07,26.73 25.40,25.98 25.40,25.98
             25.40,25.98 21.38,30.00 21.38,30.00 Z
           M 21.11,3.69
           C 20.87,3.46 20.61,3.25 20.33,3.05
             20.33,3.05 7.37,16.01 7.37,16.01
             7.31,16.60 7.22,17.16 7.09,17.70
             7.09,17.70 21.11,3.69 21.11,3.69 Z
           M 22.74,6.05
           C 22.60,5.74 22.45,5.42 22.26,5.12
             22.26,5.12 7.42,19.96 7.42,19.96
             7.82,19.97 8.29,19.99 8.81,19.99
             8.81,19.99 22.74,6.05 22.74,6.05 Z
           M 5.38,30.00
           C 5.38,30.00 6.79,30.00 6.79,30.00
             6.79,30.00 23.54,13.25 23.54,13.25
             23.54,12.77 23.55,12.29 23.56,11.82
             23.56,11.82 5.38,30.00 5.38,30.00 Z
           M 18.55,2.25
           C 18.15,2.16 17.73,2.10 17.31,2.07
             17.31,2.07 7.44,11.94 7.44,11.94
             7.45,12.40 7.46,12.86 7.46,13.33
             7.46,13.33 18.55,2.25 18.55,2.25 Z
           M 11.53,19.84
           C 11.86,19.80 12.17,19.76 12.47,19.70
             12.47,19.70 12.31,20.48 12.31,20.48
             12.31,20.48 23.54,9.25 23.54,9.25
             23.51,8.80 23.47,8.37 23.39,7.98
             23.39,7.98 11.53,19.84 11.53,19.84 Z
           M 14.79,2.01
           C 14.34,2.02 13.80,2.05 13.26,2.11
             13.26,2.11 7.66,7.71 7.66,7.71
             7.54,8.20 7.49,8.73 7.46,9.34
             7.46,9.34 14.79,2.01 14.79,2.01 Z
           M 8.09,24.24
           C 7.40,24.35 6.77,24.69 6.30,25.21
             6.30,25.21 2.00,30.00 2.00,30.00
             2.00,30.00 2.79,30.00 2.79,30.00
             2.79,30.00 8.65,24.14 8.65,24.14
             8.65,24.14 8.09,24.24 8.09,24.24 Z"

marker_icon = makeIcon(
  iconUrl = "app_www/location.svg#location",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

#### Fix autocomplete_input ####
logical_js = function(b) {
  tolower(isTRUE(b))
}

not_null <- function(vec) {
  vec[vapply(vec, length, 0L) > 0L]
}

autocomplete_input = function(
  id, label, options, value = "", width = NULL, placeholder = NULL,
  max_options = 0, hide_values = FALSE, create = FALSE, contains = FALSE
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is needed to convert list of options into json!")
  }
  value <- shiny::restoreInput(id = id, default = value)
  js_opts <- jsonlite::toJSON(as.list(options), auto_unbox = TRUE)
  width <- shiny::validateCssUnit(width)
  if (length(value) == 0L) value <- ""
  shiny::div(
    class = "form-group shiny-input-container autocomplete",
    style = if (!is.null(width)) paste0("width: ", width, ";"),
    if (!is.null(label)) shiny::tags$label(label, `for` = id),
    shiny::tags$input(
      id = id, type = "text", class = "form-control", result = value,
      value = value, placeholder = placeholder, "data-options" = js_opts,
      "data-max" = max_options, "data-contains" = logical_js(contains),
      "data-hide" = logical_js(hide_values), "data-create" = logical_js(create),
      autocomplete = "off"
    ),
    htmltools::htmlDependency(
      "autocomplete", "0.0.1", c(href = ""),
      # script = "js/autocomplete-binding.js",
      script = "app_www/autocomplete.js",
      stylesheet = "app_www/autocomplete.css"
    )
  )
}

update_autocomplete_input <- function(
  session, id, label = NULL, options = NULL, max_options = NULL, value = NULL,
  placeholder = NULL, hide_values = NULL, create = NULL, contains = NULL
) {
  message <- not_null(list(
    label = label, options = options, value = value, max = max_options,
    placeholder = placeholder, hide = hide_values, create = create,
    contains = contains
  ))
  session$sendInputMessage(id, message)
}
