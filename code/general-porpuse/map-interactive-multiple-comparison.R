###--------------------------------------------------------------------------###
####               map-interactive-multiple-comparison.R                     ###
###--------------------------------------------------------------------------###

library(paletteer)
library(leaflet.extras)

# Polygon highlighting options
poly_highlighting = highlightOptions(
  weight = 2,
  color = "white",
  fillOpacity = 1,
  bringToFront = TRUE
)

# Tooltip (label) options
tooltip_options = labelOptions(
  direction = "auto",
  textsize = "15px",
  opacity = 0.9,
  style = list(
    "font-family" = "Arial",
    "padding" = "6px",
    "border-color" = "black"
  )
)

# Function to add variable legends to map
add_legend = function(map, var_id, var_pal, polygons, ...){
  addLegend(
    map = map,
    data = polygons,
    opacity = 0.7,
    na.label = "No data",
    position = "bottomright",
    layerId = var_id,
    title = var_id,
    pal = var_pal,
    # Required in order to select the correct variable via var_id
    values = select(polygons, !!var_id),
    labFormat = labelFormat(digits = 2, suffix = " %"),
    ...
  )
}

# Function to add variable choropleths to map
add_choropleth = function(map, var_id, var_pal, var_pal_lines, polygons, ...){

  # Tooltip (label)
  tooltip = sprintf(
    "<b>Sezione elettorale %s</b><br/>%g%%",
    polygons[["sezione_elettorale"]],
    polygons[[var_id]]
  ) %>% lapply(htmltools::HTML)
  # Polygons
  addPolygons(
    map = map, data = polygons,
    fillOpacity = 0.7,
    color = ~var_pal_lines(polygons[[var_id]]),
    weight = 0.6, opacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2, fillOpacity = 1, bringToFront = TRUE
    ),
    label = tooltip, labelOptions = tooltip_options,
    group = var_id,
    # Required in order to select the correct variable via var_id
    fillColor = ~var_pal(polygons[[var_id]]),
    ...
  )
}

add_markers = function(map, var_id, markers, icon, ...){

  # Tooltip (label)
  tooltip = sprintf(
    "<b>Rank %s</b><br/>",
    markers[["rank"]]
  ) %>% lapply(htmltools::HTML)
  # Marker
  addMarkers(
    map = map,
    data = markers[[var_id]],
    icon = icon,
    label = tooltip,
    labelOptions = tooltip_options,
    group = var_id
  )
}

# function to create map interactive multiple
map_interactive_multiple = function(polygons,
                                    centroids,
                                    election_results_from, nome_from,
                                    election_results_to, nome_to,
                                    title_popup = "Senato Plurinominale 2022",
                                    map_provider = c("CartoDB.Positron", "CartoDB.DarkMatter")){

  require(dplyr)
  require(leaflet)
  require(ggsci)
  require(sf)
  require(htmltools)
  require(htmlwidgets)

  icon = makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/2107/2107890.png",
                  iconWidth = 18, iconHeight = 18)

  icon_top = makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/2475/2475607.png",
                      iconWidth = 18, iconHeight = 18)

  icon_flop = makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/2475/2475294.png",
                       iconWidth = 18, iconHeight = 18)

  names(election_results_from)[which(names(election_results_from) %in% c("nome_lista", "nome_candidato"))] = "y"
  names(election_results_to)[which(names(election_results_to) %in% c("nome_lista", "nome_candidato"))] = "y"

  var_ids_from = nome_from
  var_ids_to = nome_to

  markers_top = tibble(rank = 1:10)
  markers_flop = tibble(rank = 1:10)

  for(l in 1:length(var_ids_from)){
    var_id_from = var_ids_from[l]
    var_id_to = var_ids_to[l]

    election_results_var_id = inner_join(
      election_results_from %>%
        group_by(sezione_elettorale) %>%
        summarise(freq_from = round(100*sum(ifelse(y == var_id_from, voti_validi, 0))/sum(voti_validi), 2),
                  .groups = "drop") %>%
        mutate(freq_from = ifelse(is.nan(freq_from), 0, freq_from)),
      election_results_to %>%
        group_by(sezione_elettorale) %>%
        summarise(freq_to = round(100*sum(ifelse(y == var_id_to, voti_validi, 0))/sum(voti_validi), 2),
                  .groups = "drop") %>%
        mutate(freq_to = ifelse(is.nan(freq_to), 0, freq_to)),
      by = "sezione_elettorale") %>%
      mutate(delta = freq_to - freq_from)

    top_10_var_id = election_results_var_id  %>%
      inner_join(centroids, by = "sezione_elettorale") %>%
      slice_max(n = 10, order_by = delta) %>%
      mutate(rank = row_number()) %>%
      select(rank, geometry)

    flop_10_var_id = election_results_var_id  %>%
      inner_join(centroids, by = "sezione_elettorale") %>%
      slice_min(n = 10, order_by = delta) %>%
      mutate(rank = row_number()) %>%
      select(rank, geometry)

    polygons = polygons %>%
      left_join(election_results_var_id,  by = "sezione_elettorale") %>%
      rename_with(~var_id_to, delta)

    markers_top = markers_top %>%
      left_join(top_10_var_id, by = "rank") %>%
      rename_with(~var_id_to, geometry)

    markers_flop = markers_flop %>%
      left_join(flop_10_var_id, by = "rank") %>%
      rename_with(~var_id_to, geometry)
  }

  # Model information icon content
  info_content <- HTML(paste0(
    HTML('<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'),
    # Header / Title
    HTML(paste0("<h3>", title_popup, "</h3>")),
    HTML('</div><div class="modal-body">'),
    # Body
    HTML(
      '<h4><strong>Informazioni</strong></h4>
    <p>
    Questa mappa mostra la distribuzione del voto per partito in ciascuna sezione elettorale. I poligoni sono costituiti
    dall\'unione dei poligoni relativi ai singoli numerici civici afferenti ad una sezione, "espandendo" ciascun punto
    (numero civico) fino ad incontrare l\'"espansione" dei numeri civici confinanti. L\'"espansione è vincolata alla zona
    a cui ciascun civico afferisce,
    Per fare questa operazione abbiamo usato la funzione
    <text style = "font-family:Consolas;">st_voronoi</text> del pacchetto
    <text style = "font-family:Consolas;">sf</text>.
    </p>
    <hr>
    <h4><strong>Links</strong></h4>
    <p>Data source: <a href="https://opendata.comune.bologna.it/explore/dataset/elezioni-politiche-2022-senato-plurinominale/export/?disjunctive.quartiere&disjunctive.zona&disjunctive.nome_lista/">
    link</a></p>'),
    # Closing divs
    HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
  ))

  # Define bins
  var_bins = list()
  for(i in 1:length(var_ids_to)){
    var_bins[[i]] = unique(quantile(abs(na.omit(polygons[[var_ids_to[i]]])), seq(0, 1, 0.2)))
  }

  # Define colour palettes
  var_palettes = var_palettes_lines = list()
  for(i in 1:length(var_ids_to)){
    n_bins = length(var_bins[[i]])
    temp_palette = c(rev(colorRampPalette(c("white", "#fb0934"))(n_bins)[-1]),
                     "white",
                     colorRampPalette(c("white", "#27b376"))(n_bins)[-1])
    temp_palette_lines = c(rep("white", n_bins - 1), "grey", rep("white", n_bins - 1))
    var_palettes[[i]] = colorBin(temp_palette, domain = polygons[[var_ids_to[i]]],
                                 bins = c(-rev(var_bins[[i]][-1]), var_bins[[i]][-1]))
    var_palettes_lines[[i]] = colorBin(temp_palette_lines, domain = polygons[[var_ids_to[i]]],
                                       bins = c(-rev(var_bins[[i]][-1]), var_bins[[i]][-1]))
  }

  # Plot map
  out =
    leaflet() %>%
    addProviderTiles(map_provider) %>%
    # Set view and zoom level
    setView(lng = 11.3426163, lat = 44.494887, zoom = 12) %>%
    # Reset map to default setting
    addResetMapButton() %>%
    # Add a scalebar
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(imperial = FALSE)
    )
  for(l in 1:length(var_ids_to)){
    out = out %>%
      add_choropleth(var_id = var_ids_to[l], var_pal = var_palettes[[l]], var_pal_lines = var_palettes_lines[[l]], polygons = polygons) %>%
      add_legend(var_id = var_ids_to[l], var_pal = var_palettes[[l]], polygons = polygons) %>%
      add_markers(var_id = var_ids_to[l], markers = markers_top, icon = icon_top) %>%
      add_markers(var_id = var_ids_to[l], markers = markers_flop, icon = icon_flop)
  }
  out = out %>%
    # Add layers control
    addLayersControl(
      options = layersControlOptions(collapsed = FALSE),
      baseGroups = var_ids_to
    ) %>%
    # Customize
    htmlwidgets::onRender(
      jsCode = "function() { $('.leaflet-control-layers').css({
    'border':'none',
    'background': 'rgba(255,255,255,0.8)',
    'box-shadow': '0 0 15px rgb(0 0 0 / 20%)'
    });}"
    ) %>%
    # Base group title
    htmlwidgets::onRender(
      jsCode = paste0("function() { $('.leaflet-control-layers-base').prepend('<label style=\"text-align:left\"><strong><font size=\"4\">", title_popup, "</font></strong><br>Risultati definitivi 25 settembre 2022</label>');}")
    ) %>%
    # Switch legends when a different base group is selected
    # Code from here: https://gist.github.com/noamross/98c2053d81085517e686407096ec0a69
    htmlwidgets::onRender(paste0("
      function(el, x) {
        var initialLegend = '", var_ids_to[1],"' // Set the initial legend to be displayed by layerId
        var myMap = this;
        for (var legend in myMap.controls._controlsById) {
          var el = myMap.controls.get(legend.toString())._container;
          if(legend.toString() === initialLegend) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          };
        };
      myMap.on('baselayerchange',
        function (layer) {
          for (var legend in myMap.controls._controlsById) {
            var el = myMap.controls.get(legend.toString())._container;
            if(legend.toString() === layer.name) {
              el.style.display = 'block';
            } else {
              el.style.display = 'none';
            };
          };
        });
      }")) %>%
    # Add information icon (model onclick)
    # Code from here: https://stackoverflow.com/questions/68995343/r-leaflet-adding-an-information-popup-using-easybutton
    addBootstrapDependency() %>%
    addEasyButton(
      easyButton(
        icon = "fa-info-circle", title = "Map Information",
        onClick = JS("function(btn, map){ $('#infobox').modal('show'); }"))) %>%
    htmlwidgets::appendContent(info_content)

  return(out)
}

