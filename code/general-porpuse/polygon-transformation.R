###--------------------------------------------------------------------------###
####                        polygon-transformation.R                        ####
###--------------------------------------------------------------------------###

library(dplyr)
library(tidyr)
library(sf)
library(lwgeom)
library(cli)
library(purrr)
library(leaflet)

options(cli.progress_bar_style = "fillsquares",
        cli.progress_show_after = 0)

zone = readRDS("data/general-porpuse/zone.rds")
civici_sezioni = readRDS("data/general-porpuse/civici_sezioni.rds")
aree_stradal = readRDS("data/polygons/aree_stradali.rds")

### aree_stradali_polygons ###
aree_stradali_polygons = NULL

options(cli.progress_bar_style = "fillsquares")
cli_progress_bar(
  total = nrow(aree_stradali),
  format = "Converting to MULTIPOLYGON {i}/{nrow(aree_stradali)} {pb_bar} {pb_percent}"
)

for(i in seq_len(nrow(aree_stradali))){
  codice_ogg = aree_stradali[i, ]$codice_ogg

  cli_progress_update(set = i)

  n_polygons = vec_depth(aree_stradali[i, ]$geo_shape$coordinates[[1]])

  inner_temp = NULL
  if(n_polygons == 1){
    subpolygons = dim(aree_stradali[i, ]$geo_shape$coordinates[[1]][,,1])
    if(is.null(subpolygons)){
      inner_temp =
        tibble(
          codice_ogg = aree_stradali[i, ]$codice_ogg,
          subcodice_ogg = 1,
          longitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][,,1],
          latitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][,,2]) %>%
        st_as_sf(., coords = c("longitude", "latitude")) %>%
        group_by(codice_ogg, subcodice_ogg) %>%
        summarise(geometry = st_combine(geometry), .groups = "drop") %>%
        st_cast("POLYGON")
    }else{
      for(j in seq_len(subpolygons[1])){
        inner_temp =
          tibble(
            codice_ogg = aree_stradali[i, ]$codice_ogg,
            subcodice_ogg = j,
            longitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][,,1][j,],
            latitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][,,2][j,]) %>%
          st_as_sf(., coords = c("longitude", "latitude")) %>%
          group_by(codice_ogg, subcodice_ogg) %>%
          summarise(geometry = st_combine(geometry), .groups = "drop") %>%
          st_cast("POLYGON") %>%
          bind_rows(inner_temp, .)
      }
    }
  }else{
    for(j in seq_len(n_polygons)){
      inner_temp =
        tibble(
          codice_ogg = aree_stradali[i, ]$codice_ogg,
          subcodice_ogg = j,
          longitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][[j]][,1],
          latitude = aree_stradali[i, ]$geo_shape$coordinates[[1]][[j]][,2])%>%
        st_as_sf(., coords = c("longitude", "latitude")) %>%
        group_by(codice_ogg, subcodice_ogg) %>%
        summarise(geometry = st_combine(geometry), .groups = "drop") %>%
        st_cast("POLYGON") %>%
        bind_rows(inner_temp, .)
    }
  }
  inner_temp = inner_temp %>%
    ungroup() %>%
    group_by(codice_ogg) %>% summarise(geometry = st_combine(geometry)) %>%
    st_cast("MULTIPOLYGON")

  aree_stradali_polygons =
    bind_rows(aree_stradali_polygons,
              inner_temp)
}
cli_progress_done()

saveRDS(aree_stradali_polygons, "data/polygons/aree_stradali_polygons.rds")

### zone_sf ###
zone_sf = zone %>%
  group_by(id_zona) %>%
  unnest(coordinates) %>%
  mutate(longitude = coordinates %>% .[[1]] %>% .[1,1,,1] %>% list(),
         latitude = coordinates %>% .[[1]] %>% .[1,1,,2] %>% list()) %>%
  unnest(c(latitude, longitude)) %>%
  select(id_zona, longitude, latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("POLYGON")

### civici_sezioni_sf
civici_sezioni_sf = civici_sezioni %>%
  distinct(id_civico, latitude, longitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  st_cast("POINT")

### zone_no_streets_polygons ###
zone_no_streets_polygons = aree_stradali_polygons %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_difference(zone_sf, .) %>%
  select(geometry) %>%  st_cast("POLYGON") %>%
  mutate(id_isolato = row_number())

### civici_sezioni_zone ###
isolato_indeces = st_intersects(civici_sezioni_sf, zone_no_streets_polygons, sparse = TRUE) %>% as.numeric()

civici_sezioni_zone = civici_sezioni_sf %>%
  st_set_geometry(zone_no_streets_polygons[isolato_indeces,]$geometry) %>%
  bind_cols(id_isolato = zone_no_streets_polygons[isolato_indeces,]$id_isolato)

### polygon_finale ###
polygon_finale = NULL

n_isolati = length(unique(civici_sezioni_zone$id_isolato))

cli_progress_bar(
  total = n_isolati,
  format = "Converting to MULTIPOLYGON {i}/{n_isolati} {pb_bar} {pb_percent}"
)

for(i in 1:n_isolati){

  cli_progress_update(set = i)

  id_isolato_corrente = unique(civici_sezioni_zone$id_isolato)[i]
  civici_sezioni_zone_i = civici_sezioni_zone %>% filter(id_isolato == id_isolato_corrente)
  id_civici_isolato = civici_sezioni_zone_i %>% pull(id_civico)

  polygon_corrente = civici_sezioni_sf %>% filter(id_civico %in% id_civici_isolato) %>%
    st_union() %>%
    st_voronoi() %>%
    st_collection_extract() %>%
    st_intersection(zone_no_streets_polygons %>% filter(id_isolato == id_isolato_corrente))

  polygon_id_corrente = st_intersects(civici_sezioni_sf %>% filter(id_civico %in% id_civici_isolato),
                                      polygon_corrente, sparse = TRUE) %>% as.numeric()

  polygon_finale = bind_rows(polygon_finale,
                             tibble(id_civico = id_civici_isolato) %>%
                               st_set_geometry(polygon_corrente[polygon_id_corrente]))
}
cli_progress_done()

### sezioni_polygons  ##
sezioni_polygons = civici_sezioni %>%
  select(id_civico, id_sezione) %>%
  inner_join(polygon_finale, by = "id_civico") %>%
  group_by(id_sezione) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf() %>% st_cast("MULTIPOLYGON")

### centroids ###
centroids = sezioni_polygons %>%
  st_centroid() %>%
  group_by(id_sezione) %>%
  slice(n = 1) %>% ungroup()

saveRDS(sezioni_polygons, "data/polygons/sezioni_polygons.rds")
saveRDS(centroids, "data/polygons/centroids.rds")
