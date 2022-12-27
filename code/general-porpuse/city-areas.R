###--------------------------------------------------------------------------###
###                              CITY AREAS                                 ####
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyverse)
library(cli)
library(sf)

options(cli.progress_bar_style = "fillsquares",
        cli.progress_show_after = 0)

#### aree_statistiche ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=aree-statistiche&q=&rows=1000&sort=nome_area_statistica&facet=nome_area_statistica&facet=nome_quartiere&facet=nome_zona")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
aree_statistiche = x[["records"]][["fields"]]

aree_statistiche_raw = aree_statistiche %>%
  mutate(codice_quartiere = codice_quartiere %>% as.numeric()) %>%
  select(id_zona = codice_zona, id_quartiere = codice_quartiere) %>%
  distinct()

aree_statistiche = aree_statistiche %>%
  as_tibble() %>%
  unnest_wider(geo_shape) %>%
  select(id_area_statistica = codice_area_statistica,
         nome_area_statistica,
         coordinates,
         id_zona = codice_zona) %>%
  mutate(nome_area_statistica = nome_area_statistica %>% str_to_title(.)) %>%
  arrange(id_area_statistica, id_zona)

saveRDS(aree_statistiche, "data/general-porpuse/aree_statistiche.rds")

### aree_statistiche_sf ###
aree_statistiche_sf =
  aree_statistiche %>%
  group_by(id_area_statistica) %>%
  unnest(coordinates) %>%
  mutate(longitude = coordinates %>% .[[1]] %>% .[1,1,,1] %>% list(),
         latitude = coordinates %>% .[[1]] %>% .[1,1,,2] %>% list()) %>%
  unnest(c(latitude, longitude)) %>%
  select(id_area_statistica, longitude, latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("POLYGON")

saveRDS(aree_statistiche_sf, "data/polygons/aree_statistiche_polygons.rds")

#### zone ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=zone-del-comune-di-bologna&q=&rows=1000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
zone = x[["records"]][["fields"]]

zone = zone %>%
  as_tibble() %>%
  unnest_wider(geo_shape) %>%
  select(id_zona = codzona,
         nome_zona = nomezona,
         coordinates) %>%
  mutate(nome_zona = str_to_title(nome_zona)) %>%
  left_join(aree_statistiche_raw %>% distinct(id_zona, id_quartiere), by = "id_zona") %>%
  arrange(id_zona)

saveRDS(zone, "data/general-porpuse/zone.rds")

#### quartieri ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=quartieri-di-bologna&q=&rows=1000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
quartieri = x[["records"]][["fields"]]

quartieri = quartieri %>%
  as_tibble() %>%
  unnest_wider(geo_shape) %>%
  select(id_quartiere = numquart,
         nome_quartiere = nomequart,
         coordinates,
         perimeter,
         area) %>%
  arrange(id_quartiere)

saveRDS(quartieri, "data/general-porpuse/quartieri.rds")

#### archi_stradali ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=rifter_arcstra_li&q=&rows=10000&facet=nomequart&facet=data_istit")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
archi_stradali = x[["records"]][["fields"]]

archi_stradali = archi_stradali %>%
  as_tibble() %>%
  unnest_wider(geo_shape) %>%
  mutate(id_arco = codarco) %>%
  select(id_arco, coordinates) %>%
  arrange(id_arco)

saveRDS(archi_stradali, "data/general-porpuse/archi_stradali.rds")

archi_stradali_sf =
  archi_stradali %>%
  group_by(id_arco) %>%
  unnest(coordinates) %>%
  mutate(longitude = coordinates %>% .[[1]] %>% .[,1] %>% list(),
         latitude = coordinates %>% .[[1]] %>% .[,2] %>% list()) %>%
  unnest(c(latitude, longitude)) %>%
  select(id_arco, longitude, latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  group_by(id_arco) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")

saveRDS(archi_stradali_sf, "data/polygons/archi_stradali_polygon.rds")

### zone_coordinates ###
zone_coordinates = zone %>%
  unnest(coordinates) %>%
  group_by(id_zona) %>%
  mutate(longitude = coordinates %>% .[[1]] %>% .[1,1,,1] %>% list(),
         latitude = coordinates %>% .[[1]] %>% .[1,1,,2] %>% list()) %>%
  unnest(c(latitude, longitude)) %>%
  select(id_zona, longitude, latitude) %>%
  mutate(coordinates = paste0("(", latitude, ", ", longitude, ")")) %>%
  slice(seq(1, nrow(.), by = 4)) %>%
  summarise(coordinates = paste0(coordinates, collapse = ", "))

# saveRDS(zone_coordinates, "data/general-porpuse/zone_coordinates.rds")

#### edifici_particellari ####
# edifici_particellari = NULL
# for(i in 1:nrow(zone_temp)){
#   print(i)
#   res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=rifter_edif_pl&q=&rows=10000&sort=-data_istit&facet=tipologia&facet=data_istit&facet=data_varia&geofilter.polygon=",
#                    zone_temp$coordinates[i]))
#   x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
#   x = x[["records"]][["fields"]] %>% as_tibble()
#   x = remove_rownames(x)
#   edifici_particellari = bind_rows(edifici_particellari, x)
# }
# edifici_particellari = edifici_particellari %>% distinct() %>% rename(part_key = partkey)

# #### cigli_stradali ####
# cigli_stradali = NULL
# for(i in 1:nrow(zone_temp)){
#   print(i)
#   res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=carta-tecnica-comunale-cigli-stradali&q=&rows=10000&sort=-objectid&geofilter.polygon=",
#                    zone_temp$coordinates[i]))
#   x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
#   x = x[["records"]][["fields"]] %>% as_tibble()
#   x = remove_rownames(x)
#   cigli_stradali = bind_rows(cigli_stradali, x)
# }

#### aree_stradali ####
aree_stradali = NULL

cli_progress_bar(
  total = nrow(zone_coordinates),
  format = "Downloading aree_stradali {i}/{nrow(zone_coordinates)} {pb_bar} {pb_percent}"
)

for(i in seq_len(nrow(zone_coordinates))){

  cli_progress_update(set = i)

  res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=aree-stradali&q=&rows=10000&facet=descrizion&facet=origine&&geofilter.polygon=",
                   zone_coordinates$coordinates[i]))
  x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
  x = x[["records"]][["fields"]] %>% as_tibble()
  x = remove_rownames(x)
  aree_stradali = bind_rows(aree_stradali, x)
}

saveRDS(aree_stradali, "data/general-porpuse/aree_stradali.rds")

#### sezioni (2021) ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2021-seggi-elettorali&q=&rows=10000&facet=sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sezioni = x[["records"]][["fields"]]

sezioni = sezioni %>%
  mutate(id_seggio = row_number(),
         sez2011 = NA) %>%
  select(id_sezione = sezione,
         id_seggio,
         indirizzo,
         `Geo Point` = geo_point_2d,
         id_zona = zona) %>%
  distinct() %>%
  separate(`Geo Point`, c("latitude", "longitude"), ", ") %>%
  mutate(indirizzo = indirizzo %>% gsub(",", ", ", .),
         latitude = latitude %>% gsub("c\\(", "", .) %>% as.numeric(),
         longitude = longitude %>% gsub("\\)", "", .)  %>% as.numeric(),
         id_sezione = id_sezione %>% strsplit(., " - ")) %>%
  unnest(id_sezione) %>%
  mutate(id_sezione = id_sezione %>% as.numeric()) %>%
  arrange(id_sezione) %>%
  distinct(id_sezione, id_seggio, indirizzo, latitude, longitude)

### sezioni_sf
sezioni_sf = sezioni %>%
  distinct(id_sezione, id_seggio, latitude, longitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  st_cast("POINT")

ids_area_statistica = st_join(sezioni_sf, aree_statistiche_sf, join = st_intersects) %>%
  pull(id_area_statistica) %>% as.numeric()

if(length(which(is.na(ids_area_statistica)))>0){
  ids_area_statistica[which(is.na(ids_area_statistica))] =
    st_join(civici_sezioni_sf[which(is.na(ids_area_statistica)),], aree_statistiche_sf, join = st_nearest_feature) %>%
    pull(id_area_statistica) %>% as.numeric()
}

sezioni = sezioni %>% bind_cols(id_area_statistica = ids_area_statistica) %>%
  select(id_sezione, id_seggio, indirizzo, latitude, longitude, id_area_statistica) %>%
  arrange(id_sezione)

saveRDS(sezioni, "data/general-porpuse/sezioni.rds")

#### civici_sezioni ####

civici_sezioni = NULL

cli_progress_bar(
  total = nrow(zone_coordinates),
  format = "Downloading civici_sezioni {i}/{nrow(zone_coordinates)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
)

for(i in seq_len(nrow(zone_coordinates))){

  cli_progress_update(set = i)

  res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni2022_civici_sezioni&q=&rows=10000&geofilter.polygon=",
                   zone_coordinates$coordinates[i]))
  x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
  x = x[["records"]][["fields"]] %>% as_tibble()
  x = remove_rownames(x)
  civici_sezioni = bind_rows(civici_sezioni, x)
}
cli_progress_done()

civici_sezioni =
  civici_sezioni %>%
  select(id_civico = numprog,
         indirizzo,
         id_sezione = sez_elet,
         `Geo Point` = geo_point_2d) %>%
  distinct() %>%
  separate(`Geo Point`, c("latitude", "longitude"), ", ") %>%
  mutate(indirizzo = indirizzo %>% gsub(",", ", ", .) %>% gsub("\\s+", " ", .),
         latitude = latitude %>% gsub("c\\(", "", .) %>% as.numeric(),
         longitude = longitude %>% gsub("\\)", "", .)  %>% as.numeric(),
         id_sezione = id_sezione %>% as.numeric()) %>%
  arrange(id_civico)

### civici_sezioni_sf
civici_sezioni_sf = civici_sezioni %>%
  distinct(id_civico, latitude, longitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  st_cast("POINT")

ids_area_statistica = st_join(civici_sezioni_sf, aree_statistiche_sf, join = st_intersects) %>%
  pull(id_area_statistica) %>% as.numeric()

if(length(which(is.na(ids_area_statistica)))>0){
  ids_area_statistica[which(is.na(ids_area_statistica))] =
    st_join(civici_sezioni_sf[which(is.na(ids_area_statistica)),], aree_statistiche_sf, join = st_nearest_feature) %>%
    pull(id_area_statistica) %>% as.numeric()
}

civici_sezioni = civici_sezioni %>% bind_cols(id_area_statistica = ids_area_statistica) %>%
  select(id_civico, id_sezione, indirizzo, latitude, longitude, id_area_statistica = id_area_statistica)

### bind id_area_statistica della sezione relativa
civici_sezioni = civici_sezioni %>%
  left_join(sezioni %>% distinct(id_sezione, id_area_statistica) %>%
              rename(id_area_statistica_sezione = id_area_statistica), by = "id_sezione")

saveRDS(civici_sezioni, "data/general-porpuse/civici_sezioni.rds")

