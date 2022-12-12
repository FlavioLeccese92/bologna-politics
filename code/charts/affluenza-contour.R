library(dplyr)
library(tidyr)
library(sf)
library(lwgeom)
library(leafem)
library(stars)
library(leaflet)
library(viridis)
library(scales)
library(gstat)
library(zoo)

# http://132.72.155.230:3838/r/spatial-interpolation-of-point-data.html
select = dplyr::select

sezioni = readRDS("data/general-porpuse/sezioni.rds") %>%
  distinct(id_sezione, id_seggio, indirizzo)
sezioni_polygons = readRDS("data/polygons/sezioni_polygons.rds")
aree_statistiche_polygons = readRDS("data/polygons/aree_statistiche_polygons.rds")

sezioni_list = list()
raster_list = list()
contour_list = list()
centroids_list = list()
hexagon_list = list()

anno = 2020
votazione = "regionali"
organo = "consiglio_regionale"

organo_affluenza = readRDS(paste0("data/", anno, "-", votazione, "/", organo, "_affluenza.rds")) %>%
  mutate(affluenza = totale_votanti/iscritti) %>%
  select(id_sezione, affluenza) %>%
  mutate(affluenza = affluenza %>% ifelse(.>1, 1, .))

sezioni_polygons_value =
  sezioni_polygons %>%
  left_join(organo_affluenza, by = "id_sezione") %>%
  st_as_sf(sf_column_name = "geometry", crs = "EPSG:4326") %>%
  select(id_sezione, affluenza, geometry )%>%
  left_join(sezioni, by = "id_sezione")

sezioni_fill_value =
  st_difference(aree_statistiche_polygons %>%
                  summarise(geometry = st_union(geometry), .groups = "drop"),
                sezioni_polygons_value %>%
                  summarise(geometry = st_union(geometry), .groups = "drop")) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf(sf_column_name = "geometry", crs = "EPSG:4326") %>%
  mutate(affluenza = 0) %>%
  select(affluenza, geometry)

sezioni_polygons_value_full = bind_rows(sezioni_polygons_value, sezioni_fill_value) %>%
  st_set_crs("EPSG:4326")

sezioni_polygons_value_raster = sezioni_polygons_value_full %>% st_rasterize()
sezioni_polygons_value_stars = sezioni_polygons_value_raster %>% st_as_stars()

civici_sezioni_sf = readRDS("data/general-porpuse/civici_sezioni.rds") %>%
  left_join(organo_affluenza, by = "id_sezione") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") %>%
  select(affluenza, geometry)

centroids_sf = readRDS("data/polygons/centroids.rds") %>%
  left_join(organo_affluenza, by = "id_sezione") %>%
  st_as_sf(sf_column_name = "geometry", crs = "EPSG:4326") %>%
  select(affluenza, geometry)

g = gstat(formula = affluenza~1, locations = civici_sezioni_sf)
z = predict(g, sezioni_polygons_value_stars)

b = seq(0, 1, by = 0.01)
contour_sf = st_contour(z, breaks = b)

zpred = z["var1.pred",,]
names(zpred) = "affluenza"

hexagon = zpred %>%
  st_as_sf() %>%
  st_make_grid(cellsize = .003, square = FALSE) %>%
  st_as_sf() %>% rename(geometry = x) %>%
  mutate(hex_id = row_number()) %>%
  st_join(zpred %>% st_as_sf(), join = st_contains)  %>%
  filter(!is.na(affluenza)) %>%
  group_by(hex_id) %>%
  summarise(affluenza = mean(affluenza, na.rm = F), .groups = "drop") %>%
  select(affluenza)

hexagon$affluenza_label = paste0("Affluenza: <strong>",
                                 100*round(hexagon$affluenza, 4), "%</strong>") %>%
  lapply(htmltools::HTML)

sezioni_polygons_value$affluenza_label = paste0(
  "Sezione: <strong>", sezioni_polygons_value$id_sezione, "</strong><br>",
  "Seggio: <strong>", sezioni_polygons_value$id_seggio, "</strong><br>",
  "Indirizzo: <strong>", sezioni_polygons_value$indirizzo, "</strong><br>",
  "Affluenza: <strong>", 100*round(sezioni_polygons_value$affluenza, 4), "%</strong>") %>%
  lapply(htmltools::HTML)

sezioni_list = readRDS(paste0(pl, "data/polygons/affluenza/sezioni_list.rds"))
raster_list = readRDS(paste0(pl, "data/polygons/affluenza/raster_list.rds"))
contour_list = readRDS(paste0(pl, "data/polygons/affluenza/contour_list.rds"))
centroids_list = readRDS(paste0(pl, "data/polygons/affluenza/centroids_list.rds"))
hexagon_list = readRDS(paste0(pl, "data/polygons/affluenza/hexagon_list.rds"))

sezioni_list[[votazione]] = sezioni_polygons_value
raster_list[[votazione]] = zpred
contour_list[[votazione]] = contour_sf
centroids_list[[votazione]] = centroids_sf
hexagon_list[[votazione]] = hexagon

saveRDS(sezioni_list, "data/polygons/affluenza/sezioni_list.rds")
saveRDS(raster_list, "data/polygons/affluenza/raster_list.rds")
saveRDS(contour_list, "data/polygons/affluenza/contour_list.rds")
saveRDS(centroids_list, "data/polygons/affluenza/centroids_list.rds")
saveRDS(hexagon_list, "data/polygons/affluenza/hexagon_list.rds")


###### bkp #######
# organo_affluenza_full = NULL
#
# for(anno_i in c("2006", "2008", "2013", "2018", "2022")){
#   if(anno_i %in% c("2006", "2008", "2013", "2018", "2022")){
#     organo_affluenza_full =
#       bind_rows(organo_affluenza_full,
#                 readRDS(paste0("data/", anno_i, "-politiche/organo_affluenza.rds")) %>%
#                   mutate(affluenza = totale_votanti/iscritti,
#                          anno = anno_i) %>%
#                   select(id_sezione, anno, affluenza)
#       )
#   }else{
#     organo_affluenza_full =
#       bind_rows(organo_affluenza_full, tibble(id_sezione = 1:437, anno = anno_i, affluenza = NA))
#   }
# }
#
# organo_affluenza_full = organo_affluenza_full %>%
#   group_by(id_sezione) %>%
#   arrange(anno) %>%
#   mutate(affluenza = na.approx(affluenza)) %>%
#   ungroup()
