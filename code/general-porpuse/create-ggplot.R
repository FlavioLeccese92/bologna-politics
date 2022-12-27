###--------------------------------------------------------------------------###
####                        polygon-transformation.R                        ####
###--------------------------------------------------------------------------###

library(dplyr)
library(sf)
library(cli)
library(ggplot2)

options(cli.progress_bar_style = "fillsquares",
        cli.progress_show_after = 0)

aree_statistiche_polygons = readRDS("data/polygons/aree_statistiche_polygons.rds")
aree_statistiche = readRDS("data/general-porpuse/aree_statistiche.rds")
zone_polygons = readRDS("data/polygons/zone_polygons.rds")
zone = readRDS("data/general-porpuse/zone.rds")
quartieri_polygons = readRDS("data/polygons/quartieri_polygons.rds")

#### aree_statistiche_polygons ####
cli_progress_bar(
  total = nrow(aree_statistiche_polygons),
  format = "Saving to png {i}/{nrow(aree_statistiche_polygons)} {pb_bar} {pb_percent}"
)

for(i in seq_len(nrow(aree_statistiche_polygons))){
  cli_progress_update(set = i)

  current_id_area_statistica = aree_statistiche_polygons %>% slice(i) %>% pull(id_area_statistica)
  current_id_zona = aree_statistiche  %>% filter(id_area_statistica == current_id_area_statistica) %>% pull(id_zona)
  current_id_aree_statistiche = aree_statistiche %>% filter(id_zona == current_id_zona) %>% pull(id_area_statistica)

  ggplot()  +
    geom_sf(data = aree_statistiche_polygons %>% filter(id_area_statistica %in% current_id_aree_statistiche),
            aes(geometry = geometry), fill = "white", color = "#15354a") +
    geom_sf(data = aree_statistiche_polygons %>% filter(id_area_statistica == current_id_area_statistica),
            fill = "#15354a", color = "#15354a") +
    theme_void()
  ggsave(paste0("data/polygons/images/aree_statisiche/", current_id_area_statistica, ".png")) %>%
    suppressMessages()

}
cli_progress_done()

#### zone_polygons ####
cli_progress_bar(
  total = nrow(zone_polygons),
  format = "Saving to png {i}/{nrow(zone_polygons)} {pb_bar} {pb_percent}"
)

for(i in seq_len(nrow(zone_polygons))){
  cli_progress_update(set = i)

  current_id_zona = zone_polygons %>% slice(i) %>% pull(id_zona)
  current_id_quartiere = zone %>% filter(id_zona == current_id_zona) %>% pull(id_quartiere)
  current_id_zone = zone %>% filter(id_quartiere == current_id_quartiere) %>% pull(id_zona)

  ggplot()  +
    geom_sf(data = zone_polygons %>% filter(id_zona %in% current_id_zone),
            aes(geometry = geometry), fill = "white", color = "#15354a") +
    geom_sf(data = zone_polygons %>% filter(id_zona == current_id_zona),
            fill = "#15354a", color = "#15354a") +
    theme_void()
  ggsave(paste0("data/polygons/images/zone/", current_id_zona, ".png")) %>%
    suppressMessages()

}
cli_progress_done()


#### quartieri_polygons ####
cli_progress_bar(
  total = nrow(quartieri_polygons),
  format = "Saving to png {i}/{nrow(quartieri_polygons)} {pb_bar} {pb_percent}"
)

for(i in seq_len(nrow(quartieri_polygons))){
  cli_progress_update(set = i)

  current_id_quartiere = quartieri_polygons %>% slice(i) %>% pull(id_quartiere)

  ggplot() +
    geom_sf(data = quartieri_polygons, fill = "transparent", color = "#15354a") +
    geom_sf(data = quartieri_polygons %>% filter(id_quartiere == current_id_quartiere), fill = "#15354a", color = "#15354a") +
    theme_void()
  ggsave(paste0("data/polygons/images/quartieri/", current_id_quartiere, ".png")) %>%
    suppressMessages()

}
cli_progress_done()
