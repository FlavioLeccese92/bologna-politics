###--------------------------------------------------------------------------###
###                         POPULATION STATISTICS                           ####
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(cli)

options(cli.progress_bar_style = "fillsquares",
        cli.progress_show_after = 0)

quartieri = readRDS("data/general-porpuse/quartieri.rds")
zone = readRDS("data/general-porpuse/zone.rds")
aree_statistiche = readRDS("data/general-porpuse/aree_statistiche.rds")


###--------------------------###
####      demografiche      ####
###--------------------------###

#### popolazione_residente per area statistica ####

popolazione_residente_as = NULL

cli_progress_bar(
  total = length(2010:2021),
  format = "Downloading popolazione_residente_as {i}/{length(2010:2021)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
)
for(i in seq_along(2010:2021)){
  cli_progress_update(set = i)
  anno = (2010:2021)[i]

  for(j in seq_len(length(unique(quartieri$nome_quartiere)))){

    quartiere = unique(quartieri$nome_quartiere)[j]

    # res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=popolazione-residente-per-stato-civile-eta-sessocittadinanza-quartiere-e-zona-se&q=&rows=10000&sort=-anno&facet=anno&facet=cittadinanza&facet=eta_grandi&facet=eta_quinquennali&facet=quartiere&facet=zona&facet=centro_storico_zone_periferiche&facet=stato_civile&facet=sesso&refine.anno=2021&refine.quartiere=", quartiere))
    res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=popolazione-residente-per-eta-sesso-cittadinanza-quartiere-zona-area-statistica-&q=&rows=10000&sort=anno&facet=anno&facet=area_statistica&facet=quartiere&facet=zona&facet=sesso&facet=eta_grandi&facet=eta&facet=cittadinanza&refine.anno=", anno,"&refine.quartiere=", quartiere))

    x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
    x = x[["records"]][["fields"]] %>% as_tibble()
    x = remove_rownames(x)
    popolazione_residente_as = bind_rows(popolazione_residente_as, x)
  }
}
cli_progress_done(); rm(i, anno, j, quartiere, res, x)

popolazione_residente_as =
  popolazione_residente_as %>%
  mutate(id_area_statistica = as.numeric(codice_area_statistica), .keep = "unused") %>%
  select(id_area_statistica, anno, eta_grandi, eta, sesso, residenti)

saveRDS(popolazione_residente_as, "data/general-porpuse/popolazione_residente_as.rds")


#### popolazione_residente per zona ####

popolazione_residente_z = NULL

cli_progress_bar(
  total = length(2010:2021),
  format = "Downloading popolazione_residente_z {i}/{length(2010:2021)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
)
for(i in seq_along(2010:2021)){
  cli_progress_update(set = i)
  anno = (2010:2021)[i]

  for(j in seq_len(length(unique(quartieri$nome_quartiere)))){

    quartiere = unique(quartieri$nome_quartiere)[j]

    res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=popolazione-residente-per-stato-civile-eta-sessocittadinanza-quartiere-e-zona-se&q=&rows=10000&sort=-anno&facet=anno&facet=cittadinanza&facet=eta_grandi&facet=eta_quinquennali&facet=quartiere&facet=zona&facet=centro_storico_zone_periferiche&facet=stato_civile&facet=sesso&refine.anno=", anno, "&refine.quartiere=", quartiere))

    x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
    x = x[["records"]][["fields"]] %>% as_tibble()
    x = remove_rownames(x)
    popolazione_residente_z = bind_rows(popolazione_residente_z, x)
  }
}
cli_progress_done(); rm(i, anno, j, quartiere, res, x)

popolazione_residente_z =
  popolazione_residente_z %>%
  mutate(zona = zona %>% gsub("San|Santa", "S.", .) %>% gsub("S. D", "San D", .)) %>%
  left_join(zone %>% distinct(id_zona, nome_zona),
            by = c("zona" = "nome_zona")) %>%
  select(id_zona, anno, eta_grandi, eta_quinquennali, eta_singolo, sesso, stato_civile, residenti)

saveRDS(popolazione_residente_z, "data/general-porpuse/popolazione_residente_z.rds")

