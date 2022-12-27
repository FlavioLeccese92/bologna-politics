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

pop_as_eta_sesso = NULL

cli_progress_bar(
  total = length(2010:2021),
  format = "Downloading pop_as_eta_sesso {i}/{length(2010:2021)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
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
    pop_as_eta_sesso = bind_rows(pop_as_eta_sesso, x)
  }
}
cli_progress_done(); rm(i, anno, j, quartiere, res, x)

pop_as_eta_sesso =
  pop_as_eta_sesso %>%
  mutate(id_area_statistica = as.numeric(codice_area_statistica), .keep = "unused") %>%
  select(id_area_statistica, anno, eta_grandi, eta, sesso, residenti)

saveRDS(pop_as_eta_sesso, "data/general-porpuse/pop_as_eta_sesso.rds")

#### popolazione_residente per zona ####

pop_z_eta_sesso_civile = NULL

cli_progress_bar(
  total = length(2010:2021),
  format = "Downloading pop_z_eta_sesso_civile {i}/{length(2010:2021)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
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
    pop_z_eta_sesso_civile = bind_rows(pop_z_eta_sesso_civile, x)
  }
}
cli_progress_done(); rm(i, anno, j, quartiere, res, x)

pop_z_eta_sesso_civile =
  pop_z_eta_sesso_civile %>%
  mutate(zona = zona %>% gsub("San|Santa", "S.", .) %>% gsub("S. D", "San D", .)) %>%
  left_join(zone %>% distinct(id_zona, nome_zona),
            by = c("zona" = "nome_zona")) %>%
  select(id_zona, anno, eta_grandi, eta_quinquennali, eta_singolo, sesso, stato_civile, residenti)

saveRDS(pop_z_eta_sesso_civile, "data/general-porpuse/pop_z_eta_sesso_civile.rds")

#### reddito per area_statistica ####

reddito_as = NULL

cli_progress_bar(
  total = length(2009:2020),
  format = "Downloading reddito_as {i}/{length(2009:2020)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
)
for(i in seq_along(2009:2020)){
  cli_progress_update(set = i)
  anno = (2009:2020)[i]

  res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=redditi-per-area-statistica&q=&sort=anno_reddito&facet=anno_reddito&facet=area_statistica&refine.anno_reddito=", anno))

  x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
  x = x[["records"]][["fields"]] %>% as_tibble()
  x = remove_rownames(x)
  reddito_as = bind_rows(reddito_as, x)

}
cli_progress_done(); rm(i, anno, res, x)

reddito_as =
  reddito_as %>%
  mutate(nome_area_statistica = tolower(area_statistica)) %>%
  left_join(aree_statistiche %>%
              distinct(id_area_statistica, nome_area_statistica) %>%
              mutate(nome_area_statistica = tolower(nome_area_statistica)),
            by = c("nome_area_statistica" = "nome_area_statistica")) %>%
  select(id_area_statistica, anno = anno_reddito,
         n_contribuenti, reddito_imponibile_ai_fini_dell_addizionale_irpef, reddito_imponibile_ai_fini_irpef) %>%
  arrange(id_area_statistica, anno)

saveRDS(reddito_as, "data/general-porpuse/reddito_as.rds")

#### reddito mediano per area_statistica ####

reddito_mediano_as = NULL

cli_progress_bar(
  total = length(2016:2020),
  format = "Downloading reddito_mediano_as {i}/{length(2016:2020)} {pb_bar} {pb_percent} [{pb_elapsed_clock}]"
)
for(i in seq_along(2016:2020)){
  cli_progress_update(set = i)
  anno = (2016:2020)[i]

  res = GET(paste0("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=reddito-mediano-per-area-statistica&q=&sort=-anno&facet=anno&facet=area_statistica&refine.anno=", anno))

  x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
  x = x[["records"]][["fields"]] %>% as_tibble()
  x = remove_rownames(x)
  reddito_mediano_as = bind_rows(reddito_mediano_as, x)

}
cli_progress_done(); rm(i, anno, res, x)

reddito_mediano_as =
  reddito_mediano_as %>%
  mutate(nome_area_statistica = tolower(area_statistica)) %>%
  left_join(aree_statistiche %>%
              distinct(id_area_statistica, nome_area_statistica) %>%
              mutate(nome_area_statistica = tolower(nome_area_statistica)),
            by = c("nome_area_statistica" = "nome_area_statistica")) %>%
  select(id_area_statistica, anno,
         numero_contribuenti_residenti,
         reddito_imponibile_mediano_dei_contribuenti_residenti) %>%
  arrange(id_area_statistica, anno)

saveRDS(reddito_mediano_as, "data/general-porpuse/reddito_mediano_as.rds")
