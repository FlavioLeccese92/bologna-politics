###--------------------------------------------------------------------------###
####                             POLITICHE 2008                             ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### senato_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-voti-liste-senato-della-repubblica&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato = x[["records"]][["fields"]]

senato_voti = senato %>%
  as_tibble() %>%
  select(id_sezione = sezione, zona, quartiere,
         starts_with("percentuale"), everything()) %>%
  pivot_longer(cols = names(.)[18:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione,
         nome_lista,
         voti_validi) %>%
  arrange(id_sezione, nome_lista)

saveRDS(senato_voti, "data/2008-politiche/senato_voti.rds")

#### senato_affluenza ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato_affluenza = x[["records"]][["fields"]]

senato_affluenza = senato_affluenza %>%
  mutate(totale_votanti = votanti_maschi + votanti_femmine,
         iscritti = elettori_maschi + elettori_femmine) %>%
  select(id_sezione = sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(senato_affluenza, "data/2008-politiche/senato_affluenza.rds")

#### camera_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-voti-liste-camera-dei-deputati&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera = x[["records"]][["fields"]]

camera_voti = camera %>%
  as_tibble() %>%
  select(id_sezione = sezione, zona, quartiere,
         starts_with("percentuale"), everything()) %>%
  pivot_longer(cols = names(.)[19:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione,
         nome_lista,
         voti_validi) %>%
  # mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(id_sezione, nome_lista)

saveRDS(camera_voti, "data/2008-politiche/camera_voti.rds")

#### camera_affluenza ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-camera-dei-deputati&q=&rows=10000&sort=-sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_affluenza = x[["records"]][["fields"]]

camera_affluenza = camera_affluenza %>%
  mutate(totale_votanti = votanti_maschi + votanti_femmine) %>%
  select(id_sezione = sezione, iscritti = elettori, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(camera_affluenza, "data/2008-politiche/camera_affluenza.rds")
