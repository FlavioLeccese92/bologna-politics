###--------------------------------------------------------------------------###
####                             POLITICHE 2018                             ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

sezioni = readRDS("data/general-porpuse/sezioni.rds")

###------------------###
####    RISULTATI   ####
###------------------###

#### senato_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2018-voti-alle-liste-senato-della-repubblica&q=&rows=10000")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato = x[["records"]][["fields"]]

senato_voti = senato %>%
  as_tibble() %>%
  select(sezione_elettorale, iscritti_al_voto,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza,
         everything()) %>%
  pivot_longer(cols = names(.)[11:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione = sezione_elettorale,
         nome_lista,
         voti_validi) %>%
  mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(id_sezione, nome_lista)

saveRDS(senato_voti, "data/2018-politiche/senato_voti.rds")

#### senato_affluenza ####
senato_affluenza = senato %>%
  select(id_sezione = sezione_elettorale, iscritti = iscritti_al_voto, totale_votanti, totale_voti_validi)

saveRDS(senato_affluenza, "data/2018-politiche/senato_affluenza.rds")

#### camera_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2018-voti-liste-camera-dei-deputati-collegio-n-6&q=&rows=10000&sort=-sezione_elettorale")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_c6 = x[["records"]][["fields"]]

camera_c6_voti = camera_c6 %>%
  as_tibble() %>%
  select(sezione_elettorale, iscritti,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza,
         everything()) %>%
  pivot_longer(cols = names(.)[11:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione = sezione_elettorale,
         nome_lista,
         voti_validi) %>%
  mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(id_sezione, nome_lista)


saveRDS(camera_c6_voti, "data/2018-politiche/camera06_voti.rds")

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2018-voti-liste-camera-dei-deputati-collegio-n-7&q=&rows=10000&sort=-sezione_elettorale")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_c7 = x[["records"]][["fields"]]

camera_c7_voti = camera_c7 %>%
  as_tibble() %>%
  select(sezione_elettorale, iscritti,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza,
         everything()) %>%
  pivot_longer(cols = names(.)[11:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione = sezione_elettorale,
         nome_lista,
         voti_validi) %>%
  mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(id_sezione, nome_lista)

saveRDS(camera_c7_voti, "data/2018-politiche/camera07_voti.rds")

camera_voti = bind_rows(camera_c6_voti,
                        camera_c7_voti) %>%
  group_by(id_sezione, nome_lista) %>%
  summarise(voti_validi = sum(voti_validi), .groups = "drop")

saveRDS(camera_voti, "data/2018-politiche/camera_voti.rds")

#### camera_affluenza ####

camera_affluenza = bind_rows(camera_c6,
                             camera_c7) %>%
  select(id_sezione = sezione_elettorale, iscritti, totale_votanti, totale_voti_validi)

saveRDS(camera_affluenza, "data/2018-politiche/camera_affluenza.rds")
