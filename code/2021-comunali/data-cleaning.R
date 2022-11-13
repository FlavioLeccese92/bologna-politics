###--------------------------------------------------------------------------###
####                             COMUNALI 2021                              ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

seggi = readRDS("data/general-porpuse/seggi.rds")

###------------------###
####    RISULTATI   ####
###------------------###

#### consiglio_comunale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2021-voti-alle-liste-per-il-consiglio-comunale&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale = x[["records"]][["fields"]]

consiglio_comunale_voti = consiglio_comunale %>%
  as_tibble() %>%
  select(sezione, iscritti, zona, quartiere,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza,
         everything()) %>%
  pivot_longer(cols = names(.)[13:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_lista,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_lista) %>%
  left_join(seggi %>% distinct(sezione_elettorale, id_zona, id_quartiere), by = "sezione_elettorale")

saveRDS(consiglio_comunale_voti, "data/2021-comunali/consiglio_comunale_voti.rds")

#### consiglio_comunale_affluenza ####

consiglio_comunale_affluenza = consiglio_comunale %>%
  select(sezione_elettorale = sezione, iscritti, totale_votanti, totale_voti_validi) %>%
  left_join(seggi %>% distinct(sezione_elettorale, id_zona, id_quartiere), by = "sezione_elettorale")

saveRDS(consiglio_comunale_affluenza, "data/2021-comunali/consiglio_comunale_affluenza.rds")

#### sindaco_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2021-risultati-votazioni-candidati-sindaci&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sindaco = x[["records"]][["fields"]]

sindaco_voti = sindaco  %>%
  as_tibble() %>%
  select(sezione, zona, quartiere,
         totale_votanti, totale_voti_validi, totale_voti_non_validi,
         nulle, bianche, contestate = contest, differenza, solo_sindaco,
         everything()) %>%
  pivot_longer(cols = names(.)[12:ncol(.)], names_to = "nome_candidato", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_candidato,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_candidato) %>%
  left_join(seggi %>% distinct(sezione_elettorale, id_zona, id_quartiere), by = "sezione_elettorale")

saveRDS(sindaco_voti, "data/2021-comunali/sindaco_voti.rds")

#### sindaco_affluenza ####

sindaco_affluenza = sindaco %>%
  select(sezione_elettorale = sezione, totale_votanti, totale_voti_validi) %>%
  left_join(seggi %>% distinct(sezione_elettorale, id_zona, id_quartiere), by = "sezione_elettorale") %>%
  left_join(consiglio_comunale_affluenza %>% distinct(sezione_elettorale, iscritti), by = "sezione_elettorale") %>%
  relocate(iscritti, .after = "sezione_elettorale")

saveRDS(sindaco_affluenza, "data/2021-comunali/sindaco_affluenza.rds")

### preferenze_CC ###

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2021-preferenze-lista-coalizione-civica-per-bologna-cora&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
preferenze_CC = x[["records"]][["fields"]]

preferenze_CC = preferenze_CC %>%
  as_tibble() %>%
  select(sezione, iscritti, zona, quartiere, nome_lista, everything()) %>%
  pivot_longer(cols = names(.)[6:ncol(.)], names_to = "candidato", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_candidato = candidato,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_candidato) %>%
  left_join(seggi %>% distinct(sezione_elettorale, id_zona, id_quartiere), by = "sezione_elettorale")

rm(x); rm(res)

