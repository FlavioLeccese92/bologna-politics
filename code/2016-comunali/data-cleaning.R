###--------------------------------------------------------------------------###
####                             COMUNALI 2016                              ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### consiglio_comunale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2016-voti-alle-liste-per-il-consiglio-comunale&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale = x[["records"]][["fields"]]

consiglio_comunale_voti = consiglio_comunale %>%
  as_tibble() %>%
  select(sezione, iscritti, zona, quartiere,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza, vecchia_denominazione_quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[14:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione = sezione,
         nome_lista,
         voti_validi) %>%
  arrange(id_sezione, nome_lista)

saveRDS(consiglio_comunale_voti, "data/2016-comunali/consiglio_comunale_voti.rds")

#### consiglio_comunale_affluenza ####

consiglio_comunale_affluenza = consiglio_comunale %>%
  select(id_sezione = sezione, iscritti, totale_votanti, totale_voti_validi)

saveRDS(consiglio_comunale_affluenza, "data/2016-comunali/consiglio_comunale_affluenza.rds")

#### sindaco_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2016-risultati-votazioni-candidati-sindaci&q=&rows=1000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sindaco = x[["records"]][["fields"]]

sindaco_voti = sindaco  %>%
  as_tibble() %>%
  select(sezione, zona, quartiere,
         totale_votanti, totale_voti_validi, totale_voti_non_validi,
         nulle, bianche, contestate = contest, differenza, solo_sindaco, vecchia_denominazione_quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[13:ncol(.)], names_to = "nome_candidato", values_to = "voti_validi") %>%
  select(id_sezione = sezione,
         nome_candidato,
         voti_validi) %>%
  arrange(id_sezione, nome_candidato)

saveRDS(sindaco_voti, "data/2016-comunali/sindaco_voti.rds")

#### sindaco_affluenza ####

sindaco_affluenza = sindaco %>%
  select(id_sezione = sezione, totale_votanti, totale_voti_validi) %>%
  left_join(consiglio_comunale_affluenza %>% distinct(id_sezione, iscritti), by = "id_sezione") %>%
  relocate(iscritti, .after = "id_sezione")

saveRDS(sindaco_affluenza, "data/2016-comunali/sindaco_affluenza.rds")

