###--------------------------------------------------------------------------###
####                              EUROPEE 2019                              ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### parlamento_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-europee-2019-preferenze-alle-liste&q=&rows=10000")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
parlamento = x[["records"]][["fields"]]

parlamento_voti = parlamento %>%
  as_tibble() %>%
  select(sezione_elettorale, iscritti,
         totale_votanti, totale_voti_di_cui, totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate, differenza,
         schede_bianche_757, schede_nulle_2416, contestate_non_assegnate_15, zona, quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[16:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale,
         nome_lista,
         voti_validi) %>%
  mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(sezione_elettorale, nome_lista)

saveRDS(parlamento_voti, "data/2019-europee/parlamento_voti.rds")

#### parlamento_affluenza ####

parlamento_affluenza = parlamento %>%
  select(sezione_elettorale, iscritti, totale_votanti, totale_voti_validi)


saveRDS(parlamento_affluenza, "data/2019-europee/parlamento_affluenza.rds")
