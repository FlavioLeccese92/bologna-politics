###--------------------------------------------------------------------------###
####                             REGIONALI 2020                             ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### consiglio_regionale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-regionali-26-gennaio-2020-voti-alle-liste&q=&rows=10000&sort=-ni_sez&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_regionale = x[["records"]][["fields"]]

consiglio_regionale_voti = consiglio_regionale %>%
  as_tibble() %>%
  select(sezione_elettorale = ni_sez, totale_iscritti, ni_iscr_m, ni_iscr_f,
         totale_voti_validi_liste,
         schede_nulle, schede_bianche, schede_contestate_e_non_attribuite, zona, quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[11:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale,
         nome_lista,
         voti_validi)
  # mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%

saveRDS(consiglio_regionale_voti, "data/2020-regionali/consiglio_regionale_voti.rds")

#### consiglio_regionale_affluenza ####

consiglio_regionale_affluenza = consiglio_regionale %>%
  mutate(totale_votanti = totale_voti_validi_liste + schede_nulle + schede_bianche + schede_contestate_e_non_attribuite) %>%
  select(sezione_elettorale = ni_sez, iscritti = totale_iscritti, totale_votanti, totale_voti_validi = totale_voti_validi_liste)

saveRDS(consiglio_regionale_affluenza, "data/2020-regionali/consiglio_regionale_affluenza.rds")


#### presidente_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-regionali-26-gennaio-2020-voti-ai-presidenti-candidati&q=&rows=10000&sort=-ni_sez")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
presidente = x[["records"]][["fields"]]

presidente_voti = presidente %>%
  as_tibble() %>%
  select(sezione_elettorale = ni_sez, totale_iscritti, totale_votanti, percentuale_votanti,
         nulle, bianche, contestate_e_non_attribuite, zona, quartiere,
         starts_with("di_cui_"),
         everything()) %>%
  pivot_longer(cols = names(.)[17:ncol(.)], names_to = "nome_candidato", values_to = "voti_validi") %>%
  select(nome_candidato,
         voti_validi)
  # mutate(nome_candidato = nome_candidato %>% gsub("_[^_]*$", "", .)) %>%

saveRDS(presidente_voti, "data/2020-regionali/presidente_voti.rds")

#### presidente_affluenza ####
presidente_affluenza = presidente %>%
  mutate(totale_voti_validi = totale_votanti - nulle + bianche - contestate_e_non_attribuite) %>%
  select(sezione_elettorale = ni_sez, iscritti = totale_iscritti, totale_votanti, totale_voti_validi)

saveRDS(presidente_affluenza, "data/2020-regionali/presidente_affluenza.rds")
