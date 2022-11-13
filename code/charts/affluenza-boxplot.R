library(echarts4r)

#### Funzioni ####

.build_data2 = function(data, ...) {
  require(dplyr)
  row.names(data) = NULL
  data = data %>% select(...)
  data = unname(data)
  out = as.list(as.data.frame(t(data))) %>% unname()
  return(out)
}

.build_data_jitter = function(data, ..., factor = 0, amount = NULL, on = c("x", "y")) {
  require(dplyr)
  row.names(data) = NULL
  data = data %>% select(...)
  names(data)[1:2] = c("x", "y")
  if("x" %in% on){data = data %>% mutate(x = jitter(x, factor, amount))}
  if("y" %in% on){data = data %>% mutate(y = jitter(y, factor, amount))}
  data = unname(data)
  out = as.list(as.data.frame(t(data))) %>% unname()
}

#### Anagrafiche ####
seggi = readRDS("data/general-porpuse/seggi.rds")

#### Affluenza ####

affluenza = bind_rows(
  #### 2004-comunali ####
  readRDS("data/2004-comunali/sindaco_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2004 - Comunali"),
  #### 2006-poliche ####
  readRDS("data/2006-politiche/camera_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2006 - Politiche"),
  #### 2008-poliche ####
  readRDS("data/2008-politiche/camera_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2008 - Politiche"),
  #### 2009-europee ####
  readRDS("data/2009-europee/parlamento_liste_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2009 - Europee"),
  #### 2009-comunali ####
  readRDS("data/2009-comunali/sindaco_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2009 - Comunali"),
  #### 2011-comunali ####
  readRDS("data/2011-comunali/consiglio_comunale_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2011 - Comunali"),
  #### 2013-poliche ####
  readRDS("data/2013-politiche/camera_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2013 - Politiche"),
  #### 2014-europee ####
  readRDS("data/2014-europee/parlamento_liste_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2014 - Europee"),
  #### 2016-comunali ####
  readRDS("data/2016-comunali/consiglio_comunale_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2016 - Comunali"),
  #### 2018-poliche ####
  readRDS("data/2018-politiche/camera_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2018 - Politiche"),
  #### 2019-europee ####
  readRDS("data/2019-europee/parlamento_liste_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2019 - Europee"),
  #### 2020-regionali ####
  readRDS("data/2020-regionali/consiglio_regionale_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2020 - Regionali"),
  #### 2021-comunali ####
  readRDS("data/2021-comunali/consiglio_comunale_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2021 - Comunali"),
  #### 2022-politiche ####
  readRDS("data/2022-politiche/camera_affluenza.rds") %>%
    select(sezione_elettorale, totale_votanti, iscritti) %>%
    mutate(affluenza = ifelse(iscritti > 0, totale_votanti/iscritti, NA),
           votazione = "2022 - Politiche")
) %>% select(votazione, sezione_elettorale, totale_votanti, iscritti, affluenza)%>%
  left_join(seggi %>% distinct(id_seggio, sezione_elettorale, indirizzo) %>%
              mutate(indirizzo = indirizzo %>% str_to_title()),
            by = "sezione_elettorale")  %>%
  mutate(id_seggio = id_seggio %>% ifelse(is.na(.), 0, .),
        indirizzo = indirizzo %>% ifelse(is.na(.), "Indirizzo fittizio", .))

affluenza$votazione = factor(affluenza$votazione,
                             levels = unique(affluenza$votazione))

affluenza_seggi = affluenza %>%
  group_by(votazione, id_seggio, indirizzo) %>%
  summarise(totale_votanti = sum(totale_votanti),
            iscritti = sum(iscritti, na.rm = T),
            affluenza = totale_votanti/iscritti %>% ifelse(iscritti == 0, NA, .),
            sezioni_elettorali = paste0(sezione_elettorale, collapse = " - "),
            .groups = "drop")

### data ###
affluenza_bologna_data = affluenza %>%
  group_by(votazione) %>%
  summarise(totale_votanti = sum(totale_votanti, na.rm = T),
            iscritti = sum(iscritti, na.rm = T),
            affluenza = round(100*totale_votanti/iscritti, 2),
            .groups = "drop") %>%
  select(votazione, affluenza, totale_votanti, iscritti) %>%
  mutate(totale_votanti = totale_votanti %>% formatC(., format = "f", big.mark = ",", digits=0),
         iscritti = iscritti %>% formatC(., format = "f", big.mark = ",", digits=0))

affluenza_italia_data = tibble(votazione = affluenza_bologna_data$votazione,
                               affluenza = c(NA, 83.62, 80.51, 66.46, NA, NA, 75.20, 58.69, NA, 72.94, 56.09, NA, NA, 63.8),
                               totale_votanti = c(NA, 39382430, 37954253, 32659162, NA, NA, 35348095, 28908004, NA, 33995268, 27652929	, NA, NA, 29415082),
                               iscritti = c(NA, 47098181, 47142437, 49135080, NA, NA, 47005431, 49256169, NA, 46604897, 49301157	, NA, NA, 46120143)) %>%
  mutate(totale_votanti = totale_votanti %>% formatC(., format = "f", big.mark = ",", digits=0),
         iscritti = iscritti %>% formatC(., format = "f", big.mark = ",", digits=0))

affluenza_boxplot_data = affluenza_seggi %>%
  filter(!is.na(affluenza)) %>%
  group_by(votazione) %>%
  summarise(enframe(quantile(affluenza, c(0, 0.25, 0.5, 0.75, 1)), "quantile", "affluenza"),
            .groups = "drop") %>%
  mutate(affluenza = 100*(round(affluenza, 4)))

affluenza_scatter_data = affluenza_seggi %>%
  filter(!is.na(affluenza)) %>%
  mutate(affluenza = 100*round(affluenza, 4),
         votazione = votazione %>% as.factor() %>% as.numeric() - 0.5) %>%
  select(votazione, affluenza, indirizzo, totale_votanti, iscritti, sezioni_elettorali, id_seggio)

### data_list ###

affluenza_bologna_data_list = affluenza_bologna_data %>%
  mutate(votazione = votazione %>% as.factor() %>% as.numeric() - 0.5) %>%
  .build_data2(., votazione, affluenza, totale_votanti, iscritti)

affluenza_italia_data_list = affluenza_italia_data %>%
  mutate(votazione = votazione %>% as.factor() %>% as.numeric() - 0.5) %>%
  .build_data2(., votazione, affluenza, totale_votanti, iscritti)

category_data_list = affluenza_boxplot_data %>% distinct(votazione) %>% pull()

affluenza_boxplot_data_list = affluenza_boxplot_data %>%
  pivot_wider(names_from = votazione, values_from = affluenza) %>%
  select(-quantile) %>% as.list() %>% unname()

series = list()

series[[1]] = list(name = "Affluenza Bologna",
                   type = "line", data = affluenza_bologna_data_list,
                   symbolSize = 18, symbol = "triangle",
                   lineStyle = list(width = 1, type = "dotted", color = "#AB2525"),
                   itemStyle = list(opacity = 0.5, color = "#AB2525"),
                   emphasis = list(focus = 'series'), smooth = TRUE,
                   xAxisIndex = 1, zlevel = 3,
                   tooltip = list(
                     textStyle = list(width = 20, overflow = "break"),
                     formatter = htmlwidgets::JS(paste0("
                           function (params) {return (
                            '<strong>Bologna</strong>' +
                            '<br />Affluenza <strong> ' + params.data[1] + '%' + '</strong>' +
                            '<br />Votanti <strong> ' + params.data[2] + ' su ' + params.data[3] + '</strong>'
                         );}"
                     ))))

series[[2]] = list(name = "Affluenza Italia",
                   type = "line", data = affluenza_italia_data_list,
                   symbolSize = 18, symbol = "diamond",
                   lineStyle = list(width = 1, type = "dotted", color = "#444655"),
                   itemStyle = list(opacity = 0.5, color = "#444655"),
                   emphasis = list(focus = 'series'), smooth = TRUE,
                   xAxisIndex = 1, zlevel = 3,
                   tooltip = list(
                     textStyle = list(width = 20, overflow = "break"),
                     formatter = htmlwidgets::JS(paste0("
                           function (params) {return (
                            '<strong>Italia</strong>' +
                            '<br />Affluenza <strong> ' + params.data[1] + '%' + '</strong>' +
                            '<br />Votanti <strong> ' + params.data[2] + ' su ' + params.data[3] + '</strong>'
                         );}"
                     ))))

series[[3]] = list(name = "Distribuzione affluenza", type = "boxplot", data = affluenza_boxplot_data_list,
                   itemStyle = list(color = "#e3e6e6", borderColor = "#AB2525"),
                   tooltip = list(
                     textStyle = list(width = 20, overflow = "break"),
                     formatter = htmlwidgets::JS(paste0("
                           function (params) {return (
                            '<strong>Distribuzione affluenza</strong>' +
                            '<br />Max <strong> ' + params.data[5] + '%' + '</strong>' +
                            '<br />Q3 <strong> ' + params.data[4] + '%' + '</strong>' +
                            '<br />median <strong> ' + params.data[3] + '%' + '</strong>' +
                            '<br />Q1 <strong> ' + params.data[2] + '%' + '</strong>' +
                            '<br />Min <strong> ' + params.data[1] + '%' + '</strong>'
                         );}"
                     ))),
                   xAxisIndex = 0, zlevel = 1)

distinct_seggio = unique(affluenza_scatter_data$id_seggio)

for(s in 1:length(distinct_seggio)){

  affluenza_scatter_data_list =
    affluenza_scatter_data %>%
    filter(id_seggio == distinct_seggio[s]) %>%
    .build_data_jitter(votazione, affluenza, indirizzo, totale_votanti, iscritti, sezioni_elettorali, factor = 1, on = "x")

  series[[3+s]] = list(name = paste0("Seggio ", distinct_seggio[s]),
                       type = "line", data = affluenza_scatter_data_list,
                       symbolSize = 6,
                       lineStyle = list(width = 0, type = "dashed"),
                       itemStyle = list(opacity = 0.5),
                       emphasis = list(focus = 'series', lineStyle = list(width = 2)),
                       xAxisIndex = 1, zlevel = 2,
                       smooth = TRUE,
                       tooltip = list(
                         textStyle = list(width = 20, overflow = "break"),
                         formatter = htmlwidgets::JS(paste0("
                         function (params) {return (
                          'Seggio  <strong>", distinct_seggio[s], "</strong>' +
                          '<br />Indirizzo <strong> ' + params.data[2] + '</strong>' +
                          '<br />Affluenza <strong> ' + params.data[1] + '%' + '</strong>' +
                          '<br />Votanti <strong> ' + params.data[3] + ' su ' + params.data[4] + '</strong>'+
                          '<br />(Sezioni elettorali ' + params.data[5] + ')'
                       );}")))
  )
}

option = list(
  grid = list(top = 100, left = 50, right = 10, bottom = 30),
  title = list(list(text = "Andamento affluenza al voto",
                    subtext = "Dati puntuali per seggio.",
                    left = "center")),
  xAxis = list(
    list(type = "category", data = category_data_list,
         splitArea = list(show = FALSE),
         axisLabel = list(show = TRUE, interval  = '0'),
         splitLine = list(show = FALSE)),
    list(type = "value", show = FALSE,
         max = length(category_data_list))
  ),
  yAxis = list(name = "",
               axisLabel = list(formatter = "{value} %"),
               splitArea = list(show = FALSE),
               min = 30, max = 100
  ),
  series = series,
  legend = list(show = TRUE, data = list("Affluenza Bologna", "Affluenza Italia", "Distribuzione affluenza"),
                top = 70),
  tooltip = list(trigger = "item")
)

e_charts() %>% e_list(option)

