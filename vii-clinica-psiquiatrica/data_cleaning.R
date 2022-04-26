# Load and clean data ----

raw_data <- readr::read_csv("G:/Documentos/ProjetosR/ASONO/act_insonia/dados/base_completa_031422.csv")


### Retrieve geolocation info ----

wiki_url <- "https://pt.wikipedia.org/wiki/Discagem_direta_a_dist%C3%A2ncia"
r_wiki <- httr::GET(wiki_url)

### From phone code

base_ddd <- r_wiki |> 
  xml2::read_html() |> 
  #xml2::xml_find_first('//table')
  xml2::xml_find_all("//table[@class='wikitable sortable']") |> 
  rvest::html_table() |> 
  magrittr::extract2(1) |> 
  janitor::clean_names()

### Retrieve Brazil's states and regions info

regioes_url <- "https://www.todamateria.com.br/siglas-estados-brasileiros/"
r_regioes <- httr::GET(regioes_url)

base_regioes <- r_regioes |> 
  xml2::read_html() |> 
  xml2::xml_find_all("//table") |> 
  rvest::html_table() |> 
  magrittr::extract2(1) |> 
  janitor::clean_names() |> 
  dplyr::mutate(observacao = stringr::str_remove(observacao, "Localizado na "))


dados_recrutamento <- raw_data |> 
  dplyr::filter(redcap_event_name == "recrutamento_arm_1",
                !is.na(telefone_1)) |> 
  dplyr::select(record_id, nome, latitude, longitude, telefone_1) |> 
  dplyr::mutate(ddd = as.double(stringr::str_extract(telefone_1, "[1-9]{2}"))) |> 
  dplyr::left_join(base_ddd, by = c("ddd" = "prefixo")) |> 
  dplyr::select(-cidades_principais_regioes)

endereco <- readr::read_rds("G:/Documentos/ProjetosR/ASONO/act_insonia/dados/enderecos.rds") |> 
  dplyr::select(record_id, state, region)

dados_localizacao <- dados_recrutamento |> 
  dplyr::left_join(endereco, by = "record_id") |> 
  dplyr::mutate(state = dplyr::coalesce(state, estado)) |> 
  dplyr::left_join(base_regioes, by = c("state" = "estado_do_brasil")) |> 
  dplyr::select(-c(estado, region)) |> 
  dplyr::rename(c(estado = state, regiao = observacao)) |> 
  tidygeocoder::geocode(state = estado ,method = 'osm') |> 
  dplyr::mutate(latitude = dplyr::coalesce(latitude, lat),
                longitude = dplyr::coalesce(longitude, long)) |> 
  dplyr::select(-c(lat, long))

### Eligibility data
library(dplyr)
renda_corrigida <- readr::read_csv("G:/Documentos/ProjetosR/ASONO/act_insonia/dados/renda_corrigida_2610.csv")

database_dbas <- raw_data |> 
  dplyr::group_by(record_id) %>% 
  tidyr::fill(c(idade,dsm_1:dsm_5), .direction = "down") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(sexo = ifelse(sexo == 1, "F", "M")) %>% 
  dplyr::filter(idade <= 59 | is.na(idade)) %>%
  dplyr::filter(dbas_16_versao_brasileira_complete == 2) %>% 
  dplyr::select(record_id, redcap_event_name, sexo, idade, etnia, escolaridade, 
                renda, medicacao_semana, igi_escore, aaq_score, dbas_score, contains("dbas_"),
                dsm_1, dsm_2, dsm_3, dsm_4, dsm_5) %>%
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") %>% 
  dplyr::left_join(dados_localizacao, by = "record_id") %>% 
  dplyr::filter(!stringr::str_detect(nome, stringr::regex("teste", ignore_case = TRUE))) |> 
  dplyr::left_join(renda_corrigida, by = "record_id") |> 
  dplyr::mutate(renda.x = case_when(
    !is.na(renda.y) ~ renda.y,
    # renda.x >= 1000000 ~ NA_real_,
    # renda.x == 0 ~ NA_real_,
    TRUE ~ renda.x
  )
  ) |> 
  dplyr::rename(renda = renda.x) |> 
  dplyr::select(-renda.y) |> 
  dplyr::mutate(regiao = dplyr::case_when(
    estado == "Distrito Federal/Goiás" ~ "Região Centro-Oeste",
    TRUE ~ regiao
  ),
  group = dplyr::case_when(dsm_1 == 0 & dsm_2 == 0 & dsm_3 == 0 
                           & dsm_4 == 0 & dsm_5 == 0 & igi_escore < 8 ~ "Good Sleeper",
                           TRUE ~ "Insomnia"))

readr::write_csv(database_dbas, "vii-clinica-psiquiatrica/dbas_network/data_dbas.csv")
