library(dplyr)

raw_data <- readr::read_csv("vii-clinica-psiquiatrica/dbas_network/data_dbas.csv")

descriptive_data <- raw_data %>% 
  dplyr::select(sexo, etnia, escolaridade, medicacao_semana, regiao) %>% 
  tidyr::pivot_longer(cols = c(sexo, etnia,sexo, etnia, escolaridade, medicacao_semana, regiao),
                      names_to = 'variable', values_to = 'value',
                      values_transform = list(value=as.character)) %>% 
  dplyr::with_groups(c(variable, value), count) %>% 
  dplyr::with_groups(c(variable), mutate, p = n/sum(n)) %>% 
  dplyr::mutate(vars = dplyr::case_when(
    variable == "escolaridade" & value %in% c(1:7) ~ "Less than 12th grade",
    variable == "escolaridade" & value == 8 ~ "Some college",
    variable == "escolaridade" & value == 9 ~ "College degree or higher",
    variable == "etnia" & value == 1 ~ "White",
    variable == "etnia" & value %in% c(2,3) ~ "Black",
    variable == "etnia" & value == 4 ~ "Asian",
    variable == "etnia" & !(value %in% c(1:4)) ~ "Other/Not informed",
    variable == "medicacao_semana" & value == 0 ~ "Don't use",
    variable == "medicacao_semana" & value %in% c(1:5) ~ "1-5 days a week",
    variable == "medicacao_semana" & value %in% c(6,7) ~ "6-7 days a week",
    variable == "regiao" & value == "Região Centro-Oeste" ~ "Central-West",
    variable == "regiao" & value == "Região Nordeste" ~ "Northeast",
    variable == "regiao" & value == "Região Norte" ~ "North",
    variable == "regiao" & value == "Região Sudeste" ~ "Southeast",
    variable == "regiao" & value == "Região Sul" ~ "South",
    variable == "sexo" & value == "F" ~ "Female",
    variable == "sexo" & value == "M" ~ "Male"
  )) %>% 
  dplyr::with_groups(c(variable, vars), summarise, n = sum(n), p = sum(p)) %>% 
  #dplyr::mutate(total = paste0(n, " (", signif(p*100,3), ")")) %>%
  dplyr::mutate(variable = factor(variable, 
                                  levels = c("sexo", "etnia", "escolaridade", "regiao", "medicacao_semana"))) %>% 
  dplyr::arrange(variable) #%>%
  #dplyr::select(-c(variable, n, p))


continuous_data <- raw_data %>% 
  dplyr::select(idade, renda) %>% 
  dplyr::summarise(mean_age = mean(idade, na.rm = TRUE),
                   sd_age = sd(idade, na.rm = TRUE),
                   income = median(renda, na.rm = TRUE)) %>% 
  #dplyr::mutate("Age (years)" = paste0(signif(mean_age,2), " (", signif(sd_age,3), ")")) %>% 
  #dplyr::select(-c(mean_age, sd_age)) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = 'vars', 
                      values_to = 'total', values_transform = list(total=as.character))

table1 <- dplyr::bind_rows(continuous_data, descriptive_data) %>% dplyr::slice(2:n())
