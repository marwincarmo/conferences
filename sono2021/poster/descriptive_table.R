# age
# sex
# race
# education (Fundamental incompleto, Superior incompleto, superior completo)
# regiao (5 regioes)
# medicacao (0, 1-5, 6-7)



descriptive_data <- dados %>% 
  dplyr::select(sexo, etnia, escolaridade, medicacao_semana, regiao) %>% 
  tidyr::pivot_longer(cols = c(sexo, etnia,sexo, etnia, escolaridade, medicacao_semana, regiao),
                      names_to = 'variable', values_to = 'value',
                      values_transform = list(value=as.character)) %>% 
  dplyr::with_groups(c(variable, value), count) %>% 
  dplyr::with_groups(c(variable), mutate, p = n/sum(n)) %>% 
  dplyr::mutate(vars = dplyr::case_when(
    variable == "escolaridade" & value %in% c(1:7) ~ "$\\le$ 12th grade",
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
  dplyr::mutate(total = paste0(n, " (", signif(p*100,3), ")")) %>%
  dplyr::mutate(variable = factor(variable, 
                                  levels = c("sexo", "etnia", "escolaridade", "regiao", "medicacao_semana"))) %>% 
  dplyr::arrange(variable) %>% 
  dplyr::select(-c(variable, n, p))

  
continuous_data <- dados %>% 
  dplyr::select(idade, renda) %>% 
  dplyr::summarise(mean_age = mean(idade),
            sd_age = sd(idade),
            income = median(renda, na.rm = TRUE)) %>% 
  dplyr::mutate("Age (years)" = paste0(signif(mean_age,2), " (", signif(sd_age,3), ")")) %>% 
  dplyr::select(-c(mean_age, sd_age)) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = 'vars', 
                      values_to = 'total', values_transform = list(total=as.character))

table1 <- dplyr::bind_rows(continuous_data, descriptive_data) %>% dplyr::slice(2:n())
#readr::write_csv(table1, "conferences/cbsono_21/poster/table1data.csv")

#The effect of Anxiety was a significant positive predictor of ISI scores ($\beta$ = `r round(s$coefficients[4,1],2)`, 95% CI [0.06, 0.25], t(634) = 3.30, p = 0.001), as was DBAS-16 (β = 0.40, 95% CI [0.32, 0.47], t(634) = 10.51, p < 0.001) and AAQ-II (β = 0.12, 95% CI [0.02, 0.22], t(634) = 2.39, p = 0.017). There was a significant and positive interaction effect between DBAS-16 and AAQ-II (β = 0.001, 95% CI [0.0001, 0.002], t(634) = 2.20, p = 0.028), such that DBAS-16 slope predicting ISI scores became significantly more positive with greater scores on AAQ-II. 


# Modelsummary output ---------------------------------------------------------


descriptive <- dados %>% 
  dplyr::select(idade, sexo, etnia, escolaridade, medicacao_semana, regiao) %>% 
  dplyr::mutate(
    sexo = dplyr::case_when(
      sexo == "F" ~ "Female",
      sexo == "M" ~ "Male"
      ),
    etnia = dplyr::case_when(
      etnia ==  1 ~ "White",
      etnia %in% c(2,3) ~ "Black",
      etnia == 4 ~ "Asian",
      !(etnia %in% c(1:4)) ~ "Other/Not informed",
    ),
    escolaridade = dplyr::case_when(
      escolaridade %in% c(1:7) ~ "$\\le$ 12th grade",
      escolaridade == 8 ~ "Some college",
      escolaridade == 9 ~ "College degree or higher",
    ),
    medicacao_semana = dplyr::case_when(
      medicacao_semana == 0 ~ "Don't use",
      medicacao_semana %in% c(1:5) ~ "1-5 days a week",
      medicacao_semana %in% c(6,7) ~ "6-7 days a week",
    ),
    regiao = dplyr::case_when(
      regiao == "Região Centro-Oeste" ~ "Central-West",
      regiao == "Região Nordeste" ~ "Northeast",
      regiao == "Região Norte" ~ "North",
      regiao == "Região Sudeste" ~ "Southeast",
      regiao == "Região Sul" ~ "South",
    )
  ) %>% 
  mutate(group = "A")

datasummary_balance(~group, data = descriptive, dinm=FALSE)
datasummary(hp + mpg ~ Factor(cyl) * mean, data = mtcars)
