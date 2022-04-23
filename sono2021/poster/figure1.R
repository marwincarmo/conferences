dados_c <- dados |> 
  dplyr::mutate(idade = idade - 18) |> 
  dplyr::mutate(dplyr::across(c(igi_escore:spaq_score), ~scale(.x) %>% as.vector)) %>% 
  dplyr::mutate(sexo = ifelse(sexo == "F", .5,-.5),
                sexo = factor(sexo))

mod1 <- lm(igi_escore ~ idade + sexo +ehad_ansiedade_escore + ehad_depressao_escore + dbas_score*aaq_score, data = dados_c)

aaq_high <- dados_c %>% 
  dplyr::mutate(aaq_score = aaq_score + 1)

aaq_low <- dados_c %>% 
  dplyr::mutate(aaq_score = aaq_score - 1)

pred_high <- predict(mod1, aaq_high)
pred_low <- predict(mod1, aaq_low)
pred_avg <-  predict(mod1, dados_c)

dados_c %>% 
  select(igi_escore, dbas_score) %>% 
  mutate(alto = pred_high,
         baixo = pred_low,
         avg = pred_avg) %>% 
  tidyr::pivot_longer(cols = c("alto", "baixo", "avg"), names_to = "grupo", values_to = "igi") %>% 
  ggplot() +
  geom_point(aes(x = dbas_score, y = igi_escore), alpha = .08, position = "jitter") +
  stat_smooth(aes(x = dbas_score, y = igi, color = factor(grupo)), 
              method="lm",se=FALSE, fullrange = TRUE,
              size = 1.3) +
  my_theme +
  scale_color_viridis_d(option = "A", label = c("High (+1SD)", "Average","Low (-1SD)"), 
                        direction = -1, begin = .9, end = .4, name = "AAQ-II") +
  labs(x = "Standardized DBAS-16 scores",
       y = "Standardized ISI scores") +
  theme(legend.position = "bottom",
        axis.title=element_text(size=16))
