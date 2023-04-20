
# Análisis exploratorio ---------------------------------------------------

pacman::p_load(tidyverse,
               janitor,
               readr,
               lubridate)

acc_21 <- read_rds("data/accidentes_21.rds")
pers_21 <- read_rds("data/personas_21.rds")
vehi_21 <- read_rds("data/vehiculos_21.rds")

vehi_21 %>% 
  tabyl(Tipo) %>% 
  arrange(desc(n))

pers_21 %>% 
  mutate(Resultado = fct(Resultado, levels = c("ILESO",
                                               "LEVE",
                                               "MENOS GRAVE",
                                               "GRAVE",
                                               "MUERTO"))) %>% 
  tabyl(Calidad, Resultado) %>% 
  adorn_percentages(denominator = "row") 

vehi_21 %>% 
  tabyl(Servicio)

afectados <- acc_21 %>% 
  rowwise() %>% 
  mutate(tot_invol = sum(Muertos, Graves, M.Grave, Leves, Ilesos), .after = Ilesos)

afectados %>% 
  group_by(Fecha) %>% 
  summarise(muertos_dia = sum(Muertos),
            porc_muertos = muertos_dia/sum(tot_invol)) %>% 
  ggplot(aes(Fecha, porc_muertos)) +
  geom_line()

afectados %>% 
  mutate(semana = week(Fecha)) %>% 
  group_by(semana) %>% 
  summarise(muertos_semana = sum(Muertos),
            porc_muertos = muertos_semana/sum(tot_invol)) %>% 
  ggplot(aes(semana, porc_muertos)) +
  geom_line()

med_movil <- afectados %>%
  group_by(Fecha) %>% 
  summarise(Muertos_d = sum(Muertos),
            afect_d = sum(tot_invol)) %>% 
  mutate(m_muertos7 = zoo::rollmean(Muertos_d, 7, fill = NA),
         m_afect7 = zoo::rollmean(afect_d, 7, fill = NA),
         porc_muertos = m_muertos7/m_afect7)

med_movil %>%
  ggplot(aes(Fecha, m_muertos7)) +
  geom_line()

med_movil %>% 
  ggplot(aes(Fecha, porc_muertos)) +
  geom_line()


vehic_dia <- vehi_21 %>% 
  group_by(Fecha, Tipo) %>% 
  summarise(conteo = n())

vehic_dia %>% 
  ggplot(aes(Fecha, conteo, color = Tipo)) + 
  geom_line()

vehic_dia %>% 
  filter(Tipo == "BICICLETA") %>% 
  # mutate(media_bic = zoo::rollmean(conteo, 7, fill = NA)) %>% 
  ggplot(aes(Fecha, conteo, color = Tipo)) + 
  geom_line()

bicis <- vehi_21 %>% 
  filter(Tipo == "BICICLETA") %>% 
  group_by(Fecha) %>% 
  summarise(bici_dia = n()) %>% 
  mutate(media_bici_3 = zoo::rollmean(bici_dia, 3, fill = NA),
         media_bici_7 = zoo::rollmean(bici_dia, 7, fill = NA),
         media_bici_9 = zoo::rollmean(bici_dia, 9, fill = NA))

bicis %>% 
  pivot_longer(cols = 3:5, names_to = "tipo_dias", values_to = "mean") %>% 
  ggplot(aes(Fecha, mean, color = tipo_dias)) + 
  geom_line() +
  facet_wrap(tipo_dias~.)
