
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
