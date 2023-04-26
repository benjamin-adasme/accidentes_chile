
# mapas por región --------------------------------------------------------


pacman::p_load(tidyverse,
               janitor,
               readr,
               lubridate,
               chilemapas,
               sf)

acc_21 <- read_rds("data/accidentes_21.rds")
pers_21 <- read_rds("data/personas_21.rds")
vehi_21 <- read_rds("data/vehiculos_21.rds")


regiones <- generar_regiones()

unique(acc_21$Region)

View(codigos_territoriales)

class(regiones$codigo_region)

nom_reg <-c("08", "09", "06", "05", "13", "10", "03", "07", "12", "04", "01", "16", "15", "02", "11", "14")

tab_regiones <- tibble(cod_reg = unique(acc_21$Region),
                       nom_reg = nom_reg)

regiones <- left_join(regiones, tab_regiones, by = c("codigo_region" = "nom_reg"))

acc_georegion <- acc_21 %>% 
  group_by(Region) %>% 
  count() %>% 
  left_join(regiones, by = c("Region" = "cod_reg"))

acc_georegion %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = n))

muertos_reg21 <- acc_21 %>% 
  rowwise() %>% 
  mutate(personas = sum(c(Muertos, Graves, M.Grave, Leves, Ilesos))) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  summarise(muertos_reg = sum(Muertos),
            perso_reg = sum(personas),
            ratio_muertos = muertos_reg/perso_reg)

muertos_reg21 %>% 
  left_join(regiones, by = c("Region" = "cod_reg")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = ratio_muertos)) +
  coord_sf(xlim = c(-65))

muertos_reg21 %>% 
  arrange(desc(ratio_muertos)) %>% 
  mutate(porc_muertos = scales::percent(ratio_muertos, big.mark = ".", decimal.mark = ","))

