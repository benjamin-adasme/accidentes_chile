
# mapas por región --------------------------------------------------------


pacman::p_load(tidyverse,
               janitor,
               readr,
               lubridate,
               chilemapas)

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

