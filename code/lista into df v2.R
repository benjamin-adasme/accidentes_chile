pacman::p_load(tidyverse,
               janitor)

ls1 <- read_rds("data/lista_accidentes_10-19.rds")

df_1 <- do.call(rbind, ls1)

df_2 <- df_1 %>% 
  rownames_to_column(var = "año_id") %>% 
  separate(col = año_id, into = c("Año", "id_n"), sep = "\\.") %>% 
  mutate(across(17:21, ~na_if(., "NULL")))

colSums(is.na(df_2))  

