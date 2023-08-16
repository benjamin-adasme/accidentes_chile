pacman::p_load(tidyverse,
               janitor)

ls1 <- read_rds("data/lista_accidentes_10-19.rds")

df_1 <- do.call(rbind, ls1)

df_2 <- df_1 %>% 
  rownames_to_column(var = "año_id") %>% 
  separate(col = año_id, into = c("Año", "id_n"), sep = "\\.") %>% 
  mutate(across(17:21, ~na_if(., "NULL")))

colSums(is.na(df_2))  

df_2 %>% tabyl(Región)

str(df_2)

df_2 %>% 
  filter(Región == "VII") %>% 
  ggplot(aes(Año)) +
  geom_bar()

df_2 %>% mutate(muertos_2 = if_else(Muertos == 0, 0, 1)) %>% 
  tabyl(Urbano.Rural, muertos_2) %>% adorn_percentages(denominator = "row")

df_2 %>% 
  mutate(dir = case_when(Urbano.Rural == "RURAL" ~ paste0(Ruta, ", km ", Ubicación.km),
                         Urbano.Rural == "URBANO" & !is.na(Frentenumero) ~ paste0(Calleuno, " ", Frentenumero),
                         Urbano.Rural == "URBANO" & is.na(Frentenumero) ~ paste0(Calleuno, " con ", Calledos))) %>% 
  View()
