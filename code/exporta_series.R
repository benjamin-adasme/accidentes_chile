pacman::p_load(tidyverse,
               zoo)


serie_1 <- read_csv("data/serie_acc.csv")

serie_1 %>% 
  ggplot(aes(Fecha, num_acc)) +
  geom_line()

df_2 <- serie_1 %>% 
  mutate(rm_3 = rollmean(num_acc, k = 3, align = "center", fill = NA),
         rm_5 = rollmean(num_acc, k = 5, align = "center", fill = NA),
         rm_7 = rollmean(num_acc, k = 7, align = "center", fill = NA)) %>% 
  pivot_longer(cols = 2:5, names_to = "Serie", values_to = "valor")


df_2 %>% 
  ggplot(aes(Fecha, valor, colour = Serie)) +
  geom_line() +
  facet_wrap(.~Serie)


df_3 <- serie_1 %>% 
  mutate(Año = year(Fecha),
         Mes = month(Fecha)) %>% 
  group_by(Año, Mes) %>% 
  summarise(acc_mes = sum(num_acc, na.rm = T))
