pacman::p_load(tidyverse,
               readxl,
               readxlsb,
               readr,
               lubridate, 
               janitor)

df1 <- read_xlsb("os2_acc_2021_v2.xlsb", range = "Accdtes. 2021!A1:V80752", na = "NULL")

str(df1)

acc_2021 <- df1 %>% 
  as_tibble() 

dffff <- acc_2021 %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)
  
causas <- dffff %>% 
  group_by(Causas) %>% 
  count() %>% 
  arrange(desc(n))


acc_día <- dffff %>% 
  group_by(Fecha) %>% 
  count() 

acc_día %>% 
  ggplot(aes(Fecha, n)) +
  geom_line()

acc_semana <- dffff %>% 
  mutate(Semana = week(Fecha)) %>% 
  group_by(Semana) %>% 
  count()

acc_semana %>% 
  ggplot(aes(Semana, n)) +
  geom_line()

dffff %>% 
  group_by(Accdtes.) %>% 
  count(sort = T)

dffff %>% 
  tabyl(Urbano.Rural)

dffff %>% 
  group_by(Urbano.Rural) %>% 
  summarise(muertos_tot = sum(Muertos),
            accidentes = n()) %>% 
  mutate(muerte_acc = muertos_tot/accidentes *100)

