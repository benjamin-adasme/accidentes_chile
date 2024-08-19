
# Baja datataset personas -------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               readxlsb)

##### 2010 a 2019 ####

url_ejemplo <-
  "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_2010.xlsb"

url_1 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_"

anios <- as.character(2010:2019)

pers_10_19 <- list()

for (i in anios){
  print(str_glue("Procesando base del año {i}"))
  url_anio <- str_glue("{url_1}{i}.xlsb")
  tf <-  tempfile(fileext = ".xlsb")
  download.file(url_anio, tf, mode = "wb")
  print(str_glue("Se descargó la base de personas del año {i}"))
  df <- read_xlsb(path = tf, sheet = 1)
  pers_10_19[[i]] <- df
  print(str_glue("Se agregó la base de personas del año {i} a la lista"))
}

names(pers_10_19)
length(pers_10_19)
View(pers_10_19)


df_1019 <- do.call(rbind, pers_10_19)

df_1019 <- rownames_to_column(df_1019)

df_b <- separate(df_1019, col = rowname, into = c("Año", "fila"), convert = TRUE)

dim(df_b)

write_rds(df_b, "data/personas_10_19.rds", compress = "gz")

#### 2020-2021 ####

ruta <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_2020_v2.xlsb"

anios_2 <- c("2020", "2021")

pers_20_21 <- list()

for (i in anios_2){
  print(str_glue("Procesando base del año {i}"))
  ruta <- str_glue("https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_{i}_v2.xlsb")
  tf <-  tempfile(fileext = ".xlsb")
  download.file(ruta, tf, mode = "wb")
  print(str_glue("Se descargó la base de personas del año {i}"))
  df <- read_xlsb(path = tf, sheet = 1)
  pers_20_21[[i]] <- df
  print(str_glue("Se agregó la base de personas del año {i} a la lista"))
}

length(pers_20_21)

pers_20_21[["2020"]] <- rename(pers_20_21[["2020"]],
                               "Codcomuna" = "Comuna",
                               "Comuna" = "Comunas")

df_2021 <- do.call(rbind, pers_20_21)

df_2021 <- rownames_to_column(df_2021)

df_c <- separate(df_2021, col = rowname, into = c("Año", "fila"), convert = TRUE)

write_rds(df_c, "data/personas_20_21.rds", compress = "gz")

#### Retomamos aquí ####

df_1019 <- read_rds("data/personas_10_19.rds")

df_2021 <- read_rds("data/personas_20_21.rds")

df_1019 <- df_1019 %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)

df_2021 <- df_2021 %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)


df_1 <- full_join(df_1019, df_2021)

df_2 <- df_1 %>% 
  rename("Nom_comuna" = Comuna) %>% 
  mutate(Región = case_match(Región, 
                             c("R.M.", "METROPOLITANA") ~ "METROPOLITANA",
                             c("I", "TARAPACÁ") ~ "TARAPACÁ",
                             c("II", "ANTOFAGASTA") ~  "ANTOFAGASTA",
                             c("III", "ATACAMA")~"ATACAMA",
                             c("IV", "COQUIMBO") ~ "COQUIMBO",
                             c("V", "VALPARAÍSO") ~ "VALPARAÍSO",
                             c("VI", "LIB. BDO. O´HIGGINS") ~ "LIB. BDO. O´HIGGINS",
                             c("VII", "MAULE") ~"MAULE",
                             c("VIII", "BIO-BIO") ~ "BIO-BIO",
                             c("IX", "ARAUCANÍA") ~ "ARAUCANÍA",
                             c("X", "LOS LAGOS") ~ "LOS LAGOS",
                             c("XI", "AYSÉN") ~ "AYSÉN",
                             c("XII", "MAGALLANES Y ART. CHILENA") ~ "MAGALLANES Y ART. CHILENA",
                             c("XIV", "LOS RÍOS") ~ "LOS RÍOS",
                             c("XV", "ARICA Y PARINACOTA") ~ "ARICA Y PARINACOTA",
                             c("XVI", "ÑUBLE") ~ "ÑUBLE"))


cut_com <- read_xls("code/CUT_2018_v04.xls")

tb_comuna <- cut_com %>% 
  select(6:7) %>% 
  rename("cod_com" = 1,
         "nom_com" = 2) %>% 
  mutate(nom_com = str_to_upper(nom_com))

tb_comuna <- tb_comuna %>% 
  mutate(nom_com = iconv(nom_com, to = "ASCII//TRANSLIT"))

df_3 <- df_2 %>% 
  mutate(Codcomuna = case_when(Codcomuna < 10000 ~ paste0("0", as.character(Codcomuna)),
                               Codcomuna >= 10000 ~ as.character(Codcomuna)))

df_4 <- df_3 %>% 
  mutate(Nom_comuna = case_match(Nom_comuna,
                                 "AISEN" ~ "AYSEN",
                                 "ALTO BIO BIO" ~ "ALTO BIOBIO",
                                 "CHOL CHOL" ~ "CHOLCHOL",
                                 "COIHAIQUE" ~ "COYHAIQUE",
                                 "O'Higgins" ~ "O'HIGGINS",
                                 "P. AGUIRRE CERDA" ~ "PEDRO AGUIRRE CERDA",
                                 "SAN PEDRO ATACAMA" ~ "SAN PEDRO DE ATACAMA",
                                 .default = Nom_comuna)) |> 
  left_join(tb_comuna, by = c("Nom_comuna" = "nom_com"))

colSums(is.na(df_4))

df_5 <- df_4 %>% 
  select(-Codcomuna) %>% 
  mutate(Edad = na_if(Edad, "NULL"),
         Edad = as.numeric(Edad))

unique(df_5$Resultado)

write_rds(df_5, "data/personas_cons_v1.rds", compress = "gz")
