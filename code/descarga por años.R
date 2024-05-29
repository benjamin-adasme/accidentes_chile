
pacman::p_load(readxl,
               tidyverse,
               readxlsb)

# num <- 1:12
# 
# path <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022_"
# 
# data_list_2 <- list()
# 
# for (i in num) {
#   mes_num <- ifelse(i < 10, paste0("0",i), as.character(i))
#   urls <- paste0(path, mes_num, ".xlsx")
#   p1 <- tempfile(fileext = ".xlsx")
#   download.file(urls, p1, mode = "wb")
#   data_list_2[[i]] <- readxl::read_xlsx(path = p1, na = "NULL")
# }


# for(i in 1:length(data_list_2)) {
#   assign(paste0("db",i), data_list_2[[i]])
# }


# Reporte por tipo de accidente -------------------------------------------

#### 2010 - 2019 ####


lista_10_19 <- list()

path_2 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_"

años <- 2010:2019

for (i in años) {
  urls <- paste0(path_2,i,".xlsb")
  tf_2 <- tempfile(fileext = ".xlsb")
  download.file(urls, tf_2, mode = "wb")
  lista_10_19[[i]] <- readxlsb::read_xlsb(path = tf_2, sheet = 1)
}


lista_10_19[[2009]]

lista_acc_1 <- lista_10_19[2010:2019]

names(lista_acc_1) <- años

#Guardar lista para no volver a descargar

readr::write_rds(lista_acc_1, "data/lista_accidentes_10-19.rds", compress = "gz")

rm(lista_10_19)

#Recargar lista 

lista_acc_1 <- read_rds("data/lista_accidentes_10-19.rds")

#### 2020 - 2021 ####

años_2 <- c(2020, 2021)

path_3 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_"

lista_20_21 <- list()

for (i in años_2) {
  urls <- paste0(path_3,i,"_v2.xlsb")
  tf_3 <- tempfile(fileext = ".xlsb")
  download.file(urls, tf_3, mode = "wb")
  lista_20_21[[i]] <- readxlsb::read_xlsb(path = tf_3, sheet = 1)
}

lista_20_21[[2020]]

lista_acc_2 <- lista_20_21[2020:2021]

names(lista_acc_2) <- años_2

rm(lista_20_21)

write_rds(lista_acc_2, "data/lista_accidentes_20-21.rds", compress = "gz")

lista_acc_2 <- read_rds("data/lista_accidentes_20-21.rds")

#### 2022 ####

url_22 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022.xlsx"
tf_22 <- tempfile(fileext = ".xlsx")
download.file(url_22, tf_22, mode = "wb", method = "curl")

acc_22 <- read_xlsx(tf_22, sheet = 1)


##### Unión bases #####

df10_19 <- do.call(rbind, lista_acc_1) #Con las 2010 -2019 funciona pues son iguales

# df20_21 <- do.call(rbind, lista_acc_2) #No funciona con estas porque hay diferencias.

agr_col <- function(x){ #función para igualar la cantidad de variables
  if (length(x) == 20) {
    out <- x %>% 
      mutate(Parte.Nro. = NA,
             Tribunal = NA)
  } else{x}
}

ls_20 <- lapply(lista_acc_2, agr_col) #se crean dos variables nuevas en 2020


name_cols <- colnames(ls_20[["2021"]]) #colnames de 2021

rename_cols <- function(x){ #func para renombrar 2020 y 2021 igual
  old_name <- colnames(x)
  new <- x %>% 
    rename_at(vars(old_name), ~name_cols)
}

ls_20 <- lapply(ls_20, rename_cols) #se renombran los df

df20_21 <- do.call(rbind, ls_20)  #Se unen en un df.


str(df10_19)
str(df20_21)

df_10_2 <- df10_19 %>% #Solo falta agregar cod comuna
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)

df_20_2 <- df20_21 %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora) %>% 
  rename("Región" = Region)


# full_acc <- full_join(df10_19, df20_21)

full_acc_2 <- full_join(df_10_2, df_20_2) #Funciona. Corregir cod comuna, region

acc_22_b <- acc_22 %>% 
  rename_cols() %>% 
  rename("Región" = Region) %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora,
         Parte.Nro. = as.character(Parte.Nro.))

full_acc_3 <- full_join(full_acc_2, acc_22_b)

# Hasta aquí, full_acc_3 tiene todos los años, solo queda corregir variables como
# región, codigo de comuna. E inspeccionar si el resto de las variables está ok.
#Luego, queda descargar las bases de personas y vehículos.

unique(full_acc_3$Urbano.Rural)

## Región

unique(full_acc_3$Región)

full_acc_4 <- full_acc_3 %>% 
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

table(full_acc_4$Región)

table(lista_acc_1[["2010"]]$Región)

sort(unique(full_acc_4$Nom_comuna))

# Guardar última lista para continuar el trabajo donde quedé
# 

# write_rds(full_acc_4, "data/accidentes_limpieza_v1.rds", compress = "gz")

# acc_5 <- read_rds("data/accidentes_limpieza_v1.rds")
 acc_5 <- full_acc_4


acc_6 <- acc_5 %>% 
  mutate(Codcomuna = case_when(Codcomuna < 10000 ~ paste0("0", as.character(Codcomuna)),
                               Codcomuna >= 10000 ~ as.character(Codcomuna)))

# Cargar CUT
cut_com <- read_xls("code/CUT_2018_v04.xls")

tb_comuna <- cut_com %>% 
  select(6:7) %>% 
  rename("cod_com" = 1,
         "nom_com" = 2) %>% 
  mutate(nom_com = str_to_upper(nom_com))



tb_comuna <- tb_comuna %>% 
  mutate(nom_com = iconv(nom_com, to = "ASCII//TRANSLIT"))

# acc_6$Codcomuna_2 <- tb_comuna$cod_com[match(acc_6$Codcomuna, tb_comuna$nom_com)]

acc_7 <- acc_6 |> 
  mutate(Nom_comuna = case_match(Nom_comuna,
                                 "AISEN" ~ "AYSEN",
                                 "ALTO BIO BIO" ~ "ALTO BIOBIO",
                                 "CHOL CHOL" ~ "CHOLCHOL",
                                 "COIHAIQUE" ~ "COYHAIQUE",
                                 "O'Higgins" ~ "O'HIGGINS",
                                 "P. AGUIRRE CERDA" ~ "PEDRO AGUIRRE CERDA",
                                 "SAN PEDRO ATACAMA" ~ "SAN PEDRO DE ATACAMA",
                                 .default = Nom_comuna)) |> 
  left_join(tb_comuna, by = c("Nom_comuna" = "nom_com")) #%>%
#mutate(Codcomuna_3 = if_else(is.na(Codcomuna), cod_com, Codcomuna))

acc_7 |> head()
colSums(is.na(acc_7))
sort(unique(acc_7$cod_com))

acc_7 |> 
  filter(is.na(cod_com)) |> 
  group_by(Nom_comuna) |> 
  summarise(perdidos = n())

write_rds(acc_7, "data/acc_cons_v1.rds", compress = "bz2")






# Gráfico para readme
# 

# full_acc_3 %>% 
#   mutate(año = year(Fecha)) %>% 
#   group_by(año) %>% 
#   summarise(n = n()) %>% 
#   ggplot(aes(año, n)) +
#   geom_line()
# 
# full_acc_3 %>% 
#   group_by(Fecha) %>%
#   summarise(n_dia = n()) %>% 
#   ggplot(aes(Fecha, n_dia)) +
#   geom_line() +
#   geom_smooth(se = F)
# 
# p1 <- full_acc_3 %>% 
#   group_by(Fecha) %>%
#   summarise(n_dia = n()) %>% 
#   mutate(m_5 = zoo::rollmean(n_dia, 5, fill = NA, align = "right")) %>% 
#   ggplot(aes(Fecha, m_5)) +
#   geom_line(color = "darkblue") +
#   labs(x = "Años", y = "Accidentes diarios (media móvil 5 días)",
#        title = "Accidentes de tránsito diarios en Chile (2010-2022)",
#        subtitle = "Media móvil de accidentes de tránsito en Chile registrados por Carabineros",
#        caption = "Datos de Carabineros de Chile. Elaborado por Benjamín Adasme Jara") + 
#   coord_cartesian(expand = F, clip = "off", ylim = c(90, 325)) +
#   theme_minimal()
# 
# 
# ggsave(filename = "plots/plot1_readme.jpg", plot = p1, width = 10, height = 7, dpi = 300)  
