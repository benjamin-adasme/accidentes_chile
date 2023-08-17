
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
download.file(url_22, tf_22, mode = "wb")

acc_22 <- read_xlsx(tf_22, sheet = 1)


##### Unión bases #####

df10_19 <- do.call(rbind, lista_acc_1) #Con las 2010 -2019 funciona pues son iguales.
# df20_21 <- do.call(rbind, lista_acc_2) #No funciona con estas porque hay diferencias.


list2env(setNames(lista_acc_2, paste0("df_",años_2)), envir = .GlobalEnv)

df_2020 <- df_2020 %>% 
  rename("Codcomuna" = Comuna,
         "Comuna" = Comuna2)

df20_21 <- full_join(df_2020, df_2021) # tira error por las columnas posixct

