
# install.packages('pacman')
pacman::p_load(tidyverse, #Tidyverse, porque sí
               janitor, #Para procesar tabulados y limpieza de nombres
               openxlsx, #Para leer urls directamente
               readxl, #para probar descargando luego leyendo
               lubridate, #fechas
               readxlsb) #Para archivos xlsb


# Intento 1: con openxlsx. URL directa a función --------------------------

num <- 1:12

path <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022_"

data_list <- list()

#Este loop funciona para cargar las bases 2022 en una lista
for (i in num) {
  mes_num <- ifelse(i < 10, paste0("0",i), as.character(i))
  urls <- paste0(path, mes_num, ".xlsx")
  data_list[[i]] <- as_tibble(read.xlsx(urls, detectDates = T))
  #print(urls)
}

#Ver cómo sacar a DF separado
list2env(setNames(data_list, paste0("dataset",num)), envir = .GlobalEnv)

eliminar_mes <- function(x){
  out <- x %>% 
    select(-Mes)
}

dataset1 <- eliminar_mes(dataset1)
dataset2 <- eliminar_mes(dataset2)
dataset3 <- eliminar_mes(dataset3)
dataset4 <- eliminar_mes(dataset4)

doubles <- function(x){
  d1 <- c(15:19)
  out <- x %>%
    mutate(across(.cols = d1, .fns = as.double)) %>% 
    mutate(Parte.Nro = as.character(Parte.Nro))
}

dataset1 <- doubles(dataset1)

acc_22 <- bind_rows(
  dataset1,
  dataset2,
  dataset3,
  dataset4,
  dataset5,
  dataset6,
  dataset7,
  dataset8,
  dataset9,
  dataset10,
  dataset11,
  dataset12
)



# Intento 2: archivo temporal local ---------------------------------------
#Esta versión usa readxl, que lee mejor las fechas y horas
num <- 1:12

path <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022_"

data_list_2 <- list()

for (i in num) {
  mes_num <- ifelse(i < 10, paste0("0",i), as.character(i))
  urls <- paste0(path, mes_num, ".xlsx")
  p1 <- tempfile(fileext = ".xlsx")
  download.file(urls, p1, mode = "wb")
  data_list_2[[i]] <- readxl::read_xlsx(path = p1, na = "NULL")
}


clean_mes <- function(x){
  if(length(x) == 22){
    select(x, -Mes)
  } else{x}
}

l_2 <- lapply(data_list_2, FUN = clean_mes)

correccion_hora <- function(x) {
  out <- x %>% 
    mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
           Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)
}

l_3 <- lapply(l_2, correccion_hora)

correccion_clase <- function(x){
  d1 <- c(15:19)
  out <- x %>% 
    mutate(across(.cols = d1, .fns = as.double))
}

l_4 <- lapply(l_3, correccion_clase)

correcion_parte <- function(x){
  out <- x %>% 
    rename("Parte_Nro" = 21) %>% 
    mutate(Parte_Nro = as.character(Parte_Nro))
}

l_5 <- lapply(l_4, correcion_parte)


renombrar <- function(x){
  out <- x %>% 
    rename_with(~colnames(l_5[[3]]))
}

l_6 <- lapply(l_5, renombrar)

l_7 <- lapply(l_6, clean_names)

#Ver cómo sacar a DF separado
list2env(setNames(l_7, paste0("df2022_",num)), envir = .GlobalEnv)

# Para sacar objetos de una lista
# for(i in 1:length(data_list_2)) {
#   assign(paste0("db",i), data_list_2[[i]])
# }

#Ahora está correcto. 
acc_22 <- bind_rows(df2022_1,
                    df2022_2,
                    df2022_3,
                    df2022_4,
                    df2022_5,
                    df2022_6,
                    df2022_7,
                    df2022_8,
                    df2022_9,
                    df2022_10,
                    df2022_11,
                    df2022_12)

acc_mes <- acc_22 %>% 
  mutate(Mes = month(fecha, label = T), .after = fecha)

write_rds(acc_mes, "data/2022/accidentes_22.rds", compress = "gz")



# Bajada 2022 tipo de personas --------------------------------------------

num <- 1:12

path_2 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_2022_"

data_personas_22 <- list()

for (i in num) {
  mes_num <- ifelse(i < 10, paste0("0",i), as.character(i))
  urls <- paste0(path_2, mes_num, ".xlsx")
  temp <- tempfile(fileext = ".xlsx")
  download.file(urls, temp, mode = "wb")
  data_personas_22[[i]] <- readxl::read_xlsx(path = temp, na = "NULL")
}

nombres <- lapply(data_personas_22, colnames)
nom2 <- as_tibble(nombres)

print(nombres)
