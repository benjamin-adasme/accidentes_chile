
pacman::p_load(tidyverse, #Tidyverse, porque sí
               janitor, #Para procesar tabulados y limpieza de nombres
               openxlsx, #Para leer urls directamente
               readxl,
               lubridate,
               readxlsb) #Para archivos xlsb

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
