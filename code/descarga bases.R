
pacman::p_load(tidyverse, #Tidyverse, porque sí
               janitor, #Para procesar tabulados y limpieza de nombres
               openxlsx, #Para leer urls directamente
               readxlsb) #Para archivos xlsb

num <- 1:12

path <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022_"

data_list <- list()

#Este loop funciona para cargar las bases 2022 en una lista
for (i in num) {
  mes_num <- ifelse(i < 10, paste0("0",i), as.character(i))
  urls <- paste0(path, mes_num, ".xlsx")
  data_list[[i]] <- read.xlsx(urls)
  #print(urls)
}

#Ver cómo sacar a DF separado
list2env(setNames(data_list, paste0("dataset",mes_num)), envir = .GlobalEnv)

