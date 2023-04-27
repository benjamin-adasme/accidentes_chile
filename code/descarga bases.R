
pacman::p_load(tidyverse, #Tidyverse, porque sí
               janitor, #Para procesar tabulados y limpieza de nombres
               openxlsx, #Para leer urls directamente
               readxlsb) #Para archivos xlsb

num <- 1:12

month_num <- if_else(num < 10, paste0("0",num), as.character(num))

path <- "data/2022"

data_list <- list()

#Tal como está el códgo, no funciona. Logré componer las 12 urls. Pero ni cerca de bajarlos
for (number in month_num) {
  filename <- paste0("https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2022_", month_num, ".xlsx")
  file_path <- file.path(path, filename)
  data_list[[number]] <- read.xlsx(file_path)
}



