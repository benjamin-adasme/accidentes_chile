
# Baja datataset personas -------------------------------------------------

pacman::p_load(tidyverse, 
               readxl,
               readxlsb)

##### 2010 a 2019 ####

url_ejemplo <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_2010.xlsb"

url_1 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_"

años <- 2010:2019

pers_10_19 <- list()

for (i in años){
  url_año <- str_glue("{url_1}{i}.xlsb")
  tf <-  tempfile(fileext = ".xlsb")
  download.file(url_año, tf, mode = "wb")
  print(str_glue("Se descargó la base de personas del año {i}"))
  df <- read_xlsb(path = tf, sheet = 1)
  pers_10_19 = append(pers_10_19, df)
  print(str_glue("Se agregó la base de personas del año {i} a la lista"))
}
