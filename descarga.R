
# Descarga de archivos ----------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               httr,
               openxlsx,
               readxlsb)
#Metodo 1

url1 <- "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2023_02.xlsx"

dl_file <- download.file(url1, destfile = tempfile(fileext = ".xlsx"))

#Método 2

r <- GET(url1, write_disk(path = tempfile(pattern = "file"), overwrite = T))

df <- read_xlsx(path = r)

#Método 3

df = read.xlsx(url1)

#Método 4

p1 <- tempfile(fileext = ".xlsx")
download.file(url1, p1, mode = "wb")
p2 <- readxl::read_xlsx(path = p1)


####### Probando con el archivo xlsb

url_b = "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_acc_2021_v2.xlsb"
b1 <- read_xlsb(url_b)


tmp <- tempfile(fileext = ".xlsb")
download.file(url_b, tmp, mode = "wb")
b2 = read_xlsb(tmp, range = "Accdtes. 2021!A1:V80752")

class(b2)

acc_21 <- b2 %>% 
  as_tibble() %>% 
  mutate(across(16:19, ~na_if(., "NULL"))) %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora,
         Ubicación.km = as.double(Ubicación.km))

readr::write_rds(acc_21, "data/accidentes_21.rds", compress = "gz")  

#Personas 2021
url_3 = "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_perso_2021_v2.xlsb"

download.file(url_3, tmp, mode = "wb")

df3 = read_xlsb(tmp, sheet = 1)

pers_21 <- df3 %>% 
  as_tibble() %>% 
  mutate(Edad = as.integer(Edad),
         Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)

readr::write_rds(pers_21, "data/personas_21.rds", compress = "gz")

#Vehículos 2021
url4 = "https://www.carabineros.cl/transparencia/tproactiva/OS2/os2_veh_2021_v2.xlsx"

tmp2 <- tempfile(fileext = ".xlsx")
download.file(url4, tmp2, mode = "wb")

df4 <- read_xlsx(tmp2)

vehic_21 <- df4 %>% 
  mutate(Hora = format(as.POSIXct(Hora), format = "%H:%M:%S"),
         Fecha_hora = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H:%M:%S"), .after = Hora)

write_rds(vehic_21, "data/vehiculos_21.rds", compress = "gz")

