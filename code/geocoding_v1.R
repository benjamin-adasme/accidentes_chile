pacman::p_load(tidyverse,
               mall,
               tidygeocoder)


acc <- read_rds("data/acc_cons_v1.rds")

limpiando_dir <- acc |> 
  mutate(across(.cols = 16:20, .fns = ~na_if(.x, "NULL"))) |> 
  mutate(Calleuno = na_if(Calleuno, "NULL"),
         Calledos = na_if(Calledos, "NULL"),
         dir_1 = case_when(str_detect(Calleuno, "=> AVDA") ~ str_glue("Avenida {Calleuno}"),
                           str_detect(Calleuno, "=> PSJE") ~ str_glue("Pasaje {Calleuno}"),
                           T ~ Calleuno),
         dir_1 = str_remove(dir_1, " => AVDA"),
         dir_1 = str_remove(dir_1, " => PSJE"),
         dir_1 = str_remove(dir_1, " => CALLE"),
         dir_2 = case_when(str_detect(Calledos, "=> AVDA") ~ str_glue("Avenida {Calledos}"),
                           str_detect(Calledos, "=> PSJE") ~ str_glue("Pasaje {Calledos}"),
                           T ~ Calledos),
         dir_2 = str_remove(dir_2, " => AVDA"),
         dir_2 = str_remove(dir_2, " => PSJE"),
         dir_2 = str_remove(dir_2, " => CALLE"),
         calle_num = if_else(!is.na(Frentenumero) & !is.na(Calleuno), 
                             str_glue("{dir_1} {Frentenumero}, {Nom_comuna}, Chile"),
                             NA),
         interseccion = if_else(!is.na(dir_1) & !is.na(dir_2),
                                str_glue("{dir_1} & {dir_2}, {Nom_comuna}, Chile"), 
                                NA))


# is.na(Frentenumero) & Urbano.Rural == "Urbano" ~

DT::datatable(head(limpiando_dir, 100))


calle_numero  <- limpiando_dir |> 
  filter(!is.na(calle_num))

calle_numero |> dim()

df_prueba_geo  <- calle_numero |> head(100)

df_prueba_geo <- df_prueba_geo |> 
  geocode(address = calle_num)

head(df_prueba_geo[,25:30],10)

writexl::write_xlsx(df_prueba_geo, "data/prueba_geocode_1.xlsx")

df_geo_talca <- calle_numero |> 
  filter(cod_com == "07101") |> 
  geocode(address = calle_num)

writexl::write_xlsx(df_geo_talca, "data/prueba_geocode_2.xlsx")
