



# hogst volum verdi data fra landbruksdirektoratets excel-filer ----
ld_avvirk_fylke <- function() {
  # fylkesvise hogstdata fra landbruksdirektoratets excel-filer
  # fylkesvis pr mnd
  files <-   list.files(path = "./data-raw",
                        pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Fylkesvis avvirkning pr m"))]


  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T)[NULL, ]
  for (i in seq_along(files)) {
    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T))
  }
  return(df)
}
hogst_fylke_ld <- ld_avvirk_fylke() %>%
  mutate(region = FYLKENR, aar = AVVIRKAAR) %>%
  mutate( TOTALVOLUM = as.numeric(TOTALVOLUM),
          TOTALVERDI = as.numeric(TOTALVERDI),
          M3PRIS = as.numeric(M3PRIS))
usethis::use_data(hogst_fylke_ld, overwrite = T, version = 3)


sortimentpriser_fylke_ldep <-   regnavn.at.ref.yr(
  regionstat = (ld_avvirk_fylke() %>%
                  mutate(region_kode = FYLKENR, aar = AVVIRKAAR,
                         TOTALVOLUM = as.numeric(TOTALVOLUM),
                         TOTALVERDI = as.numeric(TOTALVERDI),
                         M3PRIS = as.numeric(M3PRIS)))) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::group_by(reg_n2021, reg_k2021, aar, sortkode, sortiment, virkesgrp, region_kode) %>%
  dplyr::summarise(totalvolum = sum(totalvolum),
                   totalverdi = sum(totalverdi),
                   m3pris = totalverdi / totalvolum)
usethis::use_data( sortimentpriser_fylke_ldep, overwrite = T, version = 3)


ld_avvirk_kommune <- function() {
  # kommunevise avvirkningsstatistikk fra landbruksdirektoratets excel-filer
  # kommunevis pr år
  files <- list.files( path = "./data-raw",
                       pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Kommunevis avvirkning"))]

  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T)[NULL, ]
  for (i in seq_along(files)) {
    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T))
  }
  return(df)
}
hogst_kommune_ld <- ld_avvirk_kommune()
usethis::use_data(hogst_kommune_ld, overwrite = T, version = 3)

sortimentpriser_kmn_ldep <-   regnavn.at.ref.yr(
  regionstat = (ld_avvirk_kommune() %>%
                  mutate(region_kode = KOMNR, aar = AVVIRKAAR))) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::group_by(reg_n2021, reg_k2021, aar, sortkode, sortiment, virkesgrp, region_kode) %>%
  dplyr::summarise(totalvolum = sum(totalvolum),
                   totalverdi = sum(totalverdi),
                   m3pris = totalverdi / totalvolum)
usethis::use_data(sortimentpriser_kmn_ldep, overwrite = T, version = 3)

