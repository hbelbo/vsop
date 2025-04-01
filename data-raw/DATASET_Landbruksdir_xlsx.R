
fFylkeskoder_2024 <- function() {

  #Lager en hjelpetabell som viser regionkode og navn på alle fylker per 2024

  files <-   list.files(path = "./data-raw",
                        pattern = ".txt", full.names = T)
  files <- files[which(stringr::str_detect(files, "Regind_Flk_2024_01_status.txt"))]
  filename <- files[1]
  datastring <- readLines(filename, n=-1L,
                          encoding =  dplyr::pull(readr::guess_encoding(filename)[1,1]),
                          warn = F)

  splits <- stringr::str_split_fixed(datastring, " - ", 2)

  # Convert the list to a data frame
  splits <- as.data.frame(splits)
  colnames(splits) <- c("regionkode", "Fylke")

  name_split <- stringr::str_split_fixed(splits[, "Fylke"], " - ", 2)
  splits[, "Fylke"] <- name_split[, 1]

  return(splits)

}
Fylkeskoder <- fFylkeskoder_2024()

# hogst volum verdi data fra landbruksdirektoratets excel-filer ----
ld_avvirk_fylke <- function() {
  # fylkesvise hogstdata fra landbruksdirektoratets excel-filer
  # fylkesvis pr mnd
  files <-   list.files(path = "./data-raw",
                        pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Fylkesvis_avvirkning_pr_m"))]


  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T)[NULL, ]
  for (i in seq_along(files)) {
    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T))
  }
  return(df)
}




  avvirk_fylke_ldir <- regnavn.at.ref.yr(
    (ld_avvirk_fylke() %>%
       mutate(region_kode = FYLKENR, aar = AVVIRKAAR,
              TOTALVOLUM = as.numeric(stringr::str_replace_all(TOTALVOLUM, " ", "")),
              TOTALVERDI = as.numeric(stringr::str_replace_all(TOTALVERDI, " ", "")),
              M3PRIS = as.numeric(stringr::str_replace_all(M3PRIS, " ", "")))
     )) %>%
    dplyr::rename_with(tolower)  %>%
    dplyr::group_by_at(vars(starts_with("reg_"), aar, sortkode, sortiment, virkesgrp, virkeskat, kategoritekst))  %>%
    dplyr::summarise(totalvolum = sum(totalvolum, na.rm=TRUE),
                   totalverdi = sum(totalverdi, na.rm = TRUE),
                   m3pris = dplyr::if_else(!is.na(totalvolum) & totalvolum > 0, totalverdi / totalvolum, NaN),
                   region_kode = paste0(unique(region_kode), collapse = ", ")) %>%
    dplyr::ungroup()
usethis::use_data( avvirk_fylke_ldir, overwrite = T, version = 3)

# # checking that nothing went lost
# avvirk_fylke_ldir %>% group_by(aar) %>% summarise(n = n(), regioner = length(unique(reg_k2022)), totalvolum = sum(totalvolum))
# avvirk_fylke_test <- ld_avvirk_fylke()
# avvirk_fylke_test %>% group_by(AVVIRKAAR) %>% summarise(TOTALVOLUM = sum(as.numeric(TOTALVOLUM), na.rm=TRUE))
# avvirk_fylke_test %>% filter(AVVIRKAAR == 2021) %>% group_by(FYLKENR) %>% summarise(n = n(), vol = sum(as.numeric(TOTALVOLUM)))
# avvirk_fylke_test %>% filter(AVVIRKAAR == 2021, FYLKENR == "03")




ld_avvirk_kommune <- function(){
  # kommunevise avvirkningsstatistikk fra landbruksdirektoratets excel-filer
  # kommunevis pr år
  files <- list.files( path = "./data-raw",
                       pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Kommunevis_avvirkning"))]

  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T, col_types = "text")[NULL, ]
  for (i in seq_along(files)) {

    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T, col_types = "text"))
  }
  df <- df %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(
      avvirkaar = as.numeric(avvirkaar),
      sortkode = as.numeric(sortkode),
      totalvolum = as.numeric(stringr::str_replace_all(totalvolum, " ", "")),
      totalverdi = as.numeric(stringr::str_replace_all(totalverdi, " ", "")),
      m3pris = as.numeric(stringr::str_replace_all(m3pris, " ", "")))
  return(df)
}


avvirk_kmn_ldir <- ld_avvirk_kommune()

avvirk_kmn_ldir_regkorigert <-  regnavn.at.ref.yr(avvirk_kmn_ldir %>% dplyr::mutate(region_kode = komnr, aar = avvirkaar))
avvirk_kmn_ldir_regkorigert2 <- avvirk_kmn_ldir_regkorigert %>%
  dplyr::group_by_at(vars(starts_with("reg_"), aar, sortkode, sortiment, virkesgrp, virkeskat, kategoritekst)) %>%
  dplyr::summarise(totalvolum = sum(totalvolum, na_rm = TRUE),
                   totalverdi = sum(totalverdi, na_rm = TRUE),
                   m3pris = totalverdi / totalvolum,
                   region_kode = paste0(unique(region_kode), collapse = ", ")) %>%
  dplyr::ungroup() %>%
  mutate(fylke_k2025 = substring(reg_k2025, first = 1, last = 2)) %>% left_join(Fylkeskoder, by = c("fylke_k2025" = "regionkode"))

unique(avvirk_kmn_ldir_regkorigert$reg_n2025)
unique(avvirk_kmn_ldir_regkorigert$reg_k2025)
usethis::use_data(avvirk_kmn_ldir, overwrite = T, version = 3)
usethis::use_data(avvirk_kmn_ldir_regkorigert, overwrite = T, version = 3)




