

#' Sortimentparser
#'
#' @param tbbl a tibble having the variable "sortiment"
#'
#' @returns a tibble
#' @export
#'
#' @examples
#' df <- tibble::tibble(sortiment = c("Gran spesialtømmer","Furu massevirke" ))
#' sortimentparser(df)
sortimentparser <- function(tbbl){
  tbbl <- tbbl %>% dplyr::mutate(
    treslag = dplyr::case_when(stringr::str_detect(sortiment, "^Gran") ~ "Gran",
                        stringr::str_detect(sortiment, "^Furu") ~ "Furu",
                        stringr::str_detect(sortiment, "^Lauvtre") ~ "Lauv",
                        stringr::str_detect(sortiment, "bar$") ~ "Bar",
                        stringr::str_detect(sortiment, "lauv$") ~ "Lauv",
                        TRUE ~ "Annet"),
    grovsortiment = dplyr::case_when(
      stringr::str_detect(sortiment, "sams skurtømmer og massevirke$") ~ "Sams",
      stringr::str_detect(sortiment, "spesialtømmer$|skurtømmer$|sagtømmer$") ~ "Skurtømmer",
      stringr::str_detect(sortiment, "massevirke$") ~ "Massevirke",
      stringr::str_detect(sortiment, "annet rundvirke$") ~ "Massevirke",
      TRUE ~ sortiment)
  ) %>%
    dplyr::mutate(
      species = dplyr::case_when(treslag == "Gran" ~ "Spruce",
                          treslag == "Furu" ~ "Pine",
                          treslag == "Lauv" ~ "Broadleave",
                          treslag == "Bar" ~ "Conifer",
                          treslag == "Annet" ~ "Other",
                          TRUE ~ treslag),
      assortment = dplyr::case_when(grovsortiment == "Skurtømmer" ~ "Sawlogs",
                             grovsortiment == "Massevirke" ~ "Pulpwood",
                             grovsortiment == "Sams" ~ "Mix",
                             grovsortiment ==  "Ved til brensel bar"~ "Woodmix",
                             TRUE ~ sortiment)
    )
  return(tbbl)
}





#' Sortimentparser_vsop
#'
#' @param tbbl a tibble having the variables "sortiment" and "
#'
#' @returns a tibble
#' @export
#'
#' @examples
#' sortimentparser_vsop(avvirk_kmn_ldir_regkorigert)
sortimentparser_vsop <- function(tbbl){
  tbbl <- tbbl %>% dplyr::mutate(
    trsl = tolower(virkesgrp),
    treslag = dplyr::case_when(
      stringr::str_detect(trsl, "gran") ~ "Gran",
      stringr::str_detect(trsl, "furu") ~ "Furu",
      stringr::str_detect(trsl, "lauv") ~ "Lauv",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Lauv ved ") ~ "Lauv",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Rundvirke til flis - lauv") ~ "Lauv",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Bar ved ") ~ "Bar",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Rundvirke til flis - bar") ~ "Bar",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Alle treslag ") ~ "Vedmix",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Heltreflis, lauv/bar" ) ~ "Flismiks",
      stringr::str_detect(trsl, "ved") & stringr::str_detect(kategoritekst, "^Grotflis") ~ "Flismiks",
      TRUE ~ "Annet"),
    grovsortiment =  dplyr::case_when(
    stringr::str_detect(sortiment, "Sagtømmer") ~ "Skurtømmer",
    stringr::str_detect(sortiment, "Massevirke") ~ "Massevirke",
    stringr::str_detect(sortiment, "Vrak") ~ "Vrak",
    stringr::str_detect(sortiment, "Energi") ~ "Energi",
    TRUE ~ sortiment))   %>%
    dplyr::mutate(
      species = dplyr::case_when(treslag == "Gran" ~ "Spruce",
                                 treslag == "Furu" ~ "Pine",
                                 treslag == "Lauv" ~ "Broadleave",
                                 treslag == "Bar" ~ "Conifer",
                                 treslag == "Vedmix" ~ "Woodmix",
                                 treslag == "Flismix" ~ "Forest chips mix",
                                 treslag == "Annet" ~ "Other",
                                 TRUE ~ treslag),
      assortment = dplyr::case_when(grovsortiment == "Sagtømmer" ~ "Sawlogs",
                                    grovsortiment == "Massevirke" ~ "Pulpwood",
                                    grovsortiment == "Vrak" ~ "Wrecked",
                                    grovsortiment == "Energi" ~ "Energy",
                                    TRUE ~ sortiment)
    )
  return(tbbl)
}
