

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
      stringr::str_detect(sortiment, "sams skurtømmer og massevirke$") ~ "sams",
      stringr::str_detect(sortiment, "spesialtømmer$|skurtømmer$|sagtømmer$") ~ "skur",
      stringr::str_detect(sortiment, "massevirke$") ~ "masse",
      stringr::str_detect(sortiment, "annet rundvirke$") ~ "masse",
      stringr::str_detect(sortiment, "ved til brensel bar") ~ "energi",
      TRUE ~ sortiment)
  ) %>%
    dplyr::mutate(
      species = dplyr::case_when(treslag == "Gran" ~ "Spruce",
                          treslag == "Furu" ~ "Pine",
                          treslag == "Lauv" ~ "Broadleave",
                          treslag == "Bar" ~ "Conifer",
                          treslag == "Annet" ~ "Other",
                          TRUE ~ treslag),
      assortment = dplyr::case_when(grovsortiment == "skur" ~ "saw",
                             grovsortiment == "masse" ~ "pulp",
                             grovsortiment == "Sams" ~ "mix",
                             grovsortiment ==  "energi"~ "energy",
                             TRUE ~ sortiment)
    )
  return(tbbl)
}





#' Sortimentparser_vsop
#'
#' @param tbbl a tibble having the variables  "ortiment" and "
#'
#' @returns a tibble
#' @export
#'
#' @examples
#' sortimentparser_vsop(avvirk_kmn_ldir_regkorigert) %>% print(n = 20)
#' sortimentparser_vsop(avvirk_kmn_ldir_regkorigert) %>% group_by(sortkode, virkesgrp, sortiment, virkeskat, kategoritekst, treslag, grovsortiment) %>% summarise(vm = sum(totalvolum)) %>% arrange(sortkode, virkesgrp, virkeskat) %>%   print(n = 40)
sortimentparser_vsop <- function(tbbl){
  tbbl <- tbbl %>% dplyr::mutate(
    treslag = dplyr::case_when(
      stringr::str_ends(virkesgrp, "Gran") ~ "Gran",
      stringr::str_ends(virkesgrp, "Furu") ~ "Furu",
      stringr::str_ends(virkesgrp, "Lauv") ~ "Lauv",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Lauv ved ") ~ "Lauv",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Rundvirke til flis - lauv") ~ "Lauv",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Bar ved ") ~ "Bar",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Rundvirke til flis - bar") ~ "Bar",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Alle treslag ") ~ "Mix",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Heltreflis, lauv/bar" ) ~ "Mix",
      stringr::str_ends(virkesgrp, "Ved") & stringr::str_detect(kategoritekst, "^Grotflis") ~ "Mix",
      TRUE ~ "Annet"),
    grovsortiment =  dplyr::case_when(
      virkeskat %in% c("1140", "1148", "1110") ~ "skur",
      virkeskat %in% c("2140", "2148", "2110") ~ "skur",
      virkeskat %in% c("3140",  "3110") ~ "skur",

      virkeskat %in% c("1410", "1490") ~ "masse",
      virkeskat %in% c("2410", "2490") ~ "masse",
      virkeskat %in% c("3400") ~ "masse",

      virkeskat %in% c("1310") ~ "sams",
      virkeskat %in% c("2310") ~ "sams",


      stringr::str_detect(sortiment, "Vrak") ~ "vrak",
      stringr::str_detect(sortiment, "Energi") ~ "energi",
    TRUE ~ sortiment))   %>%
    dplyr::mutate(
      species = dplyr::case_when(treslag == "Gran" ~ "Spruce",
                                 treslag == "Furu" ~ "Pine",
                                 treslag == "Lauv" ~ "Broadleave",
                                 treslag == "Bar" ~ "Conifer",
                                 treslag == "Mix" ~ "Mix",
                                 treslag == "Annet" ~ "Other",
                                 TRUE ~ treslag),
      assortment = dplyr::case_when(grovsortiment == "skur" ~ "saw",
                                    grovsortiment == "masse" ~ "pulp",
                                    grovsortiment == "sams" ~ "mix",
                                    grovsortiment == "vrak" ~ "reject",
                                    grovsortiment == "energi" ~ "energy",
                                    TRUE ~ sortiment)
    )
  return(tbbl)
}
