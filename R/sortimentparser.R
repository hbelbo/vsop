

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
                             grovsortiment ==  "Ved til brensel bar"~ "Conifer Woodfuel",
                             TRUE ~ sortiment)
    )
  return(tbbl)
}
