

##### t12750
#' Skogsavvirkning priser t12750
#' prisstatistikk for virke fra SSB tabell 12750
#'
#' Tabellen gir snittpris per sortiment per fylke, fra 2006 til dd.
#' Virke blir klassifisert per treslag og sortimentgrupper (skur, massevirke, sams, annet)
#' https://www.ssb.no/statbank/list/skogav
#'
#' @return en tibble med hele datasetet.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' prisstat <- t12750()
t12750 <- function(){
  metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/12750", returnMetaData = TRUE)
  regcodes <- unlist(purrr::flatten(metadt[[1]][3]))
  fylker <- regcodes[stringr::str_length(regcodes) == 2]

  pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/12750" , #returnMetaFrames = T)
                    # Gjennomsnittspris, etter sortiment (kr per m3) (F)
                    # tidsserie 2006 - 2019
                    # NB: 0 betyr NULL
                    Region = fylker, ContentsCode = T,
                    Tid = T,
                    Treslag = T
  )
  regioner_utvalg <- tibble::as_tibble(pxdt[[2]])  %>%
    dplyr::group_by(.data$Region) %>% dplyr::summarize( harpris = sum(.data$value, na.rm = T)) %>%
    dplyr::filter( .data$harpris > 0)  %>%
    dplyr::pull(.data$Region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = .data$Region,  virkeskategori = .data$Treslag, Pris = .data$value) #Modding variable names

  priser <- tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( kategoritekst = .data$sortiment, pris = .data$value) %>%
    dplyr::bind_cols( (ds %>% dplyr::select(.data$region_kode, .data$virkeskategori, .data$Tid))) %>%
    dplyr::filter(.data$region_kode %in% regioner_utvalg)  %>%
    dplyr::mutate(
           treslag = dplyr::case_when(
             stringr::str_detect(.data$kategoritekst, "Gran") ~ "Gran",
             stringr::str_detect(.data$kategoritekst, "Furu") ~ "Furu",
             stringr::str_detect(.data$kategoritekst, "Lauvtre") ~ "Lauv",
             TRUE ~ "Ukjent"
           ),
           sortimentgruppe = dplyr::case_when(
             (.data$virkeskategori %in% c("1160", "2160") |
               stringr::str_sub(.data$virkeskategori, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
             stringr::str_sub(.data$virkeskategori, 1,2) %in% c("11", "21", "31")  ~ "skur",
             stringr::str_sub(.data$virkeskategori, 1,2) %in% c("14", "24", "34") ~ "massevirke",
             TRUE ~ "annet"
           ),
           aar = as.integer(.data$Tid)
           )
  return(priser)
}



##### t06216
#' Skogsavvirkning priser t06216
#' prisstatistikk for rundvirke SSB tabell 06216
#'
#' Tabellen gir snittpris per sortiment per fylke, fra 1996 til 2017.
#' Virke blir også klassifisert etter treslag og sortimentgrupper (skur, massevirke, sams, annet)
#' https://www.ssb.no/statbank/list/skogav#'
#'
#'
#' @return en tibble med hele datasetet.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' t06216()
t06216 <- function(){ # NB: avslutta, tidsserie 1996 - 2017

  # Gjennomsnittspris, etter sortiment (kr per m3) (F)
  metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/06216", returnMetaData = TRUE)
  regcodes <- unlist(purrr::flatten(metadt[[1]][3]))
  fylker <- regcodes[stringr::str_length(regcodes) == 2]


  pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/06216",
                    Region = fylker,
                    Tid = T,
                    Treslag = T   )
  regioner_utvalg <- tibble::as_tibble(pxdt[[2]])  %>%
    dplyr::group_by(.data$Region) %>% dplyr::summarize( harpris = sum(.data$value, na.rm = T)) %>%
    dplyr::filter( .data$harpris > 0) %>%
    dplyr::pull( .data$Region)


  priser <- tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( kategoritekst = .data$sortiment, pris = .data$value)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = .data$Region, virkeskategori = .data$Treslag, Pris = .data$value) #Correcting missleading variable names

  priser <-
    priser %>% dplyr::bind_cols( (ds %>% dplyr::select( .data$region_kode, .data$virkeskategori, .data$Tid))) %>%
    dplyr::filter( .data$region_kode %in% regioner_utvalg)  %>%
    dplyr::mutate( treslag = dplyr::case_when(
                    stringr::str_detect(.data$kategoritekst, "Gran") ~ "Gran",
                    stringr::str_detect(.data$kategoritekst, "Furu") ~ "Furu",
                    stringr::str_detect(.data$kategoritekst, "Lauvtre") ~ "Lauv",
                    TRUE ~ "Ukjent"
                  ),
                  sortimentgruppe = dplyr::case_when(
                    (.data$virkeskategori %in% c("1160", "2160") |
                       stringr::str_sub(.data$virkeskategori, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
                    stringr::str_sub(.data$virkeskategori, 1,2) %in% c("11", "21", "31")  ~ "skur",
                    stringr::str_sub(.data$virkeskategori, 1,2) %in% c("14", "24", "34") ~ "massevirke",
                    TRUE ~ "annet"
                  ),
                  aar = as.integer(.data$Tid)
    )
  return(priser)
}

