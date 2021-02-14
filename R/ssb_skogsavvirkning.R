##### t03794
#' Skogsavvirkning bruttoverdi t03794
#'
#' gross value of roundwood, pr year, county / municipality,
#'
#' total value of all roundwood sold per year and geographical unit, 1996 - now.
#' Not sure if energywood assortments and wood is included
#' https://www.ssb.no/statbank/list/skogav
#'
#' @param region_level Select type oneof: "fylke", "kommune"
#'
#' @return a tibble with the entire dataset
#' @export
#' @importFrom rlang .data
#' @source \url{https://www.ssb.no/statbank/table/03794/}
#'
#' @examples
#'  t03794()
t03794 <- function(region_level = "fylke"){

  if (!(region_level %in% c("fylke", "kommune"))) {stop("warning: to get result, ret should be one of 'fylker', 'kommuner'" )}
  metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03794", returnMetaData = TRUE)
  regs <- unlist(purrr::flatten(metadt[[1]][3]))

  kommuner <- regs[stringr::str_length(regs) == 4]
  fylker <- regs[stringr::str_length(regs) == 2]


  region_levels <- list(kommune = kommuner, fylke = fylker)
  regionlevelselector <- which(names(region_levels) == region_level)

  pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03794",
                                #Region = T, #c(landet, region_levels[[regionlevelselector]]),
                                Region = region_levels[[regionlevelselector]],
                                Tid = T, #c("2010", "2016", "2017"),
                                ContentsCode = T # 10i)
  )

  regioner_utvalg <- # identify regions having volumes
    tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( bruttoverdi = .data$value) %>%
    dplyr::group_by(.data$region) %>%
    dplyr::summarize( volumtot = sum(.data$bruttoverdi, na.rm = T)) %>%
    dplyr::filter( .data$volumtot > 0) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = .data$Region)

  bruttov <- tibble::as_tibble(pxdt[[1]])  %>%

    dplyr::rename(  bruttoverdi = .data$value) %>%
    dplyr::bind_cols( (ds %>% dplyr::select( .data$region_kode, .data$Tid))) %>%
    dplyr::filter( .data$region %in% regioner_utvalg) %>%
    dplyr::mutate( aar = as.integer(.data$Tid))

  return(bruttov)

}

##### t12750
#' Skogsavvirkning priser t12750
#' prisstatistikk for virke fra SSB tabell 12750
#'
#' Tabellen gir snittpris per sortiment per fylke, fra 2006 til 2019.
#' Virke blir klassifisert per treslag og sortimentgrupper (skur, massevirke, sams, annet)
#' https://www.ssb.no/statbank/list/skogav
#'
#' @return en tibble med hele datasetet.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' t12750()
t12750 <- function(){
  metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/12750", returnMetaData = TRUE)
  regs <- unlist(purrr::flatten(metadt[[1]][3]))
  fylker <- regs[stringr::str_length(regs) == 2]

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
  regs <- unlist(purrr::flatten(metadt[[1]][3]))
  fylker <- regs[stringr::str_length(regs) == 2]


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

####### t03895
#' Skogsavvirkning volum t03895
#' Hogststatistikk for rundvirke SSB tabell 03895
#' 1996 - dd
#'
#' Tabellen gir avvirkningsvolum for salg, etter sortiment, kommune, year.
#' Volum er avregningsvolum (m3pris)
#'
#' @param region_level regionnivaa; velg ett av landet, fylke or kommune
#'
#' @return en tibble
#' @export
#' @importFrom rlang .data
#' @examples
#' t03895() %>% dplyr::glimpse()
t03895 <- function(region_level = "fylke"){ # 1996 - dd

  if ( !(region_level %in% c("fylke", "kommune"))) { stop("warning: to get result, region_level should be one of 'fylker', 'kommune'" )}
  metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895", returnMetaData = TRUE)
  regs <- unlist(purrr::flatten(metadt[[1]][3]))

  kommuner <- regs[stringr::str_length(regs) == 4]
  fylker <- regs[stringr::str_length(regs) == 2]
  landet <- regs[stringr::str_length(regs) == 1]

  s <- list(kommune = kommuner, fylke = fylker, landet = landet)
  regionlevelselector <- which(names(s) == region_level)

  pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895",
                    Region = s[[regionlevelselector]],
                    Tid = T, #c("2010", "2016", "2017"),
                    Treslag = T # 10i)
  )

  regioner_utvalg <-
    tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( volum_m3pris = .data$value) %>%
    dplyr::group_by(.data$region) %>%
    dplyr::summarize( volumtot = sum(.data$volum_m3pris, na.rm = T)) %>%
    dplyr::filter( .data$volumtot > 0) %>%
    dplyr::pull( .data$region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = .data$Region,  virkeskategori = .data$Treslag)

  volum <- tibble::as_tibble(pxdt[[1]])  %>%

    dplyr::rename(  kategoritekst = .data$sortiment, volum_m3pris = .data$value) %>%
    dplyr::bind_cols( (ds %>% dplyr::select( .data$region_kode, .data$virkeskategori, .data$Tid))) %>%
    dplyr::filter( .data$region %in% regioner_utvalg) %>%
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
  return(volum)

}
