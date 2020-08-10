##### t03794
#' Skogsavvirkning bruttoverdi t03794
#'
#' bruttoverdi av tømmer (alt virke), pr år, fylke / kommune,
#'
#' totalverdi av tømmer solgt per år og geografisk enhet, fra 1996 - dd.
#' Litt usikker om energivirkesortimenter og ved er med.
#' https://www.ssb.no/statbank/list/skogav
#'
#' @param region_level Select type oneof: "fylke", "kommune"
#'
#' @return en tibble med hele datasetet.
#' @export
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
    dplyr::rename( bruttoverdi = value) %>%
    dplyr::group_by(region) %>%
    dplyr::summarize( volumtot = sum(bruttoverdi, na.rm = T)) %>%
    dplyr::filter( volumtot > 0) %>%
    dplyr::ungroup() %>%
    dplyr::pull(region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = Region)

  bruttov <- tibble::as_tibble(pxdt[[1]])  %>%

    dplyr::rename(  bruttoverdi = value) %>%
    dplyr::bind_cols( (ds %>% dplyr::select( region_kode, Tid))) %>%
    dplyr::filter( region %in% regioner_utvalg) %>%
    dplyr::mutate( aar = as.integer(Tid))

  return(bruttov)

}

##### t12750
#' Skogsavvirkning priser t12750
#' prisstatistikk for virke fra SSB tabell 12750
#'
#' Tabellen gir snittpris per sortiment per fylke, fra 2006 til 2019.
#' Virke blir klassifisert per treslag og sortimentgrupper (tømmmer, massevirke, sams, annet)
#' https://www.ssb.no/statbank/list/skogav
#'
#' @return en tibble med hele datasetet.
#' @export
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
    dplyr::group_by(Region) %>% dplyr::summarize( harpris = sum(value, na.rm = T)) %>%
    dplyr::filter( harpris > 0) %>%
    dplyr::select( Region) %>% dplyr::pull()

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = Region,  virkeskategori = Treslag, Pris = value) #Modding variable names

  priser <- tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( kategoritekst = sortiment, pris = value) %>%
    dplyr::bind_cols(., (ds %>% dplyr::select(region_kode, virkeskategori, Tid))) %>%
    dplyr::filter(region_kode %in% regioner_utvalg)  %>%
    dplyr::mutate(
           treslag = dplyr::case_when(
             stringr::str_detect(kategoritekst, "Gran") ~ "Gran",
             stringr::str_detect(kategoritekst, "Furu") ~ "Furu",
             stringr::str_detect(kategoritekst, "Lauvtre") ~ "Lauv",
             TRUE ~ "Ukjent"
           ),
           sortimentgruppe = dplyr::case_when(
             (virkeskategori %in% c("1160", "2160") |
               stringr::str_sub(virkeskategori, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
             stringr::str_sub(virkeskategori, 1,2) %in% c("11", "21", "31")  ~ "tømmer",
             stringr::str_sub(virkeskategori, 1,2) %in% c("14", "24", "34") ~ "massevirke",
             TRUE ~ "annet"
           ),
           aar = as.integer(Tid)
           )
  return(priser)
}



##### t06216
#' Skogsavvirkning priser t06216
#' prisstatistikk for tømmer SSB tabell 06216
#'
#' Tabellen gir snittpris per sortiment på fylkesnivå, fra 1996 til 2017.
#' Virke blir også klassifisert på treslag og sortimentgrupper (tømmmer, massevirke, sams, annet)
#' https://www.ssb.no/statbank/list/skogav#'
#'
#'
#' @return en tibble med hele datasetet.
#' @export
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
    dplyr::group_by(Region) %>% dplyr::summarize(., harpris = sum(value, na.rm = T)) %>%
    dplyr::filter( harpris > 0) %>%
    dplyr::pull( Region)


  priser <- tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( kategoritekst = sortiment, pris = value)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = Region, virkeskategori = Treslag, Pris = value) #Correcting missleading variable names

  priser <-
    priser %>% dplyr::bind_cols(., (ds %>% dplyr::select( region_kode, virkeskategori, Tid))) %>%
    dplyr::filter( region_kode %in% regioner_utvalg)  %>%
    dplyr::mutate( treslag = dplyr::case_when(
                    stringr::str_detect(kategoritekst, "Gran") ~ "Gran",
                    stringr::str_detect(kategoritekst, "Furu") ~ "Furu",
                    stringr::str_detect(kategoritekst, "Lauvtre") ~ "Lauv",
                    TRUE ~ "Ukjent"
                  ),
                  sortimentgruppe = dplyr::case_when(
                    (virkeskategori %in% c("1160", "2160") |
                       stringr::str_sub(virkeskategori, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
                    stringr::str_sub(virkeskategori, 1,2) %in% c("11", "21", "31")  ~ "tømmer",
                    stringr::str_sub(virkeskategori, 1,2) %in% c("14", "24", "34") ~ "massevirke",
                    TRUE ~ "annet"
                  ),
                  aar = as.integer(Tid)
    )
  return(priser)
}

####### t03895
#' Skogsavvirkning volum t03895
#' Hogststatistikk for tømmer SSB tabell 03895
#' 1996 - dd
#'
#' Tabellen gir avvirkningsvolum for salg, etter sortiment, kommune, år.
#' Volum er avregningsvolum (m3pris)
#'
#' @param region_level regionnivå; velg ett av landet, fylke or kommune
#'
#' @return en tibble
#' @export
#'
#' @examples
#' t03895()
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
    dplyr::rename( volum_m3pris = value) %>%
    dplyr::group_by(region) %>%
    dplyr::summarize( volumtot = sum(volum_m3pris, na.rm = T)) %>%
    dplyr::filter( volumtot > 0) %>%
    dplyr::pull( region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = Region,  virkeskategori = Treslag)

  volum <- tibble::as_tibble(pxdt[[1]])  %>%

    dplyr::rename(  kategoritekst = sortiment, volum_m3pris = value) %>%
    dplyr::bind_cols(., (ds %>% dplyr::select(., region_kode, virkeskategori, Tid))) %>%
    dplyr::filter( region %in% regioner_utvalg) %>%
    dplyr::mutate(
                  treslag = dplyr::case_when(
                    stringr::str_detect(kategoritekst, "Gran") ~ "Gran",
                    stringr::str_detect(kategoritekst, "Furu") ~ "Furu",
                    stringr::str_detect(kategoritekst, "Lauvtre") ~ "Lauv",
                    TRUE ~ "Ukjent"
                  ),
                  sortimentgruppe = dplyr::case_when(
                    (virkeskategori %in% c("1160", "2160") |
                       stringr::str_sub(virkeskategori, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
                    stringr::str_sub(virkeskategori, 1,2) %in% c("11", "21", "31")  ~ "tømmer",
                    stringr::str_sub(virkeskategori, 1,2) %in% c("14", "24", "34") ~ "massevirke",
                    TRUE ~ "annet"
                  ),
                  aar = as.integer(Tid)
                  )
  return(volum)

}
