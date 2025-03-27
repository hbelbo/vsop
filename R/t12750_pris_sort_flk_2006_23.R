
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
#'
t12750 <- function(){
  # metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/12750", returnMetaData = TRUE)
  # #regcodes <- unlist(purrr::flatten(metadt[[1]][3]))
  # #fylker <- regcodes[stringr::str_length(regcodes) == 2]
  #
  #
  # #file_path <- system.file("extdata", "agg_single_fylker_20250317.json", package = "vsop")
  # # Check if the file exists
  # if (file.exists(file_path)) {
  #   # Read the JSON file into a variable
  #   region_agg_ <- jsonlite::fromJSON(file_path)
  # } else {
  #   stop("File 'agg_single_fylker_yyyymmdd.json' not found: ", file_path)
  # }
  #
  #
  # region_category_ <- region_agg_$dimension$Region$category$label
  # region_index_ <- region_agg_$dimension$Region$category$index
  #
  # region_df_ <- data.frame(
  #   Category = names(region_category_),
  #   Label = unlist(region_category_),
  #   Index = unlist(region_index_[names(region_category_)])
  # )
  #
  file_path <- system.file("extdata", "ssbapi_table_12750_filter_FylkerAlle.json", package = "vsop")
  qry <- jsonlite::fromJSON(file_path)
  regionkoder <- qry$queryObj$query$selection$values[[1]]

  pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/12750" , #returnMetaFrames = T)
                                # Gjennomsnittspris, etter sortiment (kr per m3) (F)
                                # tidsserie 2006 - 2019
                                # NB: 0 betyr NULL
                                #Region = list("agg:KommFylker", region_df$Category),
                                #Region = list("agg_single:FylkerGjeldende", region_df_$Category),
                                Region = list("vs:FylkerAlle", regionkoder),
                                #Region = fylker,
                                #ContentsCode = T,
                                Tid = T,
                                Treslag = T
  )
  regioner_utvalg <- tibble::as_tibble(pxdt[[2]])  %>%
    dplyr::group_by(.data$Region) %>% dplyr::summarize( harpris = sum(.data$value, na.rm = T)) %>%
    dplyr::filter( .data$harpris > 0)  %>%
    dplyr::pull(.data$Region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = "Region",  virkeskategori = "Treslag", Pris = "value") #Modding variable names

  priser <- tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( kategoritekst = "sortiment", pris = "value") %>%
    dplyr::bind_cols( (ds %>% dplyr::select("region_kode", "virkeskategori", "Tid"))) %>%
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
    ) %>% dplyr::select("region_kode", "region", "aar", "treslag", "virkeskategori", "sortimentgruppe", "pris", "kategoritekst")
  return(priser)
}

