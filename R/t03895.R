####### t03895
#' Skogsavvirkning volum t03895
#' Hogststatistikk for rundvirke SSB tabell 03895
#' 1996 - dd
#'
#' Tabellen gir avvirkningsvolum for salg, etter sortiment, kommune, år.
#' Volum er avregningsvolum (m3pris)
#'
#' @param region_level regionnivaa; velg ett av landet, fylke or kommune
#'
#' @return en tibble
#' @export
#' @importFrom rlang .data
#' @examples
#' volumstat <- t03895()
#' volumstat2 <- t03895(region_level = "fylke")
#' volumstat2 %>% filter(region == "Trøndelag" ) %>% group_by(region, aar) %>% summarise(m3 = sum(m3, na.rm =TRUE)) %>%  head()
#' volumstat3 <- t03895(region_level = "kommune")
#' unique(volumstat3$region)
#' volumstat3 %>% filter(region == "Steinkjer") %>% group_by(region, aar) %>% summarise(m3 = sum(m3, na.rm = TRUE)) %>%  tail()
t03895 <- function(region_level = c("fylker", "kommuner")[1]) { # 1996 - dd

  # region_level = "fylker"
  # region_level = "kommuner"

  #metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895", returnMetaData = TRUE)

  #regcodes <- unlist(purrr::flatten(metadt[[1]][3]))
  #fylker <- regcodes[stringr::str_length(regcodes) == 2]
  #kommuner <-  regcodes[stringr::str_length(regcodes) == 4]


  file_path_flk <- system.file("extdata", "agg_summer_fylker_20250317.json", package = "vsop")
  file_path_kmn <- system.file("extdata", "agg_summer_kommuner_20250317.json", package = "vsop")

  # Check if the file exists
  if (file.exists(file_path_flk) & file.exists(file_path_kmn)) {
    # Read the JSON file into a variable
    region_agg_flk <- jsonlite::fromJSON(file_path_flk)
    region_agg_kmn <- jsonlite::fromJSON(file_path_kmn)
  } else {
    stop("File 'agg_summer_fylker_20250317.json' or
         'agg_summer_kommuner_20250317.json'not found: ", file_path_flk)
  }


  if(tolower(region_level) %in% c("fylker", "fylke")){
    reginnd <- "fylker"
      region_agg <- region_agg_flk
  } else {
    reginnd <- "kommuner"
    region_agg <- region_agg_kmn
  }



    region_category <- region_agg$dimension$Region$category$label
    region_index <- region_agg$dimension$Region$category$index

    region_df <- data.frame(
      Category = names(region_category),
      Label = unlist(region_category),
      Index = unlist(region_index[names(region_category)])
    )

    # str(region_df)




    pxdt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895" , #returnMetaFrames = T)
                                  # Volum per sortiment (kr per m3) (K)
                                  # tidsserie 2006 - 2019
                                  # NB: 0 betyr NULL
                                  # Region = reginnd,
                                  Region = list(
                                    ifelse( reginnd == "fylker",  "agg:KommFylker", "agg:KommSummer"),
                                    region_df$Category),
                                  #ContentsCode = T,
                                  Tid = T,
                                  Treslag = T
    )


    # regioner_utvalg <- tibble::as_tibble(pxdt[[2]])  %>%
    #   dplyr::group_by(.data$Region) %>% dplyr::summarize( harverdi = sum("value", na.rm = T)) %>%
    #   dplyr::filter( .data$harverdi > 0)  %>%
    #   dplyr::pull(Region)

    pxdt_en <-
      PxWebApiData::ApiData12("http://data.ssb.no/api/v0/en/table/03895",
                                       #Region =   reginnd,
                                       Region = list(
                                         ifelse( reginnd == "fylker",  "agg:KommFylker", "agg:KommSummer"),
                                         region_df$Category),

                                       #Region = list("agg:KommSummer",region_df$Category),
                                       Tid = T, #c("2010", "2016", "2017"),
                                       Treslag = T # 10i)
    )  %>%
      dplyr::mutate(aar = as.numeric(.data$Tid)) %>%
    dplyr::rename(sortimentkode = "Treslag") %>%
    dplyr::mutate(
      species = dplyr::case_when(
        stringr::str_detect(.data$sortimentkode, "^1") ~ "Spruce",
      stringr::str_detect(.data$sortimentkode, "^2") ~ "Pine",
      stringr::str_detect(.data$sortimentkode, "^3") ~ "Broadleaves",
      TRUE ~ "Unknown"
      ) ,
      assortm.grp = dplyr::case_when(
        (.data$sortimentkode %in% c("1160", "2160") |
         stringr::str_sub(.data$sortimentkode, 1,2) %in% c("13", "23"))  ~ "mix",
      stringr::str_sub(.data$sortimentkode, 1,2) %in% c("11", "21", "31")  ~ "saw",
      stringr::str_sub(.data$sortimentkode, 1,2) %in% c("14", "24", "34") ~ "pulp",
      TRUE ~ "annet"
    )) %>% dplyr::select("region", "assortm.grp", "aar", regionkode = "Region", "sortimentkode", m3 = "value", "species")

    # pxdt_en %>% filter(sortimentkode %in% c(1160)) %>% str()

    pxdt_no <- PxWebApiData::ApiData12("http://data.ssb.no/api/v0/no/table/03895",
                                  #Region =   reginnd,
                                  Region = list(ifelse( reginnd == "fylker",  "agg:KommFylker", "agg:KommSummer"),region_df$Category),
                                  Tid = T, #c("2010", "2016", "2017"),
                                  Treslag = T # 10i)
                                  )  %>%
    dplyr::mutate(aar = as.numeric(.data$Tid)) %>%

    dplyr::rename(sortimentkode = "Treslag") %>%
    dplyr::mutate(
      treslag = dplyr::case_when(
        stringr::str_detect(.data$sortimentkode, "^1") ~ "Gran",
        stringr::str_detect(.data$sortimentkode, "^2") ~ "Furu",
        stringr::str_detect(.data$sortimentkode, "^3") ~ "Lauv",
        TRUE ~ "Ukjent"
      ) ,
      sortimentgruppe = dplyr::case_when(
        (.data$sortimentkode %in% c("1160", "2160") |
           stringr::str_sub(.data$sortimentkode, 1,2) %in% c("13", "23"))  ~ "sams",
        stringr::str_sub(.data$sortimentkode, 1,2) %in% c("11", "21", "31")  ~ "skur",
        stringr::str_sub(.data$sortimentkode, 1,2) %in% c("14", "24", "34") ~ "massevirke",
        TRUE ~ "annet"
      )) %>% dplyr::select("region", "sortiment", "aar", regionkode = "Region", "sortimentkode", m3 = "value", "treslag", "sortimentgruppe")
  #  glimpse(pxdt_no)
 pxdt_all <- pxdt_no %>% dplyr::left_join(pxdt_en, by = c("region", "regionkode", "aar", "sortimentkode", "m3"))
  #pxdt_all %>% filter( stringr::str_sub(.data$sortimentkode, 1,2) %in% c("11", "21", "31")) %>%  filter(.data$m3>0) %>%
  #  filter(stringr::str_detect(region, "Trøndelag" )) %>% pull(.data$aar) %>% table()

  return(pxdt_all)

}
