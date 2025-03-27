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
#'  t03794(region_level = "kommuner")
#'  t03794(region_level = "fylker")
t03794 <- function(region_level = c("fylker", "kommuner")[1]){


  file_path_flk <- system.file("extdata", "ssbapi_table_03794_agg_single_FylkerGjeldende_20250317.json", package = "vsop")
  file_path_kmn <- system.file("extdata", "ssbapi_table_03794_agg_single_KommGjeldende_20250318.json", package = "vsop")

  # Check if the file exists
  if (file.exists(file_path_flk) & file.exists(file_path_kmn)) {
    # Read the JSON file into a variable
    region_agg_flk <- jsonlite::fromJSON(file_path_flk)
    region_agg_kmn <- jsonlite::fromJSON(file_path_kmn)
  } else {
    stop("File 'ssbapi_table_03794_agg_single_FylkerGjeldende_yyyymmdd.json' or
         'ssbapi_table_03794_agg_single_KommGjeldende_yyyymmdd.json'not found: ", file_path_flk)
  }


    if(tolower(region_level) %in% c("fylker", "fylke")){
    reginnd <- "fylker"
    #region_agg <- jsonlite::fromJSON("inst/extdata/agg_summer_fylker_20250317.json")
    #region_agg <- jsonlite::fromJSON("./json/ssbapi_table_03794_agg_single_FylkerGjeldende_20250317.json")
    region_agg <- region_agg_flk

  } else if(tolower(region_level) %in% c("kommune", "kommuner")) {
    reginnd <- "kommuner"
    region_agg <- region_agg_kmn
    #region_agg <- jsonlite::fromJSON("./json/ssbapi_table_03794_agg_single_KommGjeldende_20250318.json")
    #region_agg <- jsonlite::fromJSON("inst/extdata/agg_summer_kommuner_20250317.json")
  }

  region_values <- region_agg$queryObj$query$selection$values[[1]]


  pxdt <-
    PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03794",
      #Region = T, #c(landet, region_levels[[regionlevelselector]]),
      Region = list(ifelse(reginnd == "fylker", "agg_single:FylkerGjeldende", "agg_single:KommGjeldende"), region_values),
      Tid = T#, #c("2010", "2016", "2017"),
      #ContentsCode = T # 10i)
  )

  regioner_utvalg <- # identify regions having volumes
    tibble::as_tibble(pxdt[[1]])  %>%
    dplyr::rename( bruttoverdi = "value") %>%
    dplyr::group_by(.data$region) %>%
    dplyr::summarize( volumtot = sum(.data$bruttoverdi, na.rm = T)) %>%
    dplyr::filter( .data$volumtot > 0) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$region)

  ds <- tibble::as_tibble(pxdt[[2]]) %>%
    dplyr::rename( region_kode = "Region")

  bruttov <- tibble::as_tibble(pxdt[[1]])  %>%

    dplyr::rename(  bruttoverdi = "value") %>%
    dplyr::bind_cols( (ds %>% dplyr::select( "region_kode", "Tid"))) %>%
    dplyr::filter( .data$region %in% regioner_utvalg) %>%
    dplyr::mutate( aar = as.integer(.data$Tid))

  return(bruttov)

}

