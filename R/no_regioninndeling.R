#' Region navn og region kode for gitt tidspunkt
#'
#' Denne funksjonen tar regionkoder og regionnavn fra en regional statistikk,
#' henter inn tabell som viser historiske endringer i regional inndeling av Norge
#' og gjør om til riktige koder og navn for et gitt referanseår (ref.yr)
#' Funksjonen fungerer for fylkesnivå inkludert landet ELLER for kommunenivå.
#'
#' @param regionstat is the regional statistics, where each obs represent one year
#' and one region (fylke eller kommune)
#' The regionstat must have the variables
#' "region_kode" (a Norwegian region code)
#' "aar" (year of registration of the statistics)
#' @param ref.yr is the reference year to which geographic name and code is to be used
#'
#' @return tibble having the regional statistics including the regional
#' names and codes for the reference year in question
#' @export
#' @importFrom rlang :=
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#'
#' @examples regnavn.at.ref.yr(regionstat = t12750()) %>% glimpse()
regnavn.at.ref.yr <- function(regionstat, ref.yr = lubridate::year(lubridate::now())){
  #    regionstat = t12750() #for testing
  #    regionstat = t03895(region_level = "kommune") #for testing
  #   regionstat = t03794()

  # Fetch the relevant region reference table regref
  if (nchar(regionstat$region_kode[1]) == 2) {
    regref <- vsop::regref_fylke_l #regref_fylke_l is a table created in the DATASET script saved in the "data" directory
  } else {
    regref <- vsop::regref_kommune_l #regref_kommune_l is a table created in the DATASET script saved in the "data" directory
  }
  regref <- regref %>% mutate(yfrom = as.integer(.data$yfrom), yto = as.integer(.data$yto))

  # harmonizing
  ref.yr <- as.integer(ref.yr)


  # identify which row in regref the region_code and observation year belongs to:


 regionstat <-
    regionstat %>%
   #  dplyr::mutate(
   #                # which row in regref the region_code and observation year belongs to:
   #                regrefrow = purrr::pmap_int(regionstat, .f = function(region_kode, aar, ...){
   #                  # 1: check if exists a part of regref where "region_code_from" == region_kode AND yfrom <= aar
   #                  tmprr <-
   #                    dplyr::filter(regref, reg_code_from == region_kode, yfrom <= aar)
   #
   #                  if (nrow(tmprr) > 0 ) {
   #                    # yes then tag which row in regref the region_code and observation year belongs to. regref reference row
   #                    rrr <-  max( which(regref$reg_code_from  == region_kode & regref$yfrom <= aar))
   #                  } else {
   #                    rrr <- NA_integer_
   #                  }
   #                  return(rrr)
   #                }))  %>%
   # dplyr::filter( !is.na(.data$regrefrow)) %>%

   dplyr::mutate(
     # which row in regref the region_code and observation year belongs to:
     regrefrow = purrr::pmap_int(regionstat, .f = function(region_kode, aar, ...){
       # 1: check if exists a part of regref where "region_code_from" == region_kode AND yfrom <= aar
       tmprr <- regref %>%  dplyr::filter( .data$reg_code_from == region_kode, .data$yfrom <= aar)

       if (nrow(tmprr) > 0 ) {
         # yes then tag which row in regref the region_code and observation year belongs to. regref reference row
         rrr <-  max( which(regref$reg_code_from  == region_kode & regref$yfrom <= aar))
       } else {
         rrr <- NA_integer_
       }
       return(rrr)
     }))  %>%
   dplyr::filter( !is.na(.data$regrefrow)) %>%

    #   Then we have index needed to pick the right row
    #      to populate both reg_k@ref.yr and reg_n@ref.yr
    dplyr::mutate(
                  !!rlang::sym(paste0("reg_n", ref.yr)) := regref$reg_name_to[(.data$regrefrow)],
                  !!rlang::sym(paste0("reg_k", ref.yr)) := regref$reg_code_to[(.data$regrefrow)]
    )



  return(regionstat)

}







