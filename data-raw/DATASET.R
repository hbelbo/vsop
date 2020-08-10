## code to prepare `DATASET` dataset goes here

# 1 creating the regref tables
region_at_time_txtfls <- function(filename){
  # function returning the mapping from one region name and code tag to the next according to the SSB region classification
  #Fylker: https://www.ssb.no/en/klass/klassifikasjoner/104/versjon/1158/koder
  #Kommuner: https://www.ssb.no/en/klass/klassifikasjoner/131

  #   filename = files[1] filename = files[2]
  #readr::guess_encoding(filename)
  datastring <- readLines(filename, n=-1L,
                         encoding =  dplyr::pull(readr::guess_encoding(filename)[1,1]),
                         warn = F)#nchars = 10^6)

  headings <- unlist(stringr::str_split(datastring[1], "\t"))
  headings <-  stringr::str_replace_all(
    tolower(headings),
    (c("januar" = "jan", "februar" = "feb", "mars" = "mar", "april" = "apr", "mai" = "may",
       "juni" = "jun", "juli" = "jul", "august" = "aug", "september" = "sept",
       "oktober" = "oct", "november" = "nov", "desember" = "dec" )))

  datastring = datastring[-1]

  if((length(headings)%% 2) == 0 & length(datastring)>1) {
    # Then string should be arranged to pairs of "froms" and "tos"
    datastring = stringr::str_remove(datastring, pattern = "\t")
    convtable = matrix(data = datastring, ncol = 2, byrow = T)
    colnames(convtable) = headings
    headingsinv = unlist(lapply(X= stringr::str_split(headings, " "), FUN = function(X){paste0(X[2]," ",  X[1], " 1")}))

    colnamecandidates = stringr::str_sub(
      stringr::str_replace_all(
        string = lubridate::ymd(headingsinv),
        pattern = "-",
        replacement = ""),
      start=1, end = 6)

    convtable = dplyr::as_tibble(as.data.frame(convtable, stringsAsFactors=F))
    froms =  dplyr::as_tibble(
      stringr::str_split(
        string =  dplyr::pull(convtable[,1]), pattern = " - ", n=2, simplify = T))
    colnames(froms) = paste(c("reg_code", "reg_name"), rep(colnamecandidates[1], 2), sep = "_")

    tos = dplyr::as_tibble(
      stringr::str_split(
        string =  dplyr::pull(convtable[,2]), pattern = " - ", n=2, simplify = T))
    colnames(tos) = paste(c("reg_code", "reg_name"), rep(colnamecandidates[2], 2), sep = "_")

    fromstos = dplyr::bind_cols(froms, tos)
  } else if((length(headings)%% 2) == 1 & length(datastring)>1) {
    # Then it is the starting point, i.e, first array of region units

    convtable = matrix(data = datastring, ncol = 1, byrow = T)
    #colnames(convtable) = headings
    headingsinv =
      unlist(lapply(X= stringr::str_split(headings, " "),
                    FUN = function(X){ paste0(X[2]," ",  X[1], " 1")
                    }))
    colnamecandidates =
      stringr::str_sub(
        stringr::str_replace_all(
          string =  lubridate::ymd(headingsinv),
          pattern = "-",
          replacement = ""), start=1, end = 6)

    convtable = as.data.frame(convtable, stringsAsFactors=F )
    froms =  data.frame(
      stringr::str_split(
        string =  convtable[,1],
        pattern = " - ",
        n=2,
        simplify = T), stringsAsFactors = F )

    colnames(froms) = paste(c("reg_code", "reg_name"), rep(colnamecandidates[1], 2), sep = "_")

    fromstos = dplyr::as_tibble(froms)

  } else  if((length(headings)%% 2) == 0 & length(datastring) == 0) { # THen it is an empty update but we still need the "update dates"

    headingsinv = unlist(lapply(X= stringr::str_split(headings, " "), FUN = function(X){paste0(X[2]," ",  X[1], " 1")}))
    colnamecandidates = stringr::str_sub(
      stringr::str_replace_all(
        string =  lubridate::ymd(headingsinv),
        pattern = "-",
        replacement = ""),
      start=1, end = 6)

    froms = data.frame(
      matrix(
        data = c("a", "b"),
        ncol = 2, byrow = T)[NULL, ],
      stringsAsFactors = F)
    colnames(froms) =
      paste(c("reg_code", "reg_name"), rep(colnamecandidates[1], 2), sep = "_")

    tos = data.frame(
      matrix(data = c("a", "b"),
             ncol = 2, byrow = T)[NULL, ],
      stringsAsFactors = F)
    colnames(tos) = paste(c("reg_code", "reg_name"), rep(colnamecandidates[2], 2), sep = "_")

    fromstos = dplyr::bind_cols(dplyr::as_tibble(froms), dplyr::as_tibble(tos))
  } else {fromstos = NULL}


  return(fromstos)
}


regupdated = function(files){
  regiondef = region_at_time_txtfls(filename = files[1])

  for (i in 2:length(files)){
    print(i)
    regupdate = region_at_time_txtfls(filename = files[i])
    ## !! coming left_join: It would be best to find a way to join only by the "reg_code_x" variables but I could not find how to type this :-(
    both = dplyr::left_join(regiondef, regupdate)
    head(both)
    regupnames <- names(both)
    regupnamesl <- length(regupnames)
    both %>%
      dplyr::mutate(
                    !!sym(regupnames[regupnamesl-1]) :=
                      dplyr::case_when(
                        !is.na(!!dplyr::sym(regupnames[regupnamesl-1])) ~
                          !!dplyr::sym(regupnames[regupnamesl-1]),
                        TRUE ~ !!dplyr::sym(regupnames[regupnamesl-3])),
                    !!sym(regupnames[regupnamesl]) :=
                      dplyr::case_when(
                        !is.na(!!dplyr::sym(regupnames[regupnamesl])) ~ !!dplyr::sym(regupnames[regupnamesl]),
                        TRUE ~ !!dplyr::sym(regupnames[regupnamesl-2]))
      )  -> regiondef
  }
  return(regiondef)
}

no.regiontabell.flk = function(){
  #Fylker: https://www.ssb.no/en/klass/klassifikasjoner/104/versjon/1158/koder
  files <-   list.files( path = "./data-raw", pattern = ".txt", full.names = T)
  files <-   files[which(!stringr::str_detect(files, "~"))]
  files <-   files[which(stringr::str_detect(files, "Regindeling_Fylker"))]
  print(files)
  inndeling <- regupdated(files = files)
  return(inndeling)
}
#no.regiontabell.flk()

no.regiontabell.kmn = function(){
  #Kommuner: https://www.ssb.no/klass/klassifikasjoner/131
  files <-   list.files(path = "./data-raw", pattern = ".txt", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Regindeling_Kommuner"))]
  print(files)
  inndeling <- regupdated(files = files)
  return(inndeling)

}

# create the regref tables ----
regref_fylke <- no.regiontabell.flk()
regref_kommune <- no.regiontabell.kmn()

# creating a "long" mapping table between present region codes and current region codes.
prolong_region_mapping_table <- function(wide_region_mapping_table) {
  # for testing: wide_region_mapping_table = regref_kommune
  regref_codes <- wide_region_mapping_table %>% select( starts_with("reg_code"))
  regref_names <- wide_region_mapping_table %>% select( starts_with("reg_name"))

  start_states <- data.frame()
  for (i in sequence(ncol(regref_codes) )) {
    # 1: fetch the region codes and region names for one "state" (one column)
    one_state_c = regref_codes[,i] %>%
      tidyr::pivot_longer(  cols = everything(), names_to = "time_from",  values_to = "region_code" ) %>%
      mutate( ymfrom = stringr::str_extract(time_from, "\\d{6}"),
             yfrom = stringr::str_extract(ymfrom, "\\d{4}")) %>%
      select( ymfrom, yfrom, reg_code_from = region_code)

    one_state_n = regref_names[,i] %>%
      tidyr::pivot_longer(  cols = everything(), names_to = "time_from",  values_to = "region_name" ) %>%
      mutate( ymfrom = stringr::str_extract(time_from, "\\d{6}"),
             yfrom = stringr::str_extract(ymfrom, "\\d{4}")) %>%
      select( reg_name_from = region_name)
    # 2 bind these together in a n by 2 table
    one_state <- dplyr::bind_cols(one_state_c, one_state_n)

    start_states <- dplyr::bind_rows(start_states, one_state)

  }

  # Then make a table providing the present state for
  # each of the start states. I.e. a table equally long to start_states,
  # put together by stacking the last columnt in regref_codes and regref_names
  # until correct length
  end_states <- data.frame()
      one_state_c = regref_codes[,ncol(regref_codes)] %>%
      tidyr::pivot_longer(  cols = everything(), names_to = "end_time",  values_to = "region_code" ) %>%
      mutate( ymto = stringr::str_extract(end_time, "\\d{6}"),
             yto = stringr::str_extract(ymto, "\\d{4}")) %>%
      select(  ymto, yto, reg_code_to = region_code )

    one_state_n = regref_names[,ncol(regref_names)] %>%
      tidyr::pivot_longer(  cols = everything(), names_to = "end_year",  values_to = "region_name" ) %>%
      mutate( ymto = stringr::str_extract(end_year, "\\d{6}"),
             yto = stringr::str_extract(ymto, "\\d{4}")) %>%
      select(  reg_name_to = region_name)

  for (i in sequence(ncol(regref_codes) )) {
    one_state <- dplyr::bind_cols(one_state_c, one_state_n)
    end_states <- dplyr::bind_rows(end_states, one_state)
  }

  region_mapping_l <- dplyr::bind_cols(start_states, end_states)

  return(region_mapping_l)

}
regref_kommune_l <- prolong_region_mapping_table(wide_region_mapping_table = regref_kommune)
regref_fylke_l <- prolong_region_mapping_table(wide_region_mapping_table = regref_fylke)


# kpi_t03014 ----
# https://www.ssb.no/statbank/table/03014
#metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03014", returnMetaData = TRUE)
pxdt_t03014 <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03014",
                              Konsumgrp = "TOTAL",
                              Tid = T,
                              ContentsCode = "KpiAar")
kpi_t03014 <-
  tibble::as_tibble(pxdt_t03014[[1]])  %>%
  dplyr::select(  -konsumgruppe, -statistikkvariabel) %>%
  dplyr::rename( kpi = value)



# m3_sortiment_kmn based on ssb t03895 ----
m3_sortiment_kmn <-
  regnavn.at.ref.yr( regionstat = t03895(region_level = "kommune") ) %>%
  dplyr::select( -regrefrow)

# m3_sortiment_flk based on ssb t03895 ----
m3_sortiment_flk <-
  regnavn.at.ref.yr( regionstat = t03895(region_level = "fylke") ) %>%
  dplyr::select( -regrefrow)

# virkesverdi_kmn based on ssb t03794 ----
virkesverdi_kmn <-
  regnavn.at.ref.yr(
    regionstat = t03794( region_level = "kommune")) %>%
  dplyr::select( -regrefrow)

# virkesverdi_flk based on ssb t03794 ----
virkesverdi_flk <-
  regnavn.at.ref.yr(
    regionstat = t03794( region_level = "fylke")) %>%
  dplyr::select( -regrefrow)

# virkesverdi_kmn based on ssb t12750
sortimentpriser_kmn <-
  regnavn.at.ref.yr(
    regionstat = t12750()) %>%
    dplyr::select( -regrefrow)



# hogst volum verdi data fra landbruksdirektoratets excel-filer ----
ld_avvirk_fylke <- function() {
# fylkesvise hogstdata fra landbruksdirektoratets excel-filer
# fylkesvis pr mnd
  files <-   list.files(path = "./data-raw",
                        pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Fylkesvis avvirkning pr m"))]


  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T)[NULL, ]
  for (i in seq_along(files)) {
    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T))
  }
  return(df)
}
hogst_fylke_ld <- ld_avvirk_fylke()

ld_avvirk_kommune <- function() {
  # kommunevise avvirkningsstatistikk fra landbruksdirektoratets excel-filer
  # kommunevis pr år
  files <- list.files( path = "./data-raw",
                      pattern = ".xlsx", full.names = T)
  files <- files[which(!stringr::str_detect(files, "~"))]
  files <- files[which(stringr::str_detect(files, "Kommunevis avvirkning"))]

  df <- readxl::read_xlsx(path = files[1], sheet = 1, skip = 2, col_names = T)[NULL, ]
  for (i in seq_along(files)) {
    df <- rbind(df, readxl::read_xlsx(path = files[i], sheet = 1, skip = 2, col_names = T))
  }
  return(df)
}
hogst_kommune_ld <- ld_avvirk_kommune()

# include the datasets in the package: usethis::use_data ----
usethis::use_data(
  m3_sortiment_kmn,
  m3_sortiment_flk,
  virkesverdi_kmn,
  virkesverdi_flk,
  sortimentpriser_kmn,
  regref_kommune,
  regref_fylke,
  regref_kommune_l,
  regref_fylke_l,
  kpi_t03014,
  hogst_fylke_ld,
  hogst_kommune_ld,

  overwrite = T)
