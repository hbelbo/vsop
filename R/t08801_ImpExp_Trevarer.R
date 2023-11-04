 #' Import Export wood assortments, pr year.
 #'
 #' Import and export 1996 - now.
 #' Not sure if energywood assortments and wood is included
 #'
 #' @return a tibble with the entire dataset
 #' @export
 #' @source \url{https://www.ssb.no/statbank/table/08801/}
 #'
 #' @examples
 #' trestat <-  t08801tre()
t08801tre <- function() {

  # Eksempel fra SSB
  # https://www.ssb.no/api/pxwebapi/api-eksempler-pa-kode/enkelt-r-eksempel-med-ferdig-datasett
  # https://cran.r-project.org/web/packages/PxWebApiData/vignettes/Introduction.html

  #Varekoder
  # https://www.ssb.no/utenriksokonomi/utenrikshandel/artikler/statistisk-varefortegnelse-for-utenrikshandelen/

  variables <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/en/table/08801", returnMetaFrames = TRUE)
  varwhich <-  which(stringr::str_detect(variables[[1]]$values, pattern = "^4401|^4402|^4403" ))
  varvals <-  variables[[1]]$values[varwhich]
  vartext <- variables[[1]]$valueTexts[varwhich]
  descr <- data.frame(varvals = varvals, vartext = vartext)

  variabler <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/08801", returnMetaFrames = TRUE)
  varhvlke <-  which(stringr::str_detect(variabler[[1]]$values, pattern = "^4401|^4402|^4403" ))
  varvals <-  variabler[[1]]$values[varhvlke]
  vartext <- variabler[[1]]$valueTexts[varhvlke]
  beskr <- data.frame(varvals = varvals, vartext = vartext)

engnorsk <- dplyr::left_join(descr, beskr, by = c("varvals")) %>% dplyr::rename(norsk = .data$vartext.y, eng = .data$vartext.x)

engnorsk <- engnorsk %>%
  dplyr::mutate(grovsort = dplyr::case_when(
    stringr::str_detect(string = .data$norsk, pattern = "^Massevirke av furu") ~ "Furu mv",
    stringr::str_detect(string = .data$norsk, pattern = "^Massevirke av gran") ~ "Gran mv",
    stringr::str_detect(string = .data$norsk, pattern = "^Massevirke av bj") ~ "Lauv mv",
    stringr::str_detect(string = .data$norsk, pattern = "^Massevirke av lauv") ~ "Lauv mv",
    stringr::str_detect(string = .data$norsk, pattern = "mmer av furu") ~ "Furu sagt",
    stringr::str_detect(string = .data$norsk, pattern = "mmer av gran") ~ "Gran sagt",
    stringr::str_detect(string = .data$norsk, pattern = "mmer av lauvt") ~ "Lauv sagt",
    stringr::str_detect(string = .data$norsk, pattern = "mmer av bj") ~ "Lauv sagt",
    stringr::str_detect(string = .data$norsk, pattern = "mmer av bartr") ~ "Bar sagt",
    stringr::str_detect(string = varvals, pattern = "44012200_1988") ~ "Lauvtreflis",

  varvals %in% c("44013001_1988", "44013002_1988", "44013008_2009", "44013901_2012",  "44013902_2012", "44013909_2012",
                       "44014011_2017", "44014019_2017", "44014021_2017", "44014029_2017",
                       "44014101_2022", "44014109_2022", "44014901_2022",   "44014909_2022" ) ~ "Ind. flis avfall bark"  ,
  varvals %in% c("44013009_1988", "44013003_2009", "44013100_2012", "44013108_2017", "44013101_2017", "44013903_2017", "44013908_2017", "44013201_2022", "44013209_2022", "44013904_2022", "44013907_2022") ~ "Pellets og briketter",
  varvals %in% c("44012101_1988") ~ "Celluloseflis",
  varvals %in% c("44012109_1988", "44012200_1988", "44012109_2022") ~ "Skogsflis",
  varvals %in% c("44011000_1988", "44011100_2017", "44011200_2017") ~ "Ved til brensel",
  TRUE ~ varvals
  )
)

# ## For checking
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$norsk, "mmer av gran")) %>% .[1:20,]
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$norsk, "mmer av furu")) %>% .[1:20,]
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$norsk, "mmer av bar")) %>% .[1:10,]
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$norsk, "Massevirke")) %>% .[1:14,]
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$grovsort, "mv")) %>% .[1:25,]
#  engnorsk %>% dplyr::select(.data$varvals, .data$norsk, .data$grovsort) %>% dplyr::filter(stringr::str_detect(.data$grovsort, "44")) %>% .[1:20,]

trevarehandel_12 <- PxWebApiData::ApiData12("http://data.ssb.no/api/v0/en/table/08801", #08801,
                                         Tid = TRUE,
                                         Land = FALSE,
                                         ContentsCode = TRUE,
                                         Varekoder = varvals)
  names(trevarehandel_12) <- make.names(names(trevarehandel_12))



  trevarehandel_12 <- trevarehandel_12 %>%
    dplyr::left_join(engnorsk, by = c("Varekoder" = "varvals")) %>%
    dplyr::mutate(
      statvar = dplyr::case_when(stringr::str_detect(contents, " \\(Q1\\)") ~ "kg",
                          stringr::str_detect(contents, " \\(Q2\\)") ~ "m3",
                          stringr::str_detect(contents, " \\(NOK\\)") ~ "NOK",
                          TRUE ~ contents)
    ) %>% dplyr::select( .data$Tid, .data$Varekoder, .data$ImpEks, .data$imports.exports, .data$value, .data$statvar, .data$eng, .data$norsk, .data$grovsort)


  unique(trevarehandel_12$statvar)
  trevarehandel_kr <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "NOK") %>% dplyr::rename(kr = .data$value) %>% dplyr::select(-.data$statvar) # head(trevarehandel_kr)
  trevarehandel_m3 <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "m3") %>% dplyr::rename(m3 = .data$value) %>% dplyr::select(-.data$statvar) # head(trevarehandel_m3)
  trevarehandel_kg <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "kg") %>% dplyr::rename(kg = .data$value) %>% dplyr::select(-.data$statvar,) # head(trevarehandel_kg)

  trevarehandel_2 <- trevarehandel_kr %>% dplyr::full_join(trevarehandel_m3) %>% dplyr::full_join(trevarehandel_kg)

  return(trevarehandel_2)

}


