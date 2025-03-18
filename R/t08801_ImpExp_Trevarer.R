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
 #'  dt <- t08801tre()
 #'  str(dt)
 #'  dt %>% mutate(kr_pr_m3 = ifelse(!is.na(m3) & m3>0, kr / m3, NA_real_)) %>%
 #'   filter(!(stringr::str_detect(.data$grovsort, "440") )) %>%
 #'    dplyr::filter(.data$kr_pr_m3 > 2000) %>%
 #'   arrange(desc(kr_pr_m3)) %>% select(Tid, Varekoder, ImpEks, grovsort, assortment, m3, kg, kr, kr_pr_m3, norsk) %>% str()
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

  engnorsk <- dplyr::left_join(descr, beskr, by = c("varvals")) %>% dplyr::rename(norsk = "vartext.y", eng = "vartext.x")

  engnorsk <- engnorsk %>%
    dplyr::mutate(
        grovsort = dplyr::case_when(
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
      )) %>%
    dplyr::mutate(
      assortment = dplyr::case_when(
        .data$grovsort == "Furu mv" ~ "Pine plp",
        .data$grovsort == "Gran mv" ~ "Spruce plp",
        .data$grovsort == "Lauv mv" ~ "Broadleave plp",
        .data$grovsort == "Furu sagt" ~ "Pine sawl",
        .data$grovsort == "Gran sagt" ~ "Spruce sawl",
        .data$grovsort == "Lauv sagt" ~ "Broadleave sawl",
        .data$grovsort == "Bar sagt" ~ "Conifer sawl",
        .data$grovsort == "Ind. flis avfall bark" ~ "Industrial wood residues",
        .data$grovsort == "Celluloseflis" ~ "Pulp chips",
        .data$grovsort == "Skogsflis" ~ "Forest chips",
        .data$grovsort == "Ved til brensel" ~ "Firewood",
        TRUE ~ .data$grovsort)
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
      ) %>% dplyr::select( "Tid", "Varekoder", "ImpEks", "imports.exports", "value", "statvar", "eng", "norsk", "grovsort", "assortment")


    unique(trevarehandel_12$statvar)
    trevarehandel_kr <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "NOK") %>% dplyr::rename(kr = .data$value) %>% dplyr::select(-.data$statvar) # head(trevarehandel_kr)
    trevarehandel_m3 <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "m3") %>% dplyr::rename(m3 = .data$value) %>% dplyr::select(-.data$statvar) # head(trevarehandel_m3)
    trevarehandel_kg <- trevarehandel_12 %>% dplyr::filter(.data$statvar == "kg") %>% dplyr::rename(kg = .data$value) %>% dplyr::select(-.data$statvar,) # head(trevarehandel_kg)

    trevarehandel_2 <- trevarehandel_kr %>% dplyr::full_join(trevarehandel_m3) %>% dplyr::full_join(trevarehandel_kg)

    return(trevarehandel_2)

}



#' Plot roundwood export volume and prices
#'
#' @return a list of four plots
#' @export
#'
#' @examples
#' plot_rw_export()[[1]]
plot_rw_export <- function(){
  # table(plotdat$)
  sumvars <- c("kr", "m3", "kg")
  plotdat <- vsop::t08801tre() %>% dplyr::select(-"Varekoder") %>%
    dplyr::filter(.data$imports.exports == "Exports", !(stringr::str_detect(.data$grovsort, "440"))) %>%
    dplyr::mutate(year = as.numeric(.data$Tid))
  #head(plotdat)
  # table(plotdat$grovsort, plotdat$assortment)
  plotdat2 <- plotdat %>%
    dplyr::filter(stringr::str_detect(.data$assortment, "sawl|plp")) %>%
    dplyr::mutate(
      assortment = dplyr::case_when(
        .data$assortment == "Pine sawl" ~ "Conifer sawl",
        .data$assortment == "Spruce sawl" ~ "Conifer sawl",
        TRUE ~ .data$assortment),
      grovsort = dplyr::case_when(
        .data$grovsort == "Furu sagt" ~ "Bar sagt",
        .data$grovsort == "Gran sagt" ~ "Bar sagt",
        TRUE ~ .data$grovsort
      ),
      treslagskategori = dplyr::case_when(
        stringr::str_detect(.data$grovsort, "Bar|Gran|Furu") ~ "Bar",
        stringr::str_detect(.data$grovsort, "Lauv|Bjork") ~ "Lauv",
        TRUE ~ .data$grovsort
      ),
      speciescategory = dplyr::case_when(
        stringr::str_detect(.data$grovsort, "Bar|Gran|Furu") ~ "Conifers",
        stringr::str_detect(.data$grovsort, "Lauv|Bjork") ~ "Broadleaves",
        TRUE ~ .data$grovsort
      ),
      produktkategori = dplyr::case_when(
        stringr::str_detect(.data$grovsort, "mv") ~ "mv",
        stringr::str_detect(.data$grovsort, "sagt") ~ "sagt",
        TRUE ~ .data$grovsort
      ),
      productcategory = dplyr::case_when(
        stringr::str_detect(.data$grovsort, "mv") ~ "pulp",
        stringr::str_detect(.data$grovsort, "sagt") ~ "sawlogs",
        TRUE ~ .data$grovsort
      ),
      grp = paste0(.data$productcategory, " ", .data$speciescategory)) %>%
    dplyr::group_by(.data$Tid, .data$produktkategori,.data$productcategory, .data$treslagskategori,.data$speciescategory, .data$grp) %>%
    dplyr::summarise(dplyr::across( dplyr::all_of(sumvars), sum)) %>%
    dplyr::mutate(year = as.numeric(.data$Tid),
                  pris_kr_m3 = .data$kr / .data$m3) %>%
    dplyr::filter(.data$pris_kr_m3 < 1000, .data$m3 > 5000) %>%
    dplyr::group_by(.data$produktkategori, .data$treslagskategori, .data$grp) %>%
    dplyr::mutate(ndat = length(unique(.data$Tid)) ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$ndat > 15)

  # head(plotdat2 %>% arrange(ndat))

  p_eksp_m3_en <-  ggplot2::ggplot(plotdat2,
    ggplot2::aes( x = .data$year, y = .data$m3/1000)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$grp, color = .data$productcategory)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$speciescategory, color = .data$productcategory)) +
    ggpubr::theme_pubr()   +
    ggplot2::labs(x = "year", y = expression("1000 "~ m^3), color = "productcategory", shape = "speciescategory")
  # p_eksp_m3_en

  p_eksp_m3_no <-  ggplot2::ggplot(plotdat2 ,
                                   ggplot2::aes( x = .data$year, y = .data$m3/1000)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$grp, color = .data$produktkategori)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$treslagskategori, color = .data$produktkategori)) +
    ggpubr::theme_pubr()   +
    ggplot2::labs(x = "år", y = expression("1000 "~ m^3), color = "Produktkategori", shape = "Treslagskategori")
  #p_eksp_m3_no

  p_eksp_pris_en <-
    ggplot2::ggplot(plotdat2 %>% dplyr::filter(.data$pris_kr_m3 < 1000),
                    ggplot2::aes( x = .data$year, y = .data$pris_kr_m3)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$grp, color = .data$productcategory)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$speciescategory, color = .data$productcategory)) +
    ggpubr::theme_pubr()   +
    ggplot2::labs(x = "year", y = expression("Price, NOK " ~ m^3^-1), color = "productcategory", shape = "speciescategory")
   # p_eksp_pris_en

  p_eksp_pris_no <-
    ggplot2::ggplot(plotdat2 %>% dplyr::filter(.data$pris_kr_m3 < 1000),
                    ggplot2::aes( x = .data$year, y = .data$pris_kr_m3)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$grp, color = .data$produktkategori)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$treslagskategori, color = .data$produktkategori)) +
    ggpubr::theme_pubr()   +
    ggplot2::labs(x = "year", y = expression("pris, kr per " ~ m^3), color = "Produktkategori", shape = "Treslagskategori")
  # p_eksp_pris_no

retlist <- list(p_eksp_m3_en = p_eksp_m3_en,
                p_eksp_m3_no = p_eksp_m3_no,
                p_eksp_pris_en = p_eksp_pris_en,
                p_eksp_pris_no = p_eksp_pris_no
                )
return(retlist)

}


