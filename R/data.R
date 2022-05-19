#' @title Regioninndeling kommunevis siden 1994
#'
#' @description En tabell som viser endringer i kommunenes navn og nummer fra utgangspunkt
#'  januar 1994 fram til naa. Tabellen kan benyttes sammen med
#'  SSB regional statistikk (på kommunenivå) for aa koble observasjoner fra
#'  tidligere aar til dagens kommunestruktur.
#'
#'
#' @format En tibble med 436 rader (436 kommuner i '94) og 26 variabler:
#' \describe{
#'   \item{\code{reg_code_199401}}{Regionkode januar 1994; chr "0101" "0104"...}
#'   \item{reg_name_199401}{Regionnavn januar 1994; chr "Halden" "Moss" ...}
#'   \item{reg_code_200201}{Regionkode januar 2002; chr "0101" "0104"...}
#'   \item{reg_name_200201}{Regionnavn januar 2002; chr "Halden" "Moss" ...}
#'   \item{...code...}{fortsetter paa samme vis til 20xx}
#'   \item{...name...}{fortsetter paa samme vis til 20xx}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/131/versjoner}
"regref_kommune"



#' @title Regioninndeling fylkesvis siden 1994
#'
#' @description En tabell som viser endringer i kommunenes navn og nummer fra utgangspunkt
#'  januar 1994 fram til nå. Tabellen kan benyttes sammen med
#'  SSB regional statistikk (paa kommunenivaa) for aa koble observasjoner fra
#'  tidligere aar til dagens kommunestruktur.
#'  Fylkesinndelingen og endringer er hentet her https://www.ssb.no/klass/klassifikasjoner/104
#'
#' @format En tibble med 20 rader (20 fylker i '94) og 8 variabler:
#' \describe{
#'   \item{reg_code_199401}{Regionkode januar 1994; chr "01" "02"...}
#'   \item{reg_name_199401}{Regionnavn januar 1994; chr "Oestfold" "Akershus" ..}
#'   \item{reg_code_200607}{Regionkode juli 2006; chr "01" "02"...}
#'   \item{reg_name_200607}{Regionnavn juli 2006; chr "Oestfold" "Akershus" ...}
#'   \item{...code...}{fortsetter paa samme vis til 20xx}
#'   \item{...name...}{fortsetter paa samme vis til 20xx}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/127/versjoner}
"regref_fylke"


#' @title Hogstvolum fordelt paa kommune, sortiment, aar fra 1996 til 2019
#'
#' @description Data fra SSB at https://www.ssb.no/statbank/table/03895
#'
#' @format data.frame med om lag 172k obs og 11 variabler:
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{kategoritekst}{Sortimentnavn; chr "Gran spesialtoemmer" "Furu sekunda sagtoemmer" ...}
#'   \item{statistikkvariabel}{ ; chr "Kvantum rundvirke avvirket for salg" "Kvantum rundvirke avvirket for salg"...}
#'   \item{ar}{Aarstall; int "1996" "1997" ...}
#'   \item{volum_m3pris}{Volum (avregningsvolum, 1000 m3): int 12 36 85 125  }
#'   \item{region_kode}{Kommunenummer paa registreringstidspkt: chr "0" "0101" "0102"}
#'   \item{virkeskategori}{virkeskategori chr "1110" "1141" "1143" "1148" "1160" "1410" "1490" "2110" "2141" "2143" "2148" "2160" "2410" "2490" "3120" "3400" "3800" "1800"}
#'   \item{treslag}{chr  "Gran"   "Furu"   "Lauv"   "Ukjent"}
#'   \item{sortimentgruppe}{sortimentgruppe chr "toemmer" "sams" "massevirke" "annet"}
#'   \item{reg_n20xx}{Regionnavn pr 20xx chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k20xx}{Regionkode pr 20xx chr "0301" "1101" "1103" "1106" "1108" "1111" "1112" "1114"}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03895}
"m3_sortiment_kmn"


#' @title Brutto virkesverdi fordelt på fylke og år fra 1996
#'
#' @description Data fra SSB, tabell 03794: Bruttoverdi. Avvirkning for salg (1 000 kr) (K) 1996 - dd
#'
#' @format data.frame:
#' \describe{
#'   \item{\code{region}}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{statistikkvariabel}{ ; chr "Bruttoverdi" "Bruttoverdi"}
#'   \item{år}{Aarstall; chr "1996" "1997" ...}
#'   \item{bruttoverdi}{Brutto verdi av solgt industrivirke NOK: int 2709220 2877766 2608427 ...  }
#'   \item{region_kode}{Regionkode <chr> "0101", "0101", }
#'   \item{Tid}{samme som årstall, <chr> "1996", "1997", "1998",}
#'   \item{aar}{Aarstall; int "1996" "1997" ...}
#'   \item{\code{reg_n20xx}}{Regionnavn (kommunenavn) pr 20xx chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k20xx}{Regionkode (kommunekode) pr 20xx}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03794}
"virkesverdi_flk"


#' @title Brutto virkesverdi fordelt på kommune og år fra 1996
#'
#' @description Data fra SSB, tabell 03794: Bruttoverdi. Avvirkning for salg (1 000 kr) (K) 1996 - dd
#'
#' @format dataframe med om lag 9542 obs og 7 variabler
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{statistikkvariabel}{ ; chr "Bruttoverdi" "Bruttoverdi"}
#'   \item{år}{Aarstall; chr "1996" "1997" ...}
#'   \item{bruttoverdi}{Brutto verdi av solgt industrivirke NOK: int 2709220 2877766 2608427 ...  }
#'   \item{region_kode}{Regionkode <chr> "0101", "0101", }
#'   \item{Tid}{samme som årstall, <chr> "1996", "1997", "1998",}
#'   \item{aar}{Aarstall; int "1996" "1997" ...}
#'   \item{reg_n20xx}{Regionnavn (kommunenavn) pr 20xx chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k20xx}{Regionkode (kommunekode) pr 20xx}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03794}
"virkesverdi_kmn"


#' @title Sortimentpriser fordelt på fylke og år fra 2006
#'
#' @description Data fra SSB, tabell 12750:
#'     Gjennomsnittspris, etter sortiment (kr per m³) (F) 2006 - 2019
#'
#' @format data.frame med om lag 9542 obs og 7 variabler
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{kategoritekst}{Sortimentnavn; chr "Gran spesialtoemmer" "Furu sekunda sagtoemmer" ...}
#'   \item{statistikkvariabel}{ ; chr "Gjennomsnittspris" "Gjennomsnittspris"}
#'   \item{år}{årstall ; chr "2006", "2007"}
#'   \item{pris}{nok; int NA, 637, 465, ...}
#'   \item{region_kode}{Fylkesnummer paa registreringstidspkt: chr "0" "01" "02"}
#'   \item{treslag}{chr  "Gran"   "Furu"   "Lauv"   "Ukjent"}
#'   \item{virkeskategori}{virkeskategori chr "1110" "1141" "1143" "1148" "1160" "1410" "1490" "2110" "2141" "2143" "2148" "2160" "2410" "2490" "3120" "3400" "3800" "1800"}
#'   \item{sortimentgruppe}{sortimentgruppe chr "tømmer" "sams" "massevirke" "annet"}
#'   \item{aar}{Aarstall; int "2006" "2007" ...}
#'   \item{reg_n20xx}{Regionnavn (fylkesnavn) pr år 20xx chr "Hele landet" "Viken" "Trøndelag"}
#'   \item{reg_k20xx}{Regionkode (fylkeskode) pr år 20xx}
#' }
#' @source \url{https://www.ssb.no/statbank/table/12750}
"sortimentpriser_flk"





#' @title volum og priser per kommune, treslag, sortiment, år fra 2014
#'
#' @description Data fra Landbruksdirektoratets hogststatistikk
#'
#' @format tibble med om lag 1300 obs og 10 variabler
#' \describe{
#'   \item{reg_n202x}{Regionnavn pr dagens årstall chr "Agder", "Agder" }
#'   \item{reg_k202x}{Regionkode pr dagens årstall chr "42", "42" }
#'   \item{aar}{årstall; num  2014 2014 2014 ...}
#'   \item{sortkode}{Sortimentkode : chr "01" "01" "01" ...}
#'   \item{sortiment}{Sortimentnavn: chr "Sagtømmer" "Sagtømmer" ... }
#'   \item{virkesgrp}{Virkesgruppe; chr "1-Gran" "1-Gran" "2-Furu" "3-Lauv" "5-Ved" ... }
#'   \item{virkeskat}{Virkeskat; chr "1140" "1148" "1410" ... }
#'   \item{kategoritekst}{Kategoritekst; chr "Gran sagtømmer sams" "Gran annet sagtømmer" "Gran massevirke" ... }
#'   \item{totalvolum}{m3; num 3440 14215 ...}
#'   \item{totalverdi}{kr; num 1599470 6898324 703000 ...}
#'   \item{m3pris}{kr pr m3; num 465 485 263 398 3 ...}
#'   \item{region_kode}{kode; chr 09, 10 09, 10 09, 10 09,  ...}
#' }
"avvirk_fylke_ldir"





#' @title volum og priser per kommune, treslag, sortiment, år fra 2014
#'
#' @description Data fra Landbruksdirektoratets hogststatistikk
#'
#' @format tibble med om lag 20k obs og 13 variabler
#' \describe{
#'   \item{reg_n202x}{Regionnavn pr dagens årstall chr "Halden", "Halden" }
#'   \item{reg_k202x}{Regionkode pr dagens årstall chr "Halden", "Halden" }
#'   \item{aar}{årstall; num  2014 2014 2014 ...}
#'   \item{sortkode}{Sortimentkode : chr "01" "01" "01" ...}
#'   \item{sortiment}{Sortimentnavn: chr "Sagtømmer" "Sagtømmer" ... }
#'   \item{virkesgrp}{Virkesgruppe; chr "1-Gran" "1-Gran" "2-Furu" "3-Lauv" "5-Ved" ... }
#'   \item{virkeskat}{Virkeskat; chr "1140" "1148" "1410" ... }
#'   \item{kategoritekst}{Kategoritekst; chr "Gran sagtømmer sams" "Gran annet sagtømmer" "Gran massevirke" ... }
#'   \item{totalvolum}{m3; num 3440 14215 ...}
#'   \item{totalverdi}{kr; num 1599470 6898324 703000 ...}
#'   \item{m3pris}{kr pr m3; num 465 485 263 398 3 ...}
#'   \item{region_kode}{kode; chr 1820, 1820, 1820, 1820,  ...}
#' }
"avvirk_kmn_ldir"




#' @title Konsumprisindeks
#'
#' @description Data fra ssb tabell 03014:
#' Konsumprisindeks, etter konsumgruppe (2015=100) 1979 - 2019
#' For konsumgrupp totalindeks
#'
#' @format tibble med om lag 41 obs og 2 variabler
#' \describe{
#'   \item{år}{årstall; chr "1979" "1980" ...}
#'   \item{kpi}{kpi ; num 25.9, 28.7,  ...}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03014}
"kpi_t03014"



#' @title Avvirkning pr sortiment pr fylke
#'
#' @description m3 per sortiment pr flk per år,  basert på ssb tabell 03895
#' Data fra SSB at https://www.ssb.no/statbank/table/03895
#'
#' @format data.frame med om lag 8244 obs og 13 variabler:
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{kategoritekst}{Sortimentnavn; chr "Gran spesialtoemmer" "Furu sekunda sagtoemmer" ...}
#'   \item{statistikkvariabel}{ ; chr "Kvantum rundvirke avvirket for salg" "Kvantum rundvirke avvirket for salg"...}
#'   \item{år}{Aarstall; int "1996" "1997" ...}
#'   \item{volum_m3pris}{Volum (avregningsvolum, 1000 m3): int 12 36 85 125  }
#'   \item{region_kode}{Kommunenummer paa registreringstidspkt: chr "0" "0101" "0102"}
#'   \item{virkeskategori}{virkeskategori chr "1110" "1141" "1143" "1148" "1160" "1410" "1490" "2110" "2141" "2143" "2148" "2160" "2410" "2490" "3120" "3400" "3800" "1800"}
#'   \item{Tid}{chr "1996", "1997", "1998", ...}
#'   \item{treslag}{chr  "Gran"   "Furu"   "Lauv"   "Ukjent"}#'
#'   \item{sortimentgruppe}{sortimentgruppe chr "toemmer" "sams" "massevirke" "annet"}
#'   \item{aar}{Aarstall; int 1996 1997 ...}
#'   \item{reg_n20xx}{Regionnavn pr 20xx chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k20xx}{Regionkode pr 20xx chr "0301" "1101" "1103" "1106" "1108" "1111" "1112" "1114"}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03895}
"m3_sortiment_flk"


#' @title Region referansetabell for fylker
#'
#' @description Tabellen gjør det mulig å slå opp hva et fylkesnavn på et gitt tidspunkt
#' tilsvarer i dagens fylkesstruktur.
#'
#' @format data frame med om lag 80 obs og 8 variabler:
#' \describe{
#'   \item{ymfrom}{År og måned reg_code_from og reg_name_from gjelder fra; chr  "197201" "197201" "197201"  ...}
#'   \item{yfrom}{årstallet fra ymfrom; chr "1972" "1972" "1972"  ...}
#'   \item{reg_code_from}{Kode på regionenhet fra dato "ymfrom" ; chr "01" "02" "03" ...}
#'   \item{reg_name_from}{Navn på regionenhet fra dato "ymfrom"; "Østfold" "Akershus" "Oslo" ...}
#'   \item{ymto}{År og måned reg_code_to og reg_name_to gjelder fra: chr "202001" "202001" "202001" }
#'   \item{yto}{År reg_code_to og reg_name_to gjelder fra: chr "2020" "2020" "2020" }
#'   \item{reg_code_to}{Kode på regionenhet fra dato ymto:chr "30" "30" "03" ...}
#'   \item{reg_name_to}{Navn på regionenhet fra dato ymto: chr  "Viken" "Viken" "Oslo"  ...}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/127/versjoner}
"regref_fylke_l"


#' @title Region referansetabell for kommuner
#'
#' @description Tabellen gjør det mulig å slå opp hva et kommunenavn / kommunekode på
#' et gitt tidspunkt tilsvarer i dagens kommunestruktur.
#'
#'
#' @format data frame med om lag 80 obs og 8 variabler:
#' \describe{
#'   \item{ymfrom}{År og måned reg_code_from og reg_name_from gjelder fra; chr  "197201" "197201" "197201"  ...}
#'   \item{yfrom}{årstallet fra ymfrom; chr "1972" "1972" "1972"  ...}
#'   \item{reg_code_from}{Kode på regionenhet fra dato "ymfrom" ; chr "01" "02" "03" ...}
#'   \item{reg_name_from}{Navn på regionenhet fra dato "ymfrom"; "Østfold" "Akershus" "Oslo" ...}
#'   \item{ymto}{År og måned reg_code_to og reg_name_to gjelder fra: chr "202001" "202001" "202001" }
#'   \item{yto}{År reg_code_to og reg_name_to gjelder fra: chr "2020" "2020" "2020" }
#'   \item{reg_code_to}{Kode på regionenhet fra dato ymto:chr "30" "30" "03" ...}
#'   \item{reg_name_to}{Navn på regionenhet fra dato ymto: chr  "Viken" "Viken" "Oslo"  ...}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/131/versjoner}
"regref_kommune_l"
