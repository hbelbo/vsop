#' Regioninndeling kommunevis siden 1994
#'
#' En tabell som viser endringer i kommunenes navn og nummer fra utgangspunkt
#'  januar 1994 fram til naa. Tabellen kan benyttes sammen med
#'  SSB regional statistikk (paa kommunenivaa) for aa koble observasjoner fra
#'  tidligere aar til dagens kommunestruktur.
#'  SSB info: https://www.ssb.no/klass/klassifikasjoner/131
#'
#'
#' @format En tibble med 436 rader (436 kommuner i '94) og 26 variabler:
#' \describe{
#'   \item{reg_code_199401}{Regionkode januar 1994; chr "0101" "0104"...}
#'   \item{reg_name_199401}{Regionnavn januar 1994; chr "Halden" "Moss" ...}
#'   \item{reg_code_200201}{Regionkode januar 2002; chr "0101" "0104"...}
#'   \item{reg_name_200201}{Regionnavn januar 2002; chr "Halden" "Moss" ...}
#'   \item{...code...}{fortsetter paa samme vis til 2020}
#'   \item{...name...}{fortsetter paa samme vis til 2020}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/131/versjoner}
"regref_kommune"



#' Regioninndeling fylkesvis siden 1994
#'
#' En tabell som viser endringer i kommunenes navn og nummer fra utgangspunkt
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
#'   \item{...code...}{fortsetter paa samme vis til 2020}
#'   \item{...name...}{fortsetter paa samme vis til 2020}
#' }
#' @source \url{https://www.ssb.no/klass/klassifikasjoner/127/versjoner}
"regref_fylke"


#' Hogstvolum fordelt paa kommune, sortiment, aar fra 1996 til 2019
#'
#' Data fra SSB at https://www.ssb.no/statbank/table/03895
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
#'   \item{reg_n2020}{Regionnavn pr 2020 chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k2020}{Regionkode pr 2020 chr "0301" "1101" "1103" "1106" "1108" "1111" "1112" "1114"}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03895}
"m3_sortiment_kmn"

#' Brutto virkesverdi fordelt på fylke og år fra 1996
#'
#' Data fra SSB, tabell 03794: Bruttoverdi. Avvirkning for salg (1 000 kr) (K) 1996 - dd
#'
#' @format data.frame med om lag 9542 obs og 7 variabler:
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{statistikkvariabel}{ ; chr "Bruttoverdi" "Bruttoverdi"}
#'   \item{år}{Aarstall; chr "1996" "1997" ...}
#'   \item{bruttoverdi}{Brutto verdi av solgt industrivirke NOK: int 2709220 2877766 2608427 ...  }
#'   \item{region_kode}{Regionkode <chr> "0101", "0101", }
#'   \item{Tid}{samme som årstall, <chr> "1996", "1997", "1998",}
#'   \item{aar}{Aarstall; int "1996" "1997" ...}
#'   \item{reg_n2020}{Regionnavn (kommunenavn) pr 2020 chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k2020}{Regionkode (kommunekode) pr 2020}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03794}
"virkesverdi_flk"

#' Brutto virkesverdi fordelt på kommune og år fra 1996
#'
#' Data fra SSB, tabell 03794: Bruttoverdi. Avvirkning for salg (1 000 kr) (K) 1996 - dd
#'
#' @format data.frame med om lag 9542 obs og 7 variabler:
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{statistikkvariabel}{ ; chr "Bruttoverdi" "Bruttoverdi"}
#'   \item{år}{Aarstall; chr "1996" "1997" ...}
#'   \item{bruttoverdi}{Brutto verdi av solgt industrivirke NOK: int 2709220 2877766 2608427 ...  }
#'   \item{region_kode}{Regionkode <chr> "0101", "0101", }
#'   \item{Tid}{samme som årstall, <chr> "1996", "1997", "1998",}
#'   \item{aar}{Aarstall; int "1996" "1997" ...}
#'   \item{reg_n2020}{Regionnavn (kommunenavn) pr 2020 chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k2020}{Regionkode (kommunekode) pr 2020}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03794}
"virkesverdi_kmn"


#' Sortimentpriser fordelt på kommune og år fra 1996
#'
#' Data fra SSB, tabell 12750: Bruttoverdi. A (K) 2006 - dd
#'
#' @format data.frame med om lag 9542 obs og 7 variabler:
#' \describe{
#'   \item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
#'   \item{kategoritekst}{Sortimentnavn; chr "Gran spesialtoemmer" "Furu sekunda sagtoemmer" ...}
#'   \item{statistikkvariabel}{ ; chr "Gjennomsnittspris" "Gjennomsnittspris"}
#'   \item{år}{årstall ; chr "2006", "2007"}
#'   \item{pris}{nok; int NA, 637, 465, ...}
#'   \item{region_kode}{Kommunenummer paa registreringstidspkt: chr "0" "0101" "0102"}
#'   \item{treslag}{chr  "Gran"   "Furu"   "Lauv"   "Ukjent"}
#'   \item{virkeskategori}{virkeskategori chr "1110" "1141" "1143" "1148" "1160" "1410" "1490" "2110" "2141" "2143" "2148" "2160" "2410" "2490" "3120" "3400" "3800" "1800"}
#'   \item{sortimentgruppe}{sortimentgruppe chr "tømmer" "sams" "massevirke" "annet"}
#'   \item{aar}{Aarstall; int "1996" "1997" ...}
#'   \item{reg_n2020}{Regionnavn (kommunenavn) pr 2020 chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k2020}{Regionkode (kommunekode) pr 2020}
#' }
#' @source \url{https://www.ssb.no/statbank/table/12750}
"sortimentpriser_kmn"



#' Hogststatistikk per fylke, sortiment, mnd, ... fra 2014
#'
#' Data fra Landbruksdirektoratets hogststatistikk
#'
#' @format tibble med om lag 19k obs og 13 variabler
#' \describe{
#'   \item{FYLKENR }{Nr paa fylke (paa hogsttidspunkt); chr "01" "01" ...}
#'   \item{FYLKENAVN}{; "Østfold" "Østfold" ...}
#'   \item{AVVIRKAAR}{årstall; num  2014 2014 2014 ...}
#'   \item{KVARTAL}{kvartal; num 1 1 1 ...}
#'   \item{AVVIRKMND}{Aarstall; num 1 1 1 ...}
#'   \item{SORTKODE}{Sortimentkode : chr "01" "01" "01" ...}
#'   \item{SORTIMENT}{Sortimentnavn: chr "Sagtømmer" "Sagtømmer" ... }
#'   \item{VIRKESGRP}{Virkesgruppe; chr "1-Gran" "1-Gran" "2-Furu" "3-Lauv" "5-Ved" ... }
#'   \item{VIRKESKAT}{Virkeskategori; chr "1110" "1140" "1148" ...}
#'   \item{KATEGORITEKST}{sortimentgruppe chr "Gran spesial" "Gran sagtømmer sams" }
#'   \item{TOTALVOLUM}{m3; num 3440 14215 ...}
#'   \item{TOTALVERDI}{kr; num 1599470 6898324 703000 ...}
#'   \item{M3PRIS}{kr pr m3; num 465 485 263 398 3 ...}
#' }
"hogst_fylke_ld"


#' Hogststatistikk per kommune, sortiment, år, ... fra 2014
#'
#' Data fra Landbruksdirektoratets hogststatistikk
#'
#' @format tibble med om lag 20k obs og 13 variabler
#' \describe{
#'   \item{FYLKENR }{Nr paa fylke (paa hogsttidspunkt); chr "01" "01" ...}
#'   \item{FYLKENAVN}{; "Østfold" "Østfold" ...}
#'   \item{KOMNR}{Nr paa kommune (på hogsttidspunkt); chr "0101" "0101" ...}
#'   \item{KOMNAVN}{; "HALDEN" "HALDEN" ...}
#'   \item{AVVIRKAAR}{årstall; num  2014 2014 2014 ...}
#'   \item{SORTKODE}{Sortimentkode : chr "01" "01" "01" ...}
#'   \item{SORTIMENT}{Sortimentnavn: chr "Sagtømmer" "Sagtømmer" ... }
#'   \item{VIRKESGRP}{Virkesgruppe; chr "1-Gran" "1-Gran" "2-Furu" "3-Lauv" "5-Ved" ... }
#'   \item{VIRKESKAT}{Virkeskategori; chr "1110" "1140" "1148" ...}
#'   \item{KATEGORITEKST}{sortimentgruppe chr "Gran spesial" "Gran sagtømmer sams" }
#'   \item{TOTALVOLUM}{m3; num 3440 14215 ...}
#'   \item{TOTALVERDI}{kr; num 1599470 6898324 703000 ...}
#'   \item{M3PRIS}{kr pr m3; num 465 485 263 398 3 ...}
#' }
"hogst_kommune_ld"


#' Konsumprisindeks
#'
#' Data fra ssb tabell 03014:
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



#' Avvirkning pr sortiment pr fylke
#'
#' m3 per sortiment pr flk per år,  basert på ssb tabell 03895
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
#'   \item{reg_n2020}{Regionnavn pr 2020 chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
#'   \item{reg_k2020}{Regionkode pr 2020 chr "0301" "1101" "1103" "1106" "1108" "1111" "1112" "1114"}
#' }
#' @source \url{https://www.ssb.no/statbank/table/03895}
"m3_sortiment_flk"


#' Region referansetabell for fylker
#'
#' Tabellen gjør det mulig å slå opp hva et fylkesnavn på et gitt tidspunkt
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


#' Region referansetabell for kommuner
#'
#' Tabellen gjør det mulig å slå opp hva et kommunenavn / kommunekode på
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