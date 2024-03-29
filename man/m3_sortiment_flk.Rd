% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{m3_sortiment_flk}
\alias{m3_sortiment_flk}
\title{Avvirkning pr sortiment pr fylke}
\format{
data.frame med om lag 8244 obs og 13 variabler:
\describe{
\item{region}{Navn paa kommunen (paa hogsttidspunkt); chr "Hele landet" "Halden" ...}
\item{kategoritekst}{Sortimentnavn; chr "Gran spesialtoemmer" "Furu sekunda sagtoemmer" ...}
\item{statistikkvariabel}{ ; chr "Kvantum rundvirke avvirket for salg" "Kvantum rundvirke avvirket for salg"...}
\item{år}{Aarstall; int "1996" "1997" ...}
\item{volum_m3pris}{Volum (avregningsvolum, 1000 m3): int 12 36 85 125  }
\item{region_kode}{Kommunenummer paa registreringstidspkt: chr "0" "0101" "0102"}
\item{virkeskategori}{virkeskategori chr "1110" "1141" "1143" "1148" "1160" "1410" "1490" "2110" "2141" "2143" "2148" "2160" "2410" "2490" "3120" "3400" "3800" "1800"}
\item{Tid}{chr "1996", "1997", "1998", ...}
\item{treslag}{chr  "Gran"   "Furu"   "Lauv"   "Ukjent"}#'
\item{sortimentgruppe}{sortimentgruppe chr "toemmer" "sams" "massevirke" "annet"}
\item{aar}{Aarstall; int 1996 1997 ...}
\item{reg_n20xx}{Regionnavn pr 20xx chr "Hele landet" "Halden" "Moss" "Sarpsborg" "Fredrikstad"}
\item{reg_k20xx}{Regionkode pr 20xx chr "0301" "1101" "1103" "1106" "1108" "1111" "1112" "1114"}
}
}
\source{
\url{https://www.ssb.no/statbank/table/03895}
}
\usage{
m3_sortiment_flk
}
\description{
m3 per sortiment pr flk per år,  basert på ssb tabell 03895
Data fra SSB at https://www.ssb.no/statbank/table/03895
}
\keyword{datasets}
