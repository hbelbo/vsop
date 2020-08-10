# vsop - rpkg for organizing roundwood logging data from vsop and derived sources  

The Norwegian "Landbruksdirektoratet"" is providing statistics for annual industrial roundwood sales and corresponding volumes at municipality level (in Norway) in excel sheets, one excel document for each year. 
https://www.landbruksdirektoratet.no/no/statistikk/skogbruk/tommeravvirkning

SSB provide similar statistics, but some of it only at county level, annual resolution and 
a bit more lagged publication. But longer history. 
https://www.ssb.no/statbank/list/skogav


This package should ease the collection of these data into R. 


Load dependent packages: 
```r
invisible(
  lapply( c("magrittr","stringr","dplyr","tibble","lubridate","readxl","PxWebApiData"),
    library, character.only = T))
```

Install package in R: 
```r
devtools::install_git('https://gitlab.nibio.no/hbel/vsop.git')
```
Demo of readymade datasets within the package:
```r
glimpse(m3_sortiment_kmn) # annual total volume by assortment and minicipality. 
glimpse(virkesverdi_kmn) # annual roundwood total value by municipality

```

Demo of some functions: 
```r
vsop::regnavn.at.ref.yr(regionstat = vsop::t12750(), ref.yr = 2020 ) -> fylke_priser
vsop::regnavn.at.ref.yr(regionstat = vsop::t03895(geolevel =  "fylke"), ref.yr = 2020 ) -> fylke_volum
```

```r
vsop::regnavn.at.ref.yr(
  regionstat = vsop::t03895(geolevel = "kommune"), 
  reg_level = "kommune", 
  ref.yr = 2020 
  ) -> kommune_volum
  
vsop::regnavn.at.ref.yr(
  regionstat = vsop::t03794( geolevel = "kommune"), 
  reg_level = "kommune", 
  ref.yr = 2020 
  ) -> kommune_verdi
```
```r
vsop::ssb_skog_omsetning()
vsop::t03895()
vsop::t06216()
vsop::t12750()

```
