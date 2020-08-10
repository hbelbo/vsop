# vsop - an r pakgage for organizing roundwood logging data from vsop and derived sources  

The Norwegian "Landbruksdirektoratet"" is providing statistics for annual industrial roundwood sales and corresponding volumes at municipality level (in Norway) in excel sheets, one excel document for each year. 
https://www.landbruksdirektoratet.no/no/statistikk/skogbruk/tommeravvirkning

SSB provide similar statistics, but some of it only at county level, annual resolution and 
a bit more lagged publication. But longer history. 
https://www.ssb.no/statbank/list/skogav


This package should ease the collection of these data into R. 


Load dependent packages: 


Install package in R: 
```r
devtools::install_git('https://github.com/hbelbo/vsop.git')
```
Demo av noen ferdige datasett i r-pakken:
```r
dplyr::glimpse(m3_sortiment_kmn) # annual total volume by assortment and minicipality. 
dplyr::glimpse(virkesverdi_kmn) # annual roundwood total value by municipality

```

Demo of functions fetching logging statistics from statistics Norway (SSB)
```r
vsop::t03794(region_level = "fylke") %>% dplyr::glimpse # bruttoverdi per aar av toemmer
vsop::t03895(region_level = "fylke") %>% dplyr::glimpse() # avvirkningsvolum for salg, per sortiment, kommune eller fylke, år.
vsop::t06216() %>% dplyr::glimpse() # snittpris per sortiment på fylkesnivå, fra 1996 til 2017
vsop::t12750() %>% dplyr::glimpse()  # snittpris per sortiment per fylke

```
Demo av funksjoner som organiserer regional historisk statistikk til dagens kommuneinndeling og fylkesinndeling
```r
# Volum per fylke / kommune, år, treslag, sortiment. Med regioninndeling per 2020. 
vsop::regnavn.at.ref.yr(
  regionstat = vsop::t03895(region_level = "fylke")) %>% dplyr::glimpse()

# Bruttoverdi av alt virke per år pr kommune, med kommunenavn pr 2020  
vsop::regnavn.at.ref.yr(
  regionstat = vsop::t03794(region_level = "kommune"))%>% glimpse() 
```
Demo of some functions: 
```r
vsop::regnavn.at.ref.yr(regionstat = vsop::t12750()) -> fylke_priser
vsop::regnavn.at.ref.yr(regionstat = vsop::t03895(geolevel =  "fylke")) -> fylke_volum
```
