


#' Biomass expansion factors according to Lethonen etal 2004
#'
#' @param species one of: "spruce", "gran", "pine", "furu", "birch", "broadleave", "løv", "lauv", "bjørk"
#' @param age age of the stand (numeric, years from breast height)
#'
#' @returns a data.frame with biomass expansion factors for estimating dry mass of
#' tree parts like stem, branches etc, in tonne per m3 stem volume
#' (likely total stump to top stem volume) (også kalt skogskubikmeter)
#' @export
#'
#' @examples
#' BEF_Let2004_spc_age(species = "pine", age = 70)
BEF_Let2004_spc_age <- function(species , age ){
  # based on Biomass expansion factors (BEFs) for Scots pine, Norway spruce
  # and birch according to stand age for boreal forests
  # Forest Ecology and Management 188 (2004) 211–224
  # https://doi.org/10.1016/j.foreco.2003.07.008

  stopifnot( tolower(species) %in% c("gran", "spruce", "furu", "pine", "birch", "bjørk", "broadleave", "løv", "lauv", "deciduous"))
  stopifnot(is.numeric(age) & age > 1 & age < 250)
  agef = exp(-age/100)
  if(tolower(species) %in% c("spruce", "gran")) {

    Stembm = 0.4 + -0.0462 * agef
    Foliagebm = 0.0388 + 0.0849 * agef
    Branchesbm = 0.0905 + 0.0719 * agef
    Branchesdbm = 0.0088 + 0.0001 * agef
    Barkbm = 0.0353 + 0.0125 * agef
    Stumpbm = 0.0488 + 0.0002 * agef
    RootsCbm = 0.1024 + -0.0271 * agef
    RootsSbm = 0.0201 + 0.0448 * agef
    Totalbm = 0.7406 + 0.006 * agef
    Total_abvgbm = 0.5734 + 0.1272 * agef

  } else if(tolower(species) %in% c("pine", "furu") ){
    Stembm = 0.4194 + -0.0798 * agef
    Foliagebm = 0.0177 + 0.0499 * agef
    Branchesbm = 0.0706 + 0.0212 * agef
    Branchesdbm = 0.0104 + 0.0059 * agef
    Barkbm = 0.0254 + 0.0221 * agef
    Stumpbm = 0.0472 + - 0.0039 * agef
    RootsCbm = 0.0838 + -0.0365 * agef
    RootsSbm = 0.0272 + 0.0269 * agef
    Totalbm = 0.7018 + 0.0058 * agef
    Total_abvgbm = 0.5436 + 0.0193 * agef
  } else if(tolower(species) %in% c("bjørk", "birch", "broadleave", "løv", "lauv",  "deciduous") ){
    Stembm = 0.3964 + -0.0186 * agef
    Foliagebm = NA_real_ # 0. + 0.0
    Branchesbm = 0.1011 + 0.0180 * agef
    Branchesdbm = 0.0053 + 0.0059 * agef
    Barkbm = 0.0588 + 0.0221 * agef
    Stumpbm = 0.08 # guesstimate based on Lethonen etal Discussion chapter forth paragraph birch can be expected to be 0.19 for stump&rooot system
    RootsCbm = 0.07
    RootsSbm = 0.04
    Totalbm = NA_real_
    Total_abvgbm = 0.05616 + -0.0179 * agef
  }

  return(data.frame(spc_age = paste0(species, age), Stembm = Stembm, Foliagebm, Branchesbm, Branchesdbm, Barkbm, Stumpbm, RootsCbm, RootsSbm, Totalbm, Total_abvgbm))

}
