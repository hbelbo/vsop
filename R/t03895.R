####### t03895
#' Skogsavvirkning volum t03895
#' Hogststatistikk for rundvirke SSB tabell 03895
#' 1996 - dd
#'
#' Tabellen gir avvirkningsvolum for salg, etter sortiment, kommune, year.
#' Volum er avregningsvolum (m3pris)
#'
#' @param region_level regionnivaa; velg ett av landet, fylke or kommune
#' @param Tid same as Tid variable used in PxWebApiData::Introduction
#'
#' @return en tibble
#' @export
#' @importFrom rlang .data
#' @examples
#' volumstat <- t03895()
#' volumstat2 <- t03895(region_level = "fylke")
#' volumstat3 <- t03895(region_level = "kommune")
#' volumstat4 <- t03895(region_level = "fylke", Tid = c("2021"))

t03895 <- function(region_level = "fylker2020", Tid = TRUE ){ # 1996 - dd

  if ( !(tolower(region_level) %in% c("fylker2020" , "kommuner2020", "fylke", "kommune"))) { stop("warning: to get result, region_level should be one of 'fylker2020', 'kommuner2020','fylke', 'kommune'" )}
  kmn2020 <- c("K-3001", "K-3002", "K-3003", "K-3004", "K-3005", "K-3006", "K-3007", "K-3011", "K-3012", "K-3013", "K-3014", "K-3015", "K-3016", "K-3017", "K-3018", "K-3019", "K-3020", "K-3021", "K-3022", "K-3023", "K-3024", "K-3025", "K-3026", "K-3027", "K-3028", "K-3029", "K-3030", "K-3031", "K-3032", "K-3033", "K-3034", "K-3035", "K-3036", "K-3037", "K-3038", "K-3039", "K-3040", "K-3041", "K-3042", "K-3043", "K-3044", "K-3045", "K-3046", "K-3047", "K-3048", "K-3049", "K-3050", "K-3051", "K-3052", "K-3053", "K-3054", "K-0301", "K-3401", "K-3403", "K-3405", "K-3407", "K-3411", "K-3412", "K-3413", "K-3414", "K-3415", "K-3416", "K-3417", "K-3418", "K-3419", "K-3420", "K-3421", "K-3422", "K-3423", "K-3424", "K-3425", "K-3426", "K-3427", "K-3428", "K-3429", "K-3430", "K-3431", "K-3432", "K-3433", "K-3434", "K-3435", "K-3436", "K-3437", "K-3438", "K-3439", "K-3440", "K-3441", "K-3442", "K-3443", "K-3446", "K-3447", "K-3448", "K-3449", "K-3450", "K-3451", "K-3452", "K-3453", "K-3454", "K-3801", "K-3802", "K-3803", "K-3804", "K-3805", "K-3806", "K-3807", "K-3808", "K-3811", "K-3812", "K-3813", "K-3814", "K-3815", "K-3816", "K-3817", "K-3818", "K-3819", "K-3820", "K-3821", "K-3822", "K-3823", "K-3824", "K-3825", "K-4201", "K-4202", "K-4203", "K-4204", "K-4205", "K-4206", "K-4207", "K-4211", "K-4212", "K-4213", "K-4214", "K-4215", "K-4216", "K-4217", "K-4218", "K-4219", "K-4220", "K-4221", "K-4222", "K-4223", "K-4224", "K-4225", "K-4226", "K-4227", "K-4228", "K-1101", "K-1103", "K-1106", "K-1108", "K-1111", "K-1112", "K-1114", "K-1119", "K-1120", "K-1121", "K-1122", "K-1124", "K-1127", "K-1130", "K-1133", "K-1134", "K-1135", "K-1144", "K-1145", "K-1146", "K-1149", "K-1151", "K-1160", "K-4601", "K-4602", "K-4611", "K-4612", "K-4613", "K-4614", "K-4615", "K-4616", "K-4617", "K-4618", "K-4619", "K-4620", "K-4621", "K-4622", "K-4623", "K-4624", "K-4625", "K-4626", "K-4627", "K-4628", "K-4629", "K-4630", "K-4631", "K-4632", "K-4633", "K-4634", "K-4635", "K-4636", "K-4637", "K-4638", "K-4639", "K-4640", "K-4641", "K-4642", "K-4643", "K-4644", "K-4645", "K-4646", "K-4647", "K-4648", "K-4649", "K-4650", "K-4651", "K-1505", "K-1506", "K-1507", "K-1511", "K-1514", "K-1515", "K-1516", "K-1517", "K-1520", "K-1525", "K-1528", "K-1531", "K-1532", "K-1535", "K-1539", "K-1547", "K-1554", "K-1557", "K-1560", "K-1563", "K-1566", "K-1573", "K-1576", "K-1577", "K-1578", "K-1579", "K-5001", "K-5006", "K-5007", "K-5014", "K-5020", "K-5021", "K-5022", "K-5025", "K-5026", "K-5027", "K-5028", "K-5029", "K-5031", "K-5032", "K-5033", "K-5034", "K-5035", "K-5036", "K-5037", "K-5038", "K-5041", "K-5042", "K-5043", "K-5044", "K-5045", "K-5046", "K-5047", "K-5049", "K-5052", "K-5053", "K-5054", "K-5055", "K-5056", "K-5057", "K-5058", "K-5059", "K-5060", "K-5061", "K-1804", "K-1806", "K-1811", "K-1812", "K-1813", "K-1815", "K-1816", "K-1818", "K-1820", "K-1822", "K-1824", "K-1825", "K-1826", "K-1827", "K-1828", "K-1832", "K-1833", "K-1834", "K-1835", "K-1836", "K-1837", "K-1838", "K-1839", "K-1840", "K-1841", "K-1845", "K-1848", "K-1851", "K-1853", "K-1856", "K-1857", "K-1859", "K-1860", "K-1865", "K-1866", "K-1867", "K-1868", "K-1870", "K-1871", "K-1874", "K-1875", "K-5401", "K-5402", "K-5403", "K-5404", "K-5405", "K-5406", "K-5411", "K-5412", "K-5413", "K-5414", "K-5415", "K-5416", "K-5417", "K-5418", "K-5419", "K-5420", "K-5421", "K-5422", "K-5423", "K-5424", "K-5425", "K-5426", "K-5427", "K-5428", "K-5429", "K-5430", "K-5432", "K-5433", "K-5434", "K-5435", "K-5436", "K-5437", "K-5438", "K-5439", "K-5440", "K-5441", "K-5442", "K-5443", "K-5444", "K-21-22", "K-23", "K-Rest")
  flk2020 <- c(  "F-30","F-03","F-34","F-38","F-42","F-11","F-46","F-15","F-50","F-18","F-54","F-21","F-22","F-23")
  flk <- c("30", "01", "02", "06", "03", "34", "04", "05", "38", "07", "08", "42", "09", "10", "11", "46", "12", "13", "14", "15", "50", "16", "17", "18", "54", "19", "20", "21", "22", "23", "25", "26", "88", "99")
  kmn <- c("3001", "3002", "3003", "3004", "3005", "3006", "3007", "3011", "3012", "3013", "3014", "3015", "3016", "3017", "3018", "3019", "3020", "3021", "3022", "3023", "3024", "3025", "3026", "3027",
           "3028", "3029", "3030", "3031", "3032", "3033", "3034", "3035", "3036", "3037", "3038", "3039", "3040", "3041", "3042", "3043", "3044", "3045", "3046", "3047", "3048", "3049", "3050", "3051",
           "3052", "3053", "3054", "0101", "0102", "0103", "0104", "0105", "0106", "0111", "0113", "0114", "0115", "0116", "0117", "0118", "0119", "0121", "0122", "0123", "0124", "0125", "0127", "0128",
           "0130", "0131", "0133", "0134", "0135", "0136", "0137", "0138", "0199", "0211", "0213", "0214", "0215", "0216", "0217", "0219", "0220", "0221", "0226", "0227", "0228", "0229", "0230", "0231",
           "0233", "0234", "0235", "0236", "0237", "0238", "0239", "0299", "0601", "0602", "0604", "0605", "0612", "0615", "0616", "0617", "0618", "0619", "0620", "0621", "0622", "0623", "0624", "0625",
           "0626", "0627", "0628", "0631", "0632", "0633", "0699", "0301", "0399", "3401", "3403", "3405", "3407", "3411", "3412", "3413", "3414", "3415", "3416", "3417", "3418", "3419", "3420", "3421",
           "3422", "3423", "3424", "3425", "3426", "3427", "3428", "3429", "3430", "3431", "3432", "3433", "3434", "3435", "3436", "3437", "3438", "3439", "3440", "3441", "3442", "3443", "3446", "3447",
           "3448", "3449", "3450", "3451", "3452", "3453", "3454", "0401", "0402", "0403", "0412", "0414", "0415", "0417", "0418", "0419", "0420", "0423", "0425", "0426", "0427", "0428", "0429", "0430",
           "0432", "0434", "0435", "0436", "0437", "0438", "0439", "0441", "0499", "0501", "0502", "0511", "0512", "0513", "0514", "0515", "0516", "0517", "0518", "0519", "0520", "0521", "0522", "0528",
           "0529", "0532", "0533", "0534", "0536", "0538", "0540", "0541", "0542", "0543", "0544", "0545", "0599", "3801", "3802", "3803", "3804", "3805", "3806", "3807", "3808", "3811", "3812", "3813",
           "3814", "3815", "3816", "3817", "3818", "3819", "3820", "3821", "3822", "3823", "3824", "3825", "0701", "0702", "0703", "0704", "0705", "0706", "0707", "0708", "0709", "0710", "0711", "0712",
           "0713", "0714", "0715", "0716", "0716u", "0717", "0718", "0719", "0720", "0721", "0722", "0723", "0724", "0725", "0726", "0727", "0728", "0729", "0799", "0805", "0806", "0807", "0811", "0814",
           "0815", "0817", "0819", "0821", "0822", "0826", "0827", "0828", "0829", "0830", "0831", "0833", "0834", "0899", "4201", "4202", "4203", "4204", "4205", "4206", "4207", "4211", "4212", "4213",
           "4214", "4215", "4216", "4217", "4218", "4219", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4228", "0901", "0903", "0904", "0906", "0911", "0912", "0914", "0918", "0919",
           "0920", "0921", "0922", "0923", "0924", "0926", "0928", "0929", "0932", "0933", "0935", "0937", "0938", "0940", "0941", "0999", "1001", "1002", "1003", "1004", "1014", "1017", "1018", "1021",
           "1026", "1027", "1029", "1032", "1034", "1037", "1046", "1099", "1101", "1102", "1103", "1106", "1108", "1111", "1112", "1114", "1119", "1120", "1121", "1122", "1124", "1127", "1129", "1130",
           "1133", "1134", "1135", "1141", "1142", "1144", "1145", "1146", "1149", "1151", "1154", "1159", "1160", "1199", "4601", "4602", "4611", "4612", "4613", "4614", "4615", "4616", "4617", "4618",
           "4619", "4620", "4621", "4622", "4623", "4624", "4625", "4626", "4627", "4628", "4629", "4630", "4631", "4632", "4633", "4634", "4635", "4636", "4637", "4638", "4639", "4640", "4641", "4642",
           "4643", "4644", "4645", "4646", "4647", "4648", "4649", "4650", "4651", "1201", "1211", "1214", "1216", "1219", "1221", "1222", "1223", "1224", "1227", "1228", "1230", "1231", "1232", "1233",
           "1234", "1235", "1238", "1241", "1242", "1243", "1244", "1245", "1246", "1247", "1248", "1249", "1250", "1251", "1252", "1253", "1255", "1256", "1259", "1260", "1263", "1264", "1265", "1266",
           "1299", "1301", "1401", "1411", "1412", "1413", "1416", "1417", "1418", "1419", "1420", "1421", "1422", "1424", "1426", "1428", "1429", "1430", "1431", "1432", "1433", "1438", "1439", "1441",
           "1443", "1444", "1445", "1448", "1449", "1499", "1501", "1502", "1503", "1504", "1505", "1506", "1507", "1511", "1514", "1515", "1516", "1517", "1519", "1520", "1523", "1524", "1525", "1526",
           "1527", "1528", "1529", "1531", "1532", "1534", "1535", "1539", "1543", "1545", "1546", "1547", "1548", "1551", "1554", "1556", "1557", "1560", "1563", "1566", "1567", "1569", "1571", "1572",
           "1573", "1576", "1577", "1578", "1579", "1599", "5001", "5004", "5005", "5006", "5007", "5011", "5012", "5013", "5014", "5015", "5016", "5017", "5018", "5019", "5020", "5021", "5022", "5023",
           "5024", "5025", "5026", "5027", "5028", "5029", "5030", "5031", "5032", "5033", "5034", "5035", "5036", "5037", "5038", "5039", "5040", "5041", "5042", "5043", "5044", "5045", "5046", "5047",
           "5048", "5049", "5050", "5051", "5052", "5053", "5054", "5055", "5056", "5057", "5058", "5059", "5060", "5061", "1601", "1612", "1613", "1617", "1620", "1621", "1622", "1624", "1627", "1630",
           "1632", "1633", "1634", "1635", "1636", "1638", "1640", "1644", "1645", "1648", "1653", "1657", "1662", "1663", "1664", "1665", "1699", "1702", "1703", "1711", "1714", "1717", "1718", "1719",
           "1721", "1723", "1724", "1725", "1729", "1736", "1738", "1739", "1740", "1742", "1743", "1744", "1748", "1749", "1750", "1751", "1755", "1756", "1799", "1804", "1805", "1806", "1811", "1812",
           "1813", "1814", "1815", "1816", "1818", "1820", "1822", "1824", "1825", "1826", "1827", "1828", "1832", "1833", "1834", "1835", "1836", "1837", "1838", "1839", "1840", "1841", "1842", "1843",
           "1845", "1848", "1849", "1850", "1851", "1852", "1853", "1854", "1855", "1856", "1857", "1858", "1859", "1860", "1865", "1866", "1867", "1868", "1870", "1871", "1874", "1875", "1899", "5401",
           "5402", "5403", "5404", "5405", "5406", "5411", "5412", "5413", "5414", "5415", "5416", "5417", "5418", "5419", "5420", "5421", "5422", "5423", "5424", "5425", "5426", "5427", "5428", "5429",
           "5430", "5432", "5433", "5434", "5435", "5436", "5437", "5438", "5439", "5440", "5441", "5442", "5443", "5444", "1901", "1902", "1903", "1911", "1913", "1915", "1917", "1919", "1920", "1921",
           "1922", "1923", "1924", "1925", "1926", "1927", "1928", "1929", "1931", "1933", "1936", "1938", "1939", "1940", "1941", "1942", "1943", "1999", "2001", "2002", "2003", "2004", "2011", "2012",
           "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2027", "2028", "2030", "2099", "2111", "2112", "2115", "2121", "2131", "2199", "2211", "2299",
           "2300", "2311", "2321", "2399", "9999")
  if(tolower(region_level) == "fylker2020"){
    reginnd <-  list("agg:KommFylker",  flk2020)
  } else if(tolower(region_level) == "kommuner2020") {
    reginnd <- list("agg:KommSummer", kmn2020);
  } else if(tolower(region_level) == "fylke"){
    reginnd <- list("vs:Fylker", flk);
  } else if(tolower(region_level) == "kommune") {
    reginnd <- list("vs:Kommun", kmn);
  }


  # metadt <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895", returnMetaData = TRUE)
  # metadf <- PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895", returnMetaFrames = TRUE)
  # PxWebApiData::ApiData("http://data.ssb.no/api/v0/no/table/03895", returnMetaValues = TRUE)


  pxdt_en <- PxWebApiData::ApiData12("http://data.ssb.no/api/v0/en/table/03895",
                                Region =  reginnd,
                                Tid = Tid, #c("2010", "2016", "2017"),
                                Treslag = T # 10i)
                                )  %>%


    dplyr::rename(Sortimentkode = .data$Treslag) %>%
    dplyr::mutate(
      species = dplyr::case_when(
        stringr::str_detect(.data$Sortimentkode, "^1") ~ "Spruce",
      stringr::str_detect(.data$Sortimentkode, "^2") ~ "Pine",
      stringr::str_detect(.data$Sortimentkode, "^3") ~ "Broadleaves",
      TRUE ~ "Unknown"
      ) ,
      assortm.grp = dplyr::case_when(
        (.data$Sortimentkode %in% c("1160", "2160") |
         stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "mix",
      stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("11", "21", "31")  ~ "saw",
      stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("14", "24", "34") ~ "pulp",
      TRUE ~ "annet"
    )) %>% dplyr::select(.data$region, .data$assortment, .data$Tid, .data$Region, .data$Sortimentkode, m3 = .data$value, .data$species, .data$assortm.grp)

  pxdt_no <- PxWebApiData::ApiData12("http://data.ssb.no/api/v0/no/table/03895",
                                  Region =   reginnd,
                                  Tid = Tid, #c("2010", "2016", "2017"),
                                  Treslag = T # 10i)
  )  %>%
    dplyr::mutate(Aar = as.numeric(.data$Tid)) %>%

    dplyr::rename(Sortimentkode = .data$Treslag) %>%
    dplyr::mutate(
      treslag = dplyr::case_when(
        stringr::str_detect(.data$Sortimentkode, "^1") ~ "Gran",
        stringr::str_detect(.data$Sortimentkode, "^2") ~ "Furu",
        stringr::str_detect(.data$Sortimentkode, "^3") ~ "Lauv",
        TRUE ~ "Ukjent"
      ) ,
      sortimentgruppe = dplyr::case_when(
        (.data$Sortimentkode %in% c("1160", "2160") |
           stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("13", "23", "1160", "2160"))  ~ "sams",
        stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("11", "21", "31")  ~ "skur",
        stringr::str_sub(.data$Sortimentkode, 1,2) %in% c("14", "24", "34") ~ "massevirke",
        TRUE ~ "annet"
      )) %>% dplyr::select(.data$region, .data$sortiment, .data$Tid, .data$Region, .data$Sortimentkode, m3 = .data$value, .data$treslag, .data$sortimentgruppe)

 pxdt_all <- pxdt_no %>% left_join(pxdt_en, by = c("region", "Region", "Tid", "Sortimentkode", "m3"))


  return(pxdt_all)

}
