# Small built-in dictionaries. These are code lists, not Orbis data.

#' NACE Rev. 2 level 1 sections
#'
#' @return A data.table with section code, broad name, and numeric range.
#' @export
nace_rev2_level1 <- function() {
  data.table::data.table(
    nace1 = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"),
    name = c(
      "Agriculture, forestry and fishing", "Mining and quarrying", "Manufacturing",
      "Electricity, gas, steam and air conditioning supply",
      "Water supply, sewerage, waste management and remediation activities",
      "Construction", "Wholesale and retail trade, repair of motor vehicles and motorcycles",
      "Transportation and storage", "Accommodation and food service activities",
      "Information and communication", "Financial and insurance activities",
      "Real estate activities", "Professional, scientific and technical activities",
      "Administrative and support service activities", "Public administration and defence",
      "Education", "Human health and social work activities",
      "Arts, entertainment and recreation", "Other service activities",
      "Activities of households as employers", "Activities of extraterritorial organisations and bodies"
    ),
    from = c(1L, 5L, 10L, 35L, 36L, 41L, 45L, 49L, 55L, 58L, 64L, 68L, 69L, 77L, 84L, 85L, 86L, 90L, 94L, 97L, 99L),
    to = c(3L, 9L, 33L, 35L, 39L, 43L, 47L, 53L, 56L, 63L, 66L, 68L, 75L, 82L, 84L, 85L, 88L, 93L, 96L, 98L, 99L)
  )
}

#' NACE Rev. 2 level 2 codes
#'
#' @return A data.table with two-digit NACE Rev. 2 code, level 1 section, and name.
#' @export
nace_rev2_level2 <- function() {
  codes <- c("01","02","03","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","35","36","37","38","39","41","42","43","45","46","47","49","50","51","52","53","55","56","58","59","60","61","62","63","64","65","66","68","69","70","71","72","73","74","75","77","78","79","80","81","82","84","85","86","87","88","90","91","92","93","94","95","96","97","98","99")
  names <- c(
    "Crop and animal production, hunting and related service activities", "Forestry and logging", "Fishing and aquaculture",
    "Mining of coal and lignite", "Extraction of crude petroleum and natural gas", "Mining of metal ores", "Other mining and quarrying", "Mining support service activities",
    "Manufacture of food products", "Manufacture of beverages", "Manufacture of tobacco products", "Manufacture of textiles", "Manufacture of wearing apparel", "Manufacture of leather and related products", "Manufacture of wood and of products of wood and cork, except furniture", "Manufacture of paper and paper products", "Printing and reproduction of recorded media", "Manufacture of coke and refined petroleum products", "Manufacture of chemicals and chemical products", "Manufacture of basic pharmaceutical products and pharmaceutical preparations", "Manufacture of rubber and plastic products", "Manufacture of other non-metallic mineral products", "Manufacture of basic metals", "Manufacture of fabricated metal products, except machinery and equipment", "Manufacture of computer, electronic and optical products", "Manufacture of electrical equipment", "Manufacture of machinery and equipment n.e.c.", "Manufacture of motor vehicles, trailers and semi-trailers", "Manufacture of other transport equipment", "Manufacture of furniture", "Other manufacturing", "Repair and installation of machinery and equipment",
    "Electricity, gas, steam and air conditioning supply", "Water collection, treatment and supply", "Sewerage", "Waste collection, treatment and disposal activities; materials recovery", "Remediation activities and other waste management services",
    "Construction of buildings", "Civil engineering", "Specialised construction activities", "Wholesale and retail trade and repair of motor vehicles and motorcycles", "Wholesale trade, except of motor vehicles and motorcycles", "Retail trade, except of motor vehicles and motorcycles", "Land transport and transport via pipelines", "Water transport", "Air transport", "Warehousing and support activities for transportation", "Postal and courier activities", "Accommodation", "Food and beverage service activities", "Publishing activities", "Motion picture, video and television programme production, sound recording and music publishing", "Programming and broadcasting activities", "Telecommunications", "Computer programming, consultancy and related activities", "Information service activities", "Financial service activities, except insurance and pension funding", "Insurance, reinsurance and pension funding, except compulsory social security", "Activities auxiliary to financial services and insurance activities", "Real estate activities", "Legal and accounting activities", "Activities of head offices; management consultancy activities", "Architectural and engineering activities; technical testing and analysis", "Scientific research and development", "Advertising and market research", "Other professional, scientific and technical activities", "Veterinary activities", "Rental and leasing activities", "Employment activities", "Travel agency, tour operator and other reservation service and related activities", "Security and investigation activities", "Services to buildings and landscape activities", "Office administrative, office support and other business support activities", "Public administration and defence; compulsory social security", "Education", "Human health activities", "Residential care activities", "Social work activities without accommodation", "Creative, arts and entertainment activities", "Libraries, archives, museums and other cultural activities", "Gambling and betting activities", "Sports activities and amusement and recreation activities", "Activities of membership organizations", "Repair of computers and personal and household goods", "Other personal service activities", "Activities of households as employers of domestic personnel", "Undifferentiated goods- and services-producing activities of private households for own use", "Activities of extraterritorial organizations and bodies"
  )
  DT <- data.table::data.table(nace2 = codes, code_int = as.integer(codes), name = names)
  l1 <- nace_rev2_level1()
  DT[, nace1 := l1$nace1[findInterval(code_int, l1$from)]]
  DT[, code_int := NULL]
  data.table::setcolorder(DT, c("nace1", "nace2", "name"))
  DT
}

#' Selected country code crosswalk
#'
#' @return A data.table with two-letter and three-letter ISO country codes for common European countries.
#' @export
bvd_country_codes <- function() {
  data.table::data.table(
    iso2 = c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU","IE","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK"),
    iso3 = c("AUT","BEL","BGR","CHE","CYP","CZE","DEU","DNK","EST","ESP","FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU","LUX","LVA","NLD","NOR","POL","PRT","ROU","SWE","SVN","SVK"),
    country = c("Austria","Belgium","Bulgaria","Switzerland","Cyprus","Czech Republic","Germany","Denmark","Estonia","Spain","Finland","France","United Kingdom","Greece","Croatia","Hungary","Ireland","Italy","Lithuania","Luxembourg","Latvia","Netherlands","Norway","Poland","Portugal","Romania","Sweden","Slovenia","Slovakia")
  )
}
