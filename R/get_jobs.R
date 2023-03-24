#' Get All Known Cannabis Jobs
#'
#' @importFrom dplyr `%>%` arrange bind_rows desc tribble
#'
#' @export
#'
get_jobs <- function() {
  # TODO:
  # recruitingbypaycor.com ~> leafly,
  # jazz.co ~> blaze, tymber, caliperfoods, jushico,
  # bamboohr.com ~> puffco,
  # breezy.hr ~> c3industries, getgoodtree,

  # List data about cannabis brands which use Greenhouse.
  greenhouse_companies <- tribble(
    ~company, ~id, ~tw,
    # "Ascend Wellness Holdings", "awholdings", "",
    "Caliva", "caliva", "@gocaliva",
    "Connected", "connectedcannabis", "@connected_cc",
    "Cresco Labs", "crescolabs", "@crescolabs",
    "Curaleaf", "curaleaf", "@Curaleaf_Inc",
    "Dutchie", "thedutchie", "@getdutchie",
    "Green Roads", "greenroads", "",
    # "Green Thumb", "greenthumbindustries", "@gtigrows",
    # "GrowFlow", "growflow", "@getGrowFlow",
    "Jane", "janetechnologies", "",
    "LeafLink", "leaflink", "@LeafLinkUS",
    "MedMen", "medmen", "@MedMen",
    "Nabis", "nabis", "@getnabis",
    "Natura", "naturaio", "",
    "Ohio Grown Therapies", "ogt", "",
    "Parallel", "paralleljobs", "@live_parallel",
    "Sunnyside*", "sunnyside", "",
    # "The ISA Group", "isa", "",
    "Urbn Leaf", "urbnleaf", "@urbnleaf",
    "Verano Holdings", "veranoholdings", "@veranocannabis",
    "Weedmaps", "weedmaps77", "@weedmaps",
  )

  # List data about cannabis brands which use Lever.
  lever_companies <- tribble(
    ~company, ~id, ~tw,
    "4Front Ventures", "4front", "@4FrontVentures",
    "Amuse", "amuse", "@amusenow",
    # "Calyx Containers", "calyxcontainers", "",
    "Confident Cannabis", "confidentcannabis", "@confidentcanna",
    "Distru", "distru", "@DistruApp",
    # "Eaze", "eaze", "@eaze",
    # "Flow Cannabis Co.", "flowcannabis", "@flowcannabisco",
    "Flowhub", "flowhub", "@Flowhubco",
    "GRAV", "GRAV", "@GravLabs",
    "HERBL", "herbl", "@herblsolutions",
    "Kiva Confections", "kivaconfections", "@kivaconfections",
    # "Lantern", "lanternnow", "@LanternCannabis",
    # "Muse", "jaks", "@MuseCannabis",
    "NorCal Cannabis Co.", "norcalcann", "@norcalcann",
    "Not Pot", "notpot", "@notpotdealer",
    "Pure Sunfarms", "puresunfarms", "@Puresunfarms",
    "Wyld", "wyld", "@wyld_canna",
  )

  # Pull jobs data, arrange and return it.
  bind_rows(
    get_greenhouse_jobs(greenhouse_companies),
    get_lever_jobs(lever_companies)
  ) %>%
    arrange(desc(updated_at))
}
