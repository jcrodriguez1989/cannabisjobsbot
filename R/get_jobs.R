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
    "Weedmaps", "weedmaps77", "@weedmaps",
    "Dutchie", "thedutchie", "@getdutchie",
    "Jane", "janetechnologies", NA_character_,
    "LeafLink", "leaflink", "@LeafLinkUS",
    "Green Thumb", "greenthumbindustries", "@gtigrows",
    "Connected", "connectedcannabis", "@connected_cc",
    "Verano Holdings", "veranoholdings", "@veranocannabis",
    "Parallel", "paralleljobs", "@live_parallel",
    "Caliva", "caliva", "@gocaliva",
  )

  # List data about cannabis brands which use Lever.
  lever_companies <- tribble(
    ~company, ~id, ~tw,
    "Eaze", "eaze", "@eaze",
    "Distru", "distru", "@DistruApp",
    "Wyld", "wyld", "@wyld_canna",
  )

  # Pull jobs data, arrange and return it.
  bind_rows(
    get_greenhouse_jobs(greenhouse_companies),
    get_lever_jobs(lever_companies)
  ) %>%
    arrange(desc(updated_at))
}
