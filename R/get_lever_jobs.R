#' Get All Lever Jobs
#'
#' @param lever_companies A tibble with columns `id`, `company`, and `tw`, which stand for
#'   the Lever ID, company name, and twitter handle respectively.
#'
#' @importFrom dplyr `%>%` everything mutate select
#' @importFrom purrr map_dfr
#'
get_lever_jobs <- function(lever_companies) {
  map_dfr(seq_len(nrow(lever_companies)), function(i) {
    company <- lever_companies[i, ]
    lever_api_jobs(company$id) %>%
      mutate(twitter = company$tw, company = company$company) %>%
      select(company, everything())
  })
}

#' Get Lever API Jobs
#'
#' @param board_token The Lever ID of the company to query jobs.
#'
#' @importFrom dplyr `%>%` mutate tibble
#' @importFrom glue glue
#' @importFrom httr content GET
#' @importFrom lubridate as_datetime
#' @importFrom purrr map_dfr
#'
lever_api_jobs <- function(board_token) {
  lever_url <- glue("https://api.lever.co/v0/postings/{board_token}?mode=json")
  jobs_list <- content(GET(lever_url))
  map_dfr(jobs_list, function(job_data) {
    tibble(
      department = job_data$categories$team,
      job = job_data$text,
      location = job_data$categories$location,
      updated_at = job_data$createdAt,
      url = job_data$hostedUrl
    )
  }) %>%
    mutate(updated_at = as_datetime(updated_at / 1000))
}
