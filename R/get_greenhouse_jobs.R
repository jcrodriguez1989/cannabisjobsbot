#' Get All Greenhouse Jobs
#'
#' @param greenhouse_companies A tibble with columns `id`, `company`, and `tw`, which stand for
#'   the Greenhouse ID, company name, and twitter handle respectively.
#'
#' @importFrom dplyr `%>%` everything mutate select
#' @importFrom purrr map_dfr
#'
get_greenhouse_jobs <- function(greenhouse_companies) {
  map_dfr(seq_len(nrow(greenhouse_companies)), function(i) {
    company <- greenhouse_companies[i, ]
    greenhouse_api_jobs(company$id) %>%
      mutate(twitter = company$tw, company = company$company) %>%
      select(company, everything())
  })
}

#' Get Greenhouse API Jobs
#'
#' @param board_token The Greenhouse ID of the company to query jobs.
#'
#' @importFrom dplyr `%>%` mutate_at one_of tibble vars
#' @importFrom glue glue
#' @importFrom httr content GET
#' @importFrom purrr map_dfr
#' @importFrom readr parse_datetime
#'
greenhouse_api_jobs <- function(board_token) {
  gh_url <- glue("https://boards-api.greenhouse.io/v1/boards/{board_token}/departments")
  jobs_list <- content(GET(gh_url))$departments
  if (is.null(jobs_list)) {
    warning("No jobs found for ", board_token)
    return(data.frame())
  }
  map_dfr(jobs_list, function(department) {
    map_dfr(department$jobs, function(job_data) {
      tibble(
        department = department$name,
        job = job_data$title,
        location = job_data$location$name,
        updated_at = job_data$updated_at,
        url = job_data$absolute_url
      )
    })
  }) %>%
    mutate_at(vars(one_of("updated_at")), parse_datetime)
}
