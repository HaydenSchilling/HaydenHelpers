#' Download WaterNSW Gauge Flow Data
#'
#'@description
#'This function pulls flow data from the NSW Water Online API. https://api-portal.waternsw.com.au/reports.
#' Specify sites by number and a date range.
#' You need to have a valid API token to use the function.
#' There are rather restrictive limits on the API basic tier (28 requests per 24 hours and a limit of 1000 rows of data per "request")
#' Large data requests therefore use more API tokens.
#'
#'
#' @param sites a vector of WaterNSW site codes (as strings)
#' @param date_start a date in format 'DD-MMM-YYYY HH:MM' (as string)
#' @param date_end a date in format 'DD-MMM-YYYY HH:MM' (as string)
#' @param token the Water NSW API Subscription Key
#'
#' @returns Dataframe containing daily flow data
#' @export
#'
#'
#' @examples
#'# Example Usage
#' token = keyring::key_get("NSW_Water_Online") # this pulls a saved key from the computer,
#' # set with key_set("NSW_Water_Online")
#' sites <- c("204007","207009","2122001")
#' date_start <- "01-Jan-2022 00:00"
#' date_end <- "01-Jan-2024 00:00"
#'
#' #NOT RUN
#' # water_data <- get_water_flow_data(sites = sites,
#' # date_start = date_start, date_end = date_end, token = water_token)



get_water_flow_data <- function(sites, date_start, date_end, token) {
  # # Load required libraries
  # library(httr2)
  # library(dplyr)
  # library(purrr)
  # library(stringr)
  # library(lubridate)
  # library(progress)

  # Error handling for inputs
  if (missing(sites) || length(sites) == 0) {
    stop("Please provide at least one site ID")
  }

  if (missing(date_start)) {
    stop("Please provide a start date in format 'DD-MMM-YYYY HH:MM'")
  }

  if (missing(date_end)) {
    stop("Please provide an end date in format 'DD-MMM-YYYY HH:MM'")
  }

  if (missing(token)) {
    stop("Please provide a valid API subscription key")
  }

  # Format sites and dates
  sites_fixed <- stringr::str_flatten(sites, ",")
  date_start_fixed <- stringr::str_replace_all(date_start, " ", "%20")
  date_end_fixed <- stringr::str_replace_all(date_end, " ", "%20")

  # Construct initial API request URL
  request_url <- paste0(
    "https://api.waternsw.com.au/water/surface-water-data-api?siteId=",
    sites_fixed,
    "&frequency=Daily&dataType=Combined&variable=FlowRate&startDate=",
    date_start_fixed,
    "&endDate=",
    date_end_fixed,
    "&pageNumber=1"
  )

  # Make initial request
  req_start <- httr2::request(request_url)

  headers <- req_start %>%
    httr2::req_headers(
      'Cache-Control' = 'no-cache',
      'Ocp-Apim-Subscription-Key' = token
    )

  perform_api <- headers %>% httr2::req_perform()
  results <- perform_api %>% httr2::resp_body_json()

  # Store results in a list
  results_list <- list()
  results_list[[1]] <- purrr::map_df(results$records, ~dplyr::as_tibble(.x))

  # Get total pages
  total_pages <- results[["totalPages"]]

  # Process remaining pages if more than one page exists
  i <- 1

  # Create progress bar if there are multiple pages
  if (total_pages > 1) {
    message(paste0("Retrieving data from ", total_pages, " pages..."))
    pb <- progress::progress_bar$new(
      format = "  Downloading [:bar] :percent | Page :current/:total | Elapsed: :elapsed | ETA: :eta",
      total = total_pages,
      width = 80,
      clear = FALSE
    )
    pb$tick(1)  # Mark the first page as complete
  }

  while (i < total_pages) {
    # Add delay between requests to avoid rate limiting
    Sys.sleep(30)

    # Format next URL
    next_url <- stringr::str_replace_all(results[["nextPageUrl"]], " ", "%20")

    # Make request
    request <- httr2::request(next_url)
    headers <- request %>%
      httr2::req_headers(
        'Cache-Control' = 'no-cache',
        'Ocp-Apim-Subscription-Key' = token
      )

    perform_api <- headers %>% httr2::req_perform()
    results <- perform_api %>% httr2::resp_body_json()

    # Add results to list
    results_list[[i+1]] <- purrr::map_df(results$records, ~tibble::as_tibble(.x))
    i <- i + 1

    # Update progress bar
    if (total_pages > 1) {
      pb$tick()
    }
  }

  # Combine all results and format date
  full_result <- dplyr::bind_rows(results_list) %>%
    dplyr::mutate(Date = lubridate::dmy_hm(timeStamp))

  return(full_result)
}

# Water Data API Function

# This function pulls flow data from the NSW Water Online API. https://api-portal.waternsw.com.au/reports
# Specify sites by number and a date range.
# You need to have a valid API token to use the function.
# There are rather restrictive limits on the API basic tier (28 requests per 24 hours and a limit of 1000 rows of data per "request")
# Large data requests therefore use more API tokens

# Function Description:
# sites - a vector of site codes (as strings)
# date_start = a date in format 'DD-MMM-YYYY HH:MM' (as string)
# date_end = a date in format 'DD-MMM-YYYY HH:MM' (as string)
# token = the Water NSW API Subscription Key

# ### Example Usage
# token = keyring::key_get("NSW_Water_Online") # this pulls a saved key from the computer, set with key_set("NSW_Water_Online")
# sites <- c("204007","207009","2122001")
# date_start <- "01-Jan-2022 00:00"
# date_end <- "01-Jan-2024 00:00"
#
#
# # function_test <- get_water_flow_data(sites = sites, date_start = date_start, date_end = date_end, token = water_token)
