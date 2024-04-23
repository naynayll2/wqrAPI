# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @export
#' @importFrom tibble as_tibble
tibble::as_tibble

#' @export
#' @importFrom jsonlite fromJSON
jsonlite::fromJSON

#' @export
#' @importFrom jsonlite toJSON
jsonlite::toJSON

#' @importFrom httr2 request
httr2::request

#' @importFrom httr2 req_method
httr2::req_method

#' @importFrom httr2 req_perform
httr2::req_perform

#' @importFrom httr2 req_body_json
httr2::req_body_json

#' @importFrom httr2 resp_body_string
httr2::resp_body_string

setClass("Endpoint",
         slots = c(type = "character", data_path = "character"))

setGeneric("get_all_lake_names",
           function(endpoint) standardGeneric("get_all_lake_names"))
setGeneric("get_lake_data",
           function(endpoint, lake_name) standardGeneric("get_lake_data"))
setGeneric("get_all_lake_data",
           function(endpoint) standardGeneric("get_all_lake_data"))
setGeneric("get_lake_history",
           function(endpoint, lake_name, start_date, end_date) standardGeneric("get_lake_history"))

#' Returns the names of all lakes registered in the database
setMethod("get_all_lake_names",
  signature(endpoint = "Endpoint"),
  function(endpoint) {
    if (endpoint@type == "test") {
      data_path <- system.file("extdata",
                               "all_lake_names.json", package = "wqrAPI")
      if (!file.exists(data_path)) {
        stop("Invalid file name")
      }
      lake_names_file <- file(data_path, open = "r")
      lake_names_json <- readLines(lake_names_file)
      close(lake_names_file)
      return(lake_names_json)
    } else if (endpoint@type == "live") {
      resp <- request(endpoint@data_path) |>
        req_body_json(list(operation = "GetAllLakeNames")) |>
        req_method("GET") |>
        req_perform() |>
        resp_body_string()
      return(resp)
    }
    stop("Invalid endpoint type")
  }
)

#' Returns most recent data for all registered lakes
#'
#' @param endpoint an Endpoint object
setMethod("get_all_lake_data",
  signature(endpoint = "Endpoint"),
  function(endpoint) {
  if (endpoint@type == "test") {
      data_path <- system.file("extdata",
                               "all_lake_data.json", package = "wqrAPI")
      if (!file.exists(data_path)) {
        stop("Invalid file name")
      }
      lake_data_file <- file(data_path, open = "r")
      lake_data_json <- readLines(lake_data_file)
      data <- fromJSON(lake_data_json)
      data$Date <- Sys.Date()
      lake_data_json <- toJSON(data)
      close(lake_data_file)
      return(lake_data_json)
    } else if (endpoint@type == "live") {
      resp <- request(endpoint@data_path) |>
        req_body_json(list(operation = "GetAllLakeData")) |>
        req_method("GET") |>
        req_perform() |>
        resp_body_string()
      return(resp)
    }
    stop("Invalid endpoint type")
  }
)

#' returns most recent data for a specified lake. The name should be all lowercase, use
#' underscores instead of spaces, and should not have a "Lake" suffix. e.g.,
#' "bde_maka_ska"
#'
#' @param endpoint an Endpoint object
#' @param lake_name a string correctly specifying the name of a lake
setMethod("get_lake_data",
  signature(endpoint = "Endpoint", lake_name = "character"),
  function(endpoint, lake_name) {
    if (endpoint@type == "test") {
      lake_data <- paste(lake_name, "_data.json", sep = "")
      data_path <- system.file("extdata", lake_data, package = "wqrAPI")
      if (!file.exists(data_path)) {
        stop("Invalid file name")
      }
      data_file <- file(data_path, open = "r")
      json_data <- readLines(data_file)
      data <- fromJSON(json_data)
      data$Date <- Sys.Date
      lake_data_json <- toJSON(data)
      close(data_file)
      return(json_data)
    } else if (endpoint@type == "live") {
      resp <- request(endpoint@data_path) |>
        req_body_json(list(operation = "GetLakeData", lakeName = lake_name)) |>
        req_method("GET") |>
        req_perform() |>
        resp_body_string()
      return(resp)
    }
    stop("Invalid endpoint type")
  }
)

#' Returns all lake data within a specified date range for a given lake.
#' The lake name should be all lowercase, use underscores instead of spaces, and
#'  should not have a "Lake" suffix. e.g., "bde_maka_ska"
#'
#' @param endpoint an Endpoint object
#' @param lake_name a string correctly specifying the name of a lake
#' @param start_date oldest date to include
#' @param end_date most recent date to include (defaults to today)
setMethod("get_lake_history",
  signature(endpoint = "Endpoint", lake_name = "character"),
  function(endpoint, lake_name, start_date, end_date = Sys.Date()) {
    if (endpoint@type == "test") {
      lake_data <- paste(lake_name, "_history.json", sep = "")
      data_path <- system.file("extdata", lake_data, package = "wqrAPI")
      if (!file.exists(data_path)) {
        stop("Invalid file name")
      }
      data_file <- file(data_path, open = "r")
      json_data <- readLines(data_file)
      today <- Sys.Date()
      dates <- seq(today,  today - 364, by="-1 day")
      df <- fromJSON(json_data)
      df$history$Date <- dates
      json_data <- toJSON(df$history[df$history$Date > start_date & df$history$Date <= end_date, ] )
      close(data_file)
      return(json_data)
    } else if (endpoint@type == "live") {
      resp <- request(endpoint@data_path) |>
        req_body_json(list(operation = "GetLakeHistory", lakeName = lake_name, startTime = start_date, endTime = end_date)) |>
        req_method("GET") |>
        req_perform() |>
        resp_body_string()
      return(resp)
    }
    stop("Invalid endpoint type")
  }
)

setClass(
  "RequestHandler",
  slots = c(endpoint = "Endpoint")
)

#'Returns all lake names
#'
#' @param endpoint a RequestHandler object
#'
#' @returns a tibble containing lake names
#'
#' @export
setMethod("get_all_lake_names",
  signature("RequestHandler"),
  function(endpoint) {
    as_tibble(fromJSON(get_all_lake_names(endpoint@endpoint)))
  }
)

#'Returns most recent data for all lakes
#'
#'@param endpoint a RequestHandler object
#'
#'@returns a tibble of most recent data from all registered lakes
#'
#' @export
setMethod("get_all_lake_data",
  c("RequestHandler"),
  function(endpoint) {
    as_tibble(fromJSON(get_all_lake_data(endpoint@endpoint)))
  }
)

#' returns most recent data for a specified lake. The name should be all lowercase, use
#' underscores instead of spaces, and should not have a "Lake" suffix. e.g.,
#' "bde_maka_ska"
#'
#' @param endpoint a RequestHandler object
#' @param lake_name a string correctly specifying the name of a lake
#'
#' @returns a one-row tibble
#'
#' @export
setMethod("get_lake_data",
  c("RequestHandler", "character"),
  function(endpoint, lake_name) {
    as_tibble(fromJSON(get_lake_data(endpoint@endpoint, lake_name)))
  }
)

#' Returns all lake data within a specified date range for a given lake.
#' The lake name should be all lowercase, use underscores instead of spaces, and
#'  should not have a "Lake" suffix. e.g., "bde_maka_ska"
#'
#' @param endpoint an RequestHandler object
#' @param lake_name a string correctly specifying the name of a lake
#' @param start_date oldest date to include
#' @param end_date most recent date to include (defaults to today)
#'
#' @export
setMethod("get_lake_history",
  signature(endpoint = "RequestHandler", lake_name = "character"),
  function(endpoint, lake_name, start_date, end_date = Sys.Date()) {
    as_tibble(fromJSON(get_lake_history(
      endpoint@endpoint, lake_name, start_date, end_date)))
  }
)

make_endpoint <- function(type, data_path = NULL) {
  obj <- new("Endpoint")
  obj@type <- type
  if (is.null(data_path)) {
    obj@data_path <- system.file("extdata", package = "wqrAPI")
  } else {
    obj@data_path <- data_path
  }
  obj
}

#'Returns a request handler
#'
#' @param type Determines if you are using a mock handler with mock data (for
#' testing) or a "real" handler with live data. Use "test" for the mock handler
#' and "live" for deployment
#'
#' @export
request_handler <- function(type, data_path = NULL) {
  obj <- new("RequestHandler")
  obj@endpoint <- make_endpoint(type, data_path)
  obj
}
