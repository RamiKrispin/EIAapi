#' Pull Metadata from EIA API
#' @description Get data descriptions and metadata from the EIA API
#' @details The function enables to explore the different data categories and available routes
#' inline with the API dashboard (https://www.eia.gov/opendata/browser/)
#' @param api_key A string, EIA API key, see https://www.eia.gov/opendata/ for registration to the API service
#' @param api_path A string, the API category/route path following the API endpoint (i.e., 'https://api.eia.gov/v2/')
#' If set to NULL (default) or as empty string "" it returns the main categories available on the API.
#' The path can be found on the EIA API dashboard, for more details see https://www.eia.gov/opendata/browser/
#' @return a list object with the series description and metadata
#' @export
#' @examples
#'\dontrun{
#' electricity_metadata <- eia_metadata(api_key = Sys.getenv("eia_key"),
#'                                      api_path = "electricity")
#'
#' electricity_metadata$response$description
#' electricity_metadata$response$id
#' electricity_metadata$response$name
#' electricity_metadata$response$routes
#'
#'}
eia_metadata <- function(api_path = NULL,
                         api_key){

  # V2 end point
  end_point <- "https://api.eia.gov/v2/"

  if(missing(api_key)){
    stop("The api_key argument is missing... \033[0;92m\xE2\x9D\x8C\033[0m\n")
  } else if(!is.character(api_key)){
    stop("The api_key argument is not valid... \033[0;92m\xE2\x9D\x8C\033[0m\n")
  } else if(missing(api_path)){
    stop(paste("The api_path argument is missing... \033[0;92m\xE2\x9D\x8C\033[0m\n",
               "Please check the API Dashboard for the API URL:\n",
               "https://www.eia.gov/opendata/browser/", sep = ""))
  } else if(!is.null(api_path) && !is.character(api_path)){
    stop(paste("The api_path argument is not valid, must be a character object \033[0;92m\xE2\x9D\x8C\033[0m\n",
               "Please check the API Dashboard for the API URL:\n",
               "https://www.eia.gov/opendata/browser/", sep = ""))
  }


  if(is.null(api_path)){
    path <- ""
  } else {
    path <- api_path
  }

  api_url <- paste(end_point, api_path, sep = "")
  query <- NULL
  query <- paste("curl '",
                 api_url,
                 "'?api_key=",
                 api_key,
                 sep = "")


  raw <- NULL

  tryCatch({
    raw <- system(command  = query, intern = TRUE)},
    error = function(c) "Fail to pull the query, please check the error log",
    message = function(c) "message"
  )

  if(is.null(raw) || !is.character(raw)){
    stop(paste("Could not pull the metadata... \033[0;92m\xE2\x9D\x8C\033[0m\n",
               "Check the query parameters (e.g., api key, path, etc.) or the error log\n", sep = ""))
  }

  parsed_query <- NULL

  tryCatch({
    parsed_query <- jsonlite::fromJSON(raw)},
    error = function(c) "Fail to pull the query, please check the error log",
    message = function(c) "message"
  )


  if(is.null(parsed_query) || !is.list(parsed_query)){
    stop(paste("Could not parse the metadata JSON... \033[0;92m\xE2\x9D\x8C\033[0m\n",
               "Please check the query parameters (e.g., api key, path, etc.) or the error log\n", sep = ""))
  }


  meta <- parsed_query$response
  meta[["command"]] <- parsed_query$request$command

  attr(meta, "class") <- c(class(meta), "eia_metadata")

  return(meta)
}
