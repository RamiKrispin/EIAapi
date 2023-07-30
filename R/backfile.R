#' Pull a Large Number of Observations with a Sequential Query
#' @description This function allows users to overcome the API's observation limit
#' per query by breaking down the query into smaller sequential sub-queries and
#' appending back the results. The main use case of this function is for backfilling
#' hourly series.
#' @param start defines the start time of the series, should use a POSIXt class
#' for hourly series or Date format for non-hourly series (daily, monthly, etc.)
#' @param end defines the end time of the series, should use a POSIXt class
#' for hourly series or Date format for non-hourly series (daily, monthly, etc.)
#' @param offset An integer, defines the number of observations limitation per query
#' @param api_key A string, EIA API key, see https://www.eia.gov/opendata/ for registration to the API service
#' @param api_path A string, the API path to follow the API endpoint https://api.eia.gov/v2/.
#' The path can be found on the EIA API dashboard, for more details see https://www.eia.gov/opendata/browser/
#' @param facets A list, optional, set the filtering argument (defined as 'facets'
#' on the API header), following the structure of list(facet_name_1 = value_1,
#' facet_name_2 = value_2)
#' @details The function use start, end, and offset arguments to define a sequence
#' of queries. While the API enables you to pull up to 5000 observations per query,
#' it is not recommended, for hourly series, to pull more than ~ 2160 observations
#' per query (~ 3 months).
#' @return A time series
#' @export
#' @examples
#'
#'  start <- as.POSIXlt("2018-06-19T00", tz = "UTC")
#'  end <- lubridate::floor_date(Sys.time()- lubridate::days(2), unit = "day")
#'  attr(end, "tzone") <- "UTC"
#'  offset <- 24 * 30 * 3
#'  api_key <- Sys.getenv("eia_key")
#'  api_path <- "electricity/rto/region-sub-ba-data/data/"
#'
#'  facets = list(parent = "NYIS",
#'                subba = "ZONA")
#'
#'  df <- eia_backfill(start = start,
#'                 end = end,
#'                 offset = offset,
#'                 api_key = api_key,
#'                 api_path = api_path,
#'                 facets = facets)
#'
#'  at_y <- pretty(df$value)[c(2, 4, 6)]
#'  at_x <- seq.POSIXt(from = start,
#'                   to = end,
#'                   by = "2 years")
#'  plot(df$time, df$value,
#'       col = "#1f77b4",
#'       type = "l",
#'       frame.plot = FALSE,
#'       axes = FALSE,
#'       panel.first = abline(h = at_y, col = "grey80"),
#'       main = "NY Independent System Operator (West) - Hourly Generation of Electricity",
#'       xlab = "Source: https://www.eia.gov/",
#'       ylab = "MegaWatt/Hours")
#'
#'  mtext(side =1, text = format(at_x, format = "%Y"), at = at_x,
#'        col = "grey20", line = 1, cex = 0.8)
#'
#'  mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
#'        col = "grey20", line = 1, cex = 0.8)
#'

eia_backfill <- function(start,
                     end,
                     offset,
                     api_key,
                     api_path,
                     facets){

  # Error handling

  # Validating the start and end arguments
  if(!any(lubridate::is.POSIXt(start),
          lubridate::is.Date(start))){
    stop("The start argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
  } else if(!any(lubridate::is.POSIXt(end),
                 lubridate::is.Date(end))){
    stop("The end argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
  } else if(!any(class(start) %in% class(end))){
    stop("The class of the start argument is different from the end argument class")
    # Validate the offset argument
  } else if(!is.numeric(offset) || offset < 0 || offset %% 1 != 0){
    stop("The offset argument is not valid, must be numeric")
    # Validate the api_key and api_path arguments
  } else if(!is.character(api_key)){
    stop("The api_key argument is not valid")
  } else if(!is.character(api_path)){
    stop("The api_path argument is not valid")
    # Validate the facets argument
  } else if(!is.null(facets) && !is.list(facets)){
    stop("The facets argument must be a list object")
  }


  # Classify the time stamp to either POSIXt or Date object
  if(lubridate::is.POSIXt(start)){
    time_class <- "POSIXt"
  } else if(lubridate::is.Date(start)){
    time_class <- "Date"
  } else {
    stop("Mismatch with start/end arguments class")
  }

  # Create a time/date vector

  if(time_class == "POSIXt"){
    time_vec <- seq.POSIXt(from = start, to = end, by = paste(offset, "hour"))
    if(max(time_vec) < end){
      time_vec <- c(time_vec, end)
    }
  } else if(time_class == "Date"){
    time_vec <- seq.Date(from = start, to = end, by = "day")
  }



  df <- lapply(seq_along(time_vec)[-length(time_vec)], function(i){

    temp <- start_h <- end_h <- start_time <- end_time <- NULL
    s <- time_vec[i]
    if(time_class == "POSIXt"){
      e <- time_vec[i + 1] - lubridate::hours(1)
      start_h <- lubridate::hour(s)
      end_h <- lubridate::hour(e)
      if(start_h < 10){
        start_time <- paste(substr(as.character(s), 1, 10), "T0", start_h, sep = "")
      } else {
        start_time <- paste(substr(as.character(start), 1, 10), "T", start_h, sep = "")
      }


      if(end_h < 10){
        end_time <- paste(substr(as.character(e), 1, 10), "T0", end_h, sep = "")
      } else {
        end_time <- paste(substr(as.character(e), 1, 10), "T", end_h, sep = "")
      }

      temp <- EIAapi::eia_get(api_key = api_key,
                              api_path = api_path,
                              facets = facets,
                              format = "data.frame",
                              start = start_time,
                              end = end_time,
                              length = NULL,
                              offset = NULL) |>
        dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
        dplyr::select(-period) |>
        dplyr::select(time, dplyr::everything()) |>
        dplyr::arrange(time)



    } else if(time_class == "Date"){
      e <- time_vec[i + 1] - lubridate::days(1)
      temp <- EIAapi::eia_get(api_key = api_key,
                              api_path = api_path,
                              facets = facets,
                              format = "data.frame",
                              start = s,
                              end = e,
                              length = NULL,
                              offset = NULL) |>
        dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
        dplyr::select(-period) |>
        dplyr::select(time, dplyr::everything()) |>
        dplyr::arrange(time)

    }

    names(temp) <- gsub(pattern = "-", replacement = "_", x = names(temp))

    return(temp)
  }) |>
    dplyr::bind_rows()



  return(df)

}
