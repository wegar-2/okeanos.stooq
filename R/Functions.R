

#' Get daily data from Stooq
#'
#' Get daily time series from Stooq for indicated tickers
#'
#' This function allows for fetching historical time series of DAILY prices for by tickers
#' from free online database at https://stooq.com/.
#' Remark re handling of the wrong ticker symbols passed inside cTickers:
#' @param cTickers - character vector of ticker symbols;
#' @param dateStartDate - Date class scalar, points at the first day of the interval
#' for which the data should be fetched
#' @param dateEndDate - Date class scalar, points at the last day of the interval
#' for which the data should be fetched
#' @return list of data.tables; each member of the list corresponds to one ticker
#' symbol in input vector cTickers
#' @export
lGetDailyDataForTickersFromStooq <- function(
  cTickers, dateStartDate, dateEndDate) {


  # 1. validate function parameters --------------------------------------------
  # 1.1. cTickers symbol
  if (!is.character(cTickers) | length(x = cTickers) == 0L) {
    stop("Incorrect function lGetListOfTickersDataFromStooq parameter cTickers - ",
         "either not a character vector or it is of length zero! ")
  }
  # 1.2. dateStartDate, dateEndDate
  if (!bIsScalarOfClass(objIn = dateStartDate, cClassName = "Date") |
      !bIsScalarOfClass(objIn = dateEndDate, cClassName = "Date")) {
    stop("Incorrect function lGetListOfTickersDataFromStooq parameter ",
         " dateStartDate or dateEndDate - either one of them is not a Date class scalar! ")
  }
  if (dateStartDate > dateEndDate) {
    stop("Incorrect function lGetListOfTickersDataFromStooq parameters ",
         " dateStartDate and dateEndDate - start date later than end date! ")
  }

  # 2. fetch the data from Stooq -----------------------------------------------
  # 2.0. prepare additional required variables
  lDataOut <- vector(mode = "list", length = length(x = cTickers))
  names(lDataOut) <- cTickers
  cStartDate <- format(x = dateStartDate, format = "%Y%m%d")
  cEndDate <- format(x = dateEndDate, format = "%Y%m%d")
  for (cIterTicker in cTickers) {
    message("Fetching daily data for ticker: ", cIterTicker)
    # 2.1. prepare the URL to fetch from
    cDataUrl <- paste0("https://stooq.com/q/d/l/?s=", tolower(cIterTicker),
                       "&d1=", cStartDate, "&d2=", cEndDate, "&i=d")
    # 2.2. try to fetch the data
    message("trying to fetch data from URL: ", cDataUrl)
    res <- try(expr = { utils::read.csv(file = cDataUrl, stringsAsFactors = FALSE) }, silent = TRUE)
    # 2.2. if error, print warning and save the error message in the output
    if (methods::is(object = res, class2 = "try-error")) {
      warning("Failed to fetch the data from URL: cDataUrl; the following error ",
              "occurred: ", res, "; skipping the ticker:  ", cIterTicker,
              "in the output", immediate. = TRUE)
      lDataOut[[cIterTicker]] <- list(NULL)
      next
    }
    if (!is.data.frame(x = res)) {
      # 2.3. check if output is a data.frame
      warning("Corrupt output returned during the fetching of the data for the ticker ",
              cIterTicker, "!!! - skipping the ticker in the output",
              immediate. = TRUE)
      lDataOut[[cIterTicker]] <- list(NULL)
      next
    } else {
      # 2.4. if success, save all the output into list
      if (nrow(x = res) == 0L) {
        warning("Output fetching for the ticker: ", cIterTicker, " is of zero length!",
                "; skipping the ticker in the output! ", immediate. = TRUE)
        lDataOut[[cIterTicker]] <- list(NULL)
        next
      } else {
        dtIterData <- data.table::as.data.table(res)
        dtIterData[["Date"]] <- as.Date(dtIterData[["Date"]], format = "%Y-%m-%d")
        lDataOut[[cIterTicker]] <- dtIterData
      }
    }
  }


  return(lDataOut)
}
