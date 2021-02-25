testthat::test_that(desc = "testing 'lGetDailyDataForTickersFromStooq' function", code = {

  # 1. check if raises error when expected
  testthat::expect_error(
    object = lGetDailyDataForTickersFromStooq(
      cTickers = "WIG20", dateStartDate = as.Date(x = "2019-01-01"), dateEndDate = "asdf"),
    regexp = "Incorrect function lGetListOfTickersDataFromStooq parameter  dateStartDate or dateEndDate")
  testthat::expect_error(
    object = lGetDailyDataForTickersFromStooq(
      cTickers = "WIG20", dateStartDate = "asdf", dateEndDate = as.Date(x = "2019-01-01")),
    regexp = "Incorrect function lGetListOfTickersDataFromStooq parameter  dateStartDate or dateEndDate")
  testthat::expect_error(
    object = lGetDailyDataForTickersFromStooq(
      cTickers = "WIG20", dateStartDate = as.Date("2020-01-01"), dateEndDate = as.Date(x = "2019-01-01")),
    regexp = "tart date later than end date")

  # 2. check if works when it should
  testthat::expect_is(
    object = lGetDailyDataForTickersFromStooq(
      cTickers = "WIG20", dateStartDate = as.Date("2020-01-01"),
      dateEndDate = as.Date(x = "2021-01-01")),
    class = "list")

  # 3. behavior on passing an invalid ticker symbol
  testthat::expect_type(
    object = suppressWarnings(lGetDailyDataForTickersFromStooq(
      cTickers = "qwerty", dateStartDate = as.Date("2020-01-01"),
      dateEndDate = as.Date(x = "2021-01-01"))),
    type = "list")

  # 4. behavior on passing multiple correct tickers
  testthat::expect_is(
    object = lGetDailyDataForTickersFromStooq(
      cTickers = c("WIG20", "WIG"), dateStartDate = as.Date("2020-01-01"),
      dateEndDate = as.Date(x = "2021-01-01")),
    class = "list")


})
