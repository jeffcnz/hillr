#Get a testdataset
testfulldata <- fullGetHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/data.hts?",
                              site = c("010480M5", "030020M1"),
                              measurement = c("Compliance Volume", "Total for Consent"),
                              from = "1/1/1970",
                              to = "31/12/2023",
                              method = "Total",
                              interval = "1 Month",
                              gapTolerance = "Interval",
                              showFinal = "Yes",
                              dateOnly = "Yes")

testfulldataDP <- hillr::fullGetHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                          site = c("HAWQi NSWQ", "Tukituki River at Red Bridge"),
                                          measurement = c("[pH (Depth Profile)]", "Flow"),
                                          from = "1/1/2020",
                                          to = "31/12/2023",
                                          method = "Total",
                                          interval = "1 Month",
                                          gapTolerance = "Interval",
                                          showFinal = "Yes",
                                          dateOnly = "Yes")

testFullEnsembeStatsByYear <- hillr::fullGetHilltopEnsembleStats(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                                      sites = c("Ngaruroro River at Fernhill", "Tukituki River at Red Bridge"),
                                                      measurement=c("Water Temperature (Tidbit)", "Water Temperature"),
                                                      from=NULL,
                                                      to=NULL,
                                                      timeInterval=NULL,
                                                      statistic="DailyPDF",
                                                      lowerPercentile="5",
                                                      upperPercentile="95",
                                                      byYear=TRUE)

# Test Full Hilltop Data
test_that("fullGetHilltopData returns a list.", {
  expect_type(testfulldata, "list")
})

test_that("fullGetHilltopData returns results for multiple sites.", {
  expect_equal(length(unique(testfulldata$Site)), 2)
})

test_that("fullGetHilltopData returns results for multiple measurements.", {
  expect_equal(length(unique(testfulldata$Measurement)), 2)
})

test_that("fullGetHilltopData returns the right number of columns.", {
  expect_equal(ncol(testfulldataDP), 7)
})

# Option parameter provides a depreciated message

# Test Full Ensemble Stats
test_that("fullGetHilltopEnsembleStats returns a list.", {
  expect_type(testFullEnsembeStatsByYear, "list")
})

test_that("fullGetHilltopEnsembleStats returns results for multiple sites.", {
  expect_equal(length(unique(testFullEnsembeStatsByYear$Site)), 2)
})

test_that("fullGetHilltopEnsembleStats returns results for multiple measurements.", {
  expect_equal(length(unique(testFullEnsembeStatsByYear$Measurement)), 2)
})

test_that("fullGetHilltopEnsembleStats returns the right number of columns.", {
  expect_equal(ncol(testFullEnsembeStatsByYear), 19)
})
