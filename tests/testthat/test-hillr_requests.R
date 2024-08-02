testData <- getHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                           site = "Ngaruroro River at Fernhill",
                           measurement = "Flow",
                           from = "1/1/2020",
                           to = "3/1/2020")

testWQ <- getHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMARDiscrete.hts?",
                        site = "Maraetotara Lagoon",
                        measurement = "E. Coli",
                        from = "1/1/2018",
                        to = "1/10/2018")

testExtrema <- getHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                           site = "Ngaruroro River at Fernhill",
                           measurement = "Flow",
                           method = "Extrema",
                           interval = "1 Day",
                           from = "1/1/2020",
                           to = "3/1/2020")

testDepthProfile <- getHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                   site = "HAWQi NSWQ",
                                   measurement = "[pH (Depth Profile)]",
                                   from = "1/1/2020")

testSiteList <- getHilltopSites(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?")

testSiteListNoLoc <- getHilltopSites(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                     location = FALSE)

testSiteListMeas <- getHilltopSites(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                    measurement = "Flow")

testEnsembleStats <- getHilltopEnsembleStats(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                      site = "Ngaruroro River at Fernhill",
                                      measurement="Water Temperature (Tidbit)",
                                      from=NULL,
                                      to=NULL,
                                      timeInterval=NULL,
                                      statistic="DailyPDF",
                                      lowerPercentile=NULL,
                                      upperPercentile=NULL)

testEnsembeStatsByYear <- fullGetHilltopEnsembleStats(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                           sites = c("Ngaruroro River at Fernhill", "Tukituki River at Red Bridge"),
                                           measurement=c("Water Temperature (Tidbit)", "Water Temperature"),
                                           from=NULL,
                                           to=NULL,
                                           timeInterval=NULL,
                                           statistic="DailyPDF",
                                           lowerPercentile="5",
                                           upperPercentile="95",
                                           byYear=TRUE)

# Test GetHilltopData Normal Timeseries
test_that("getHilltopData returns a list.", {
  expect_type(testData, "list")
})

test_that("getHilltopData returns data.", {
  expect_gt(nrow(testData), 0)
})

# Test GetHilltopData Water Quality Timeseries
test_that("getHilltopData returns a list.", {
  expect_type(testWQ, "list")
})

test_that("getHilltopData returns data.", {
  expect_gt(nrow(testWQ), 0)
})

# Test GetHilltopData Extrema
test_that("getHilltopData returns a list.", {
  expect_type(testExtrema, "list")
})

test_that("getHilltopData returns data.", {
  expect_gt(nrow(testExtrema), 0)
})

# Test GetHilltopData Depth Profiles
test_that("getHilltopData returns a list.", {
  expect_type(testDepthProfile, "list")
})

test_that("getHilltopData returns data.", {
  expect_gt(nrow(testDepthProfile), 0)
})

# Check field names
test_that("getHilltopData returns expected field names.", {
  expect_true(all(c("Time", "Value", "QualityCode", "Site", "Measurement", "Units") %in% names(testData)))
  #Check for water quality too
  expect_true(all(c("Time", "Value", "QualityCode", "Site", "Measurement", "Units") %in% names(testWQ)))
})


# Test getHilltopSites

test_that("getHilltopSites returns a list.", {
  expect_type(testSiteList, "list")
})

test_that("getHilltopSites returns data.", {
  expect_gt(nrow(testSiteList), 0)
})

test_that("getHilltopSites with a measurement parameter returns data.", {
  expect_gt(nrow(testSiteListMeas), 0)
})

test_that("getHilltopSites with a measurement parameter returns less data than without.", {
  expect_gt(nrow(testSiteList), nrow(testSiteListMeas))
})

# Test getEnsembleStats

test_that("getEnsembleStats returns a list.", {
  expect_type(testEnsembleStats, "list")
})

test_that("getEnsembleStats returns data.", {
  expect_gt(nrow(testEnsembleStats), 0)
})

test_that("getEnsembleStats with byYear set to TRUE returns data.", {
  expect_gt(nrow(testEnsembeStatsByYear), 0)
})

test_that("getHilltopSites with byYear set to TRUE returns more data than without.", {
  expect_gt(nrow(testEnsembeStatsByYear), nrow(testEnsembleStats))
})
