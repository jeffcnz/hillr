
ensembleStatUrlBase <- hillr::buildEnsembleStatsRequestUrl(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                                           site = "Ngaruroro River at Fernhill",
                                                           measurement="Water Temperature (Tidbit)",
                                                           from=NULL,
                                                           to=NULL,
                                                           timeInterval=NULL,
                                                           statistic="DailyPDF",
                                                           lowerPercentile=NULL,
                                                           upperPercentile=NULL)

ensembleStatUrlFull <- hillr::buildEnsembleStatsRequestUrl(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                                           site = "Ngaruroro River at Fernhill",
                                                           measurement="Water Temperature (Tidbit)",
                                                           from="1/10/2017",
                                                           to="1/4/2018",
                                                           timeInterval=NULL,
                                                           statistic="DailyPDF",
                                                           lowerPercentile=5,
                                                           upperPercentile=95)


# Test buildEnsembleSTatsRequestUrl

test_that("Base buildEnsembleStatsRequestUrl returns a correct url.", {
  expect_equal(ensembleStatUrlBase, "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?Service=Hilltop&Request=EnsembleStats&Site=Ngaruroro%20River%20at%20Fernhill&Measurement=Water%20Temperature%20(Tidbit)&Statistic=DailyPDF")
})

test_that("Full buildEnsembleStatsRequestUrl returns a correct url.", {
  expect_equal(ensembleStatUrlFull, "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?Service=Hilltop&Request=EnsembleStats&Site=Ngaruroro%20River%20at%20Fernhill&Measurement=Water%20Temperature%20(Tidbit)&From=1/10/2017&To=1/4/2018&Statistic=DailyPDF&LowerPercentile=5&UpperPercentile=95")
})
