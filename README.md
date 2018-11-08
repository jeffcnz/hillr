# hillr

A R package for interacting with a Hilltop time series server.

## Getting Started

In R make sure that you have devtools installed.
Remove any preinstalled versions of hillr, then run

```R
devtools::install_github("jeffcnz/hillr")
```

To get a list of sites available from the endpoint

```R
sites <- getHilltopSites(endpoint = "http://data.hbrc.govt.nz/Envirodata/EMAR.hts?")
```

To get a list of measurements at a site

```R
measurements <- getHilltopMeasurements(endpoint = "http://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                                                   site = "Maraetotara Lagoon")
```

To get some data for a site you'll need to know the Hilltop server endpoint, site name, and measurement

```R
eColi <- getHilltopData(endpoint = "http://data.hbrc.govt.nz/Envirodata/EMAR.hts?",
                       site = "Maraetotara Lagoon",
                       measurement = "E. Coli",
                       from = "1/1/2018",
                       to = "1/10/2018")
```

## Replaces

This package replaces and extends the functions contained in the Hilltop.R repository.  That repository will remain, but this package is meant to be used instead of the previous functions.

## Authors

* **Jeff Cooke**

## License

This project is licensed under the MIT License


