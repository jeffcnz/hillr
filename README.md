# hillr

A R package for interacting with a Hilltop time series server.  
More information on using Hilltop Servers can be found [here](https://www.hbrc.govt.nz/assets/Document-Library/Council-Data/20170426-HilltopServerTrimmed.pdf)
This package implements many of the features available on a Hilltop server, but not all options are available.

# Features

# Usage

## List of Sites

To get a list of sites available from the endpoint

```R
sites <- getHilltopSites(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMARDiscrete.hts?")
```

## Measurements At A Site

To get a list of measurements at a site

```R
measurements <- getHilltopMeasurements(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMARDiscrete.hts?",
                                                   site = "Maraetotara Lagoon")
```

## Data

To get some data for a site you'll need to know the Hilltop server endpoint, site name, and measurement.
The getHilltopData function does basic requests, but to request multiple sites and or measurements, 
and to get additional metadata you'll need to use the fullGetHilltopData function.

Generally the fullGetHilltopData function is the preferred option.

### Discrete (Sample Data)


```R
eColi <- getHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMARDiscrete.hts?",
                       site = "Maraetotara Lagoon",
                       measurement = "E. Coli",
                       from = "1/1/2018",
                       to = "1/10/2018")
```

To get all the metadata for a measurement, or to get data from multiple sites and measurements

```R
data <- fullGetHilltopData(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMARDiscrete.hts?",
                           sites = c("Maraetotara Lagoon", "Ngaruroro River at Fernhill"),
                           measurements = c("E. Coli", "Nitrate Nitrogen"),
                           from = "1/1/2017",
                           to = "1/10/2017",
                           option = "WQ")
```

# Requirements

# Installation

In R make sure that you have devtools installed.
Remove any preinstalled versions of hillr, then run

```R
devtools::install_github("jeffcnz/hillr")
```

# Authors

* **Jeff Cooke**

# License

This project is licensed under the MIT License


