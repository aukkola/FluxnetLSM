FluxnetProcessing
=================
Quality control and data conversion scripts for Fluxnet data.

URL: https://github.com/aukkola/FLUXNET2015_processing


### Installation

Package managed with [`devtools`](https://github.com/hadley/devtools).

> TODO: Depends on a on-standard version of palsR, need to update when
> https://github.com/dudek313/palsR/pull/4 is complete. For now, install with
> `devtools::install_github('dudek313/palsR', rev='Gab', subdir='pals')`

To install:

```{r}
# install.packages("devtools")
devtools::install_github("aukkola/FLUXNET2015_processing")
```

### Usage

See `tests/ConvertSpreadsheetToNcdf_AU-How.R` for example usage.
