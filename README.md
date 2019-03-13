FluxnetLSM
=================
R package for post-processing FLUXNET datasets for use in land surface modelling. Performs quality control and data conversion of FLUXNET data and collated site metadata. Supports FLUXNET2015, La Thuile and OzFlux data releases.

URL: https://github.com/aukkola/FluxnetLSM

Maintainer: [Anna Ukkola](<https://www.climatescience.org.au/staff/profile/aukkola>)

Contributors: Anna Ukkola, Ned Haughton and Martin De Kauwe

Requires: R ≥ 3.1.0

Package is described in:
Ukkola, A. M., Haughton, N., De Kauwe, M. G., Abramowitz, G., and Pitman, A. J.: [FluxnetLSM R package (v1.0):
a community tool for processing FLUXNET data for use in land surface modelling](https://doi.org/10.5194/gmd-10-3379-2017),
Geosci. Model Dev., 10, 3379-3390, 2017


N.B. options to check for level of gap-filling have changed since v1.0

Now using separate "missing" thresholds for met and flux variables, as well as
two tiers of gapfilling thresholds for met variables and
a separate gap-fill check for flux variables


### Installation


The package is managed with [`devtools`](https://github.com/hadley/devtools).


#### 1. Installing required packages

The FluxnetLSM package relies on three R packages: [`R.utils`](https://cran.r-project.org/web/packages/R.utils/R.utils.pdf), [`ncdf4`](https://cran.r-project.org/web/packages/ncdf4/ncdf4.pdf) and [`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf). Please install these first before proceeding to install FluxnetLSM. These packages can be installed directly in R with the command `install.packages(“package_name”)`. We also recommend installing the package [`devtools`](https://cran.r-project.org/web/packages/devtools/README.html) to aid installation.


#### 2. Installing FluxnetLSM

To install FluxnetLSM within R software using `devtools`:

```{r}
# install.packages("devtools")
devtools::install_github("aukkola/FluxnetLSM")
```

If this fails, you can download the source files by cloning the repository with the command:

```{r}
$ git clone https://github.com/aukkola/FluxnetLSM.git
```

You can then install the package within R with the command:

```{r}
install.packages("path_to_downloaded_package_files/FluxnetLSM", repos=NULL, type='source')
```


### Usage
See `examples` for example usage. Three examples are provided FLUXNET2015 and La Thuile for processing a single site or multiple sites:

- single site (`examples/*/example_conversion_single_site.R`)

- multiple sites (`examples/*/example_conversion_multiple_sites.R`)

- multiple sites using parallel programming (`examples/*/example_conversion_multiple_sites_parallel.R`)



### Troubleshooting

1. The package does not recognise the unit conversion

	Unit conversions are performed by the function `ChangeUnits`, you can find this under `R/Conversions.R`. A template is provided towards the end of this function for adding a new conversion.

2. I would like to output model-specific attributes for sites

	Model-specific attributes for each site, such as the plant functional type, can be saved in the `data/Site_metadata.csv` file (see example for CABLE PFT). Model-specific attributes are then set in the function `initialise_model` (stored in `R/Model_initialisations.R`) where a template for adding a new model attribute is provided. Finally, to write these in the NetCDF files, set the argument `model` to your model name when running the package.








