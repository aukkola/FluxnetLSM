FluxnetLSM
=================
R package for post-processing FLUXNET datasets for use in land surface modelling. Performs quality control and data conversion of FLUXNET data and collated site metadata. Supports FLUXNET2015 and La Thuile data releases.

URL: https://github.com/aukkola/FluxnetLSM

Maintainer: [Anna Ukkola](<https://www.climatescience.org.au/staff/profile/aukkola>)

Contributors: Anna Ukkola, Ned Haughton and Martin De Kauwe

Requires: R ≥ 3.1.0


### Installation


The package is managed with [`devtools`](https://github.com/hadley/devtools).


#### 1. Installing required packages

The FluxnetLSM package relies on three R packages: [`R.utils`](https://cran.r-project.org/web/packages/R.utils/R.utils.pdf), [`ncdf4`](https://cran.r-project.org/web/packages/ncdf4/ncdf4.pdf) and [`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf). Please install these first before proceeding to install FluxnetLSM. These packages can be installed directly in R with the command `install.packages(“package_name”)`. We also recommend installing the package [`devtools`](https://cran.r-project.org/web/packages/devtools/README.html) to aid installation.


#### 2. Installing FluxnetLSM

To install FluxnetLSM within R software:

```{r}
# install.packages("devtools")
devtools::install_github("aukkola/FluxnetLSM")
```

If this fails, you can download the source files directly from this website or by cloning the repository with the command:

```{r}
$ git clone https://github.com/aukkola/FluxnetLSM.git
```

If you downloaded the zip file from Github, unzip the archive first. Next using R software, click on Packages & Data > Package Installer (alternatively Tools > Install Packages... depending on your R version). From the drop-down menu, change CRAN (binaries) to Local Source Package (alternatively Repository (CRAN) to Package Archive File (.tgz; tar.gz)). Navigate to the directory where you downloaded the package files and select FluxnetLSM.tar.gz. R will then proceed to install the package.



### Usage
See `examples` for example usage. Three examples are provided for processing a single site or multiple sites:

- single site (`examples/example_conversion_single_site.R`)

- multiple sites (`examples/example_conversion_multiple_sites.R`)

- multiple sites using parallel programming (`examples/example_conversion_multiple_sites_parallel.R`)



### Troubleshooting

1. The package does not recognise the unit conversion

	Unit conversions are performed by the function `ChangeUnits`, you can find this under `R/Conversions.R`. A template is provided towards the end of this function for adding a new conversion.

2. I would like to output model-specific attributes for sites

	Model-specific attributes for each site, such as the plant functional type, can be saved in the `data/Site_metadata.csv` file (see example for CABLE PFT). Model-specific attributes are then set in the function `initialise_model` (stored in `R/Model_initialisations.R`) where a template for adding a new model attribute is provided. Finally, to write these in the NetCDF files, set the argument `model` to your model name when running the package.








