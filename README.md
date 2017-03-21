FluxnetLSM
=================
R package for post-processing FLUXNET datasets for use in land surface modelling. Performs quality control and data conversion of FLUXNET data and collated site metadata.

URL: https://github.com/aukkola/FluxnetLSM

Maintainer: [Anna Ukkola](<https://www.climatescience.org.au/staff/profile/aukkola>)

Contributors: Anna Ukkola, Ned Haughton and Martin De Kauwe

Requires: R ≥ 3.1.0


### Installation


The package is managed with [`devtools`](https://github.com/hadley/devtools).

The package relies on the PALS R package. Please install it first before proceeding to install FluxnetLSM.


#### 1. Installing PALS

To install PALS within R software:

```{r}
# install.packages("devtools")
devtools::install_github('dudek313/palsR')
```


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


