R package for post-processing FLUXNET datasets for use in land surface modelling. Performs quality control and data conversion of FLUXNET data and collated site metadata. Supports FLUXNET2015, La Thuile, OzFlux and ICOS data releases.

## How to cite this package in your article

Here is the full bibliographic reference to include in your reference list). Please acknowledge our software if key to your analysis.

> Ukkola, A. M., Haughton, N., De Kauwe, M. G., Abramowitz, G., and Pitman, A. J.: [FluxnetLSM R package (v1.0): a community tool for processing FLUXNET data for use in land surface modelling](https://doi.org/10.5194/gmd-10-3379-2017), Geosci. Model Dev., 10, 3379-3390, 2017

## Installation

### development release

To install the development releases of the package run the following
commands:

``` r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("aukkola/FluxnetLSM")
library("FluxnetLSM")
```

Vignettes are not rendered by default, if you want to include additional
documentation please use:

``` r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("aukkola/FluxnetLSM", build_vignettes = TRUE)
library("FluxnetLSM")
```

### Use

See vignettes for example usage. Three examples are provided [FLUXNET2015](docs/articles/FLUXNET2015_processing.html) and 
[La Thuile]() for processing a single site or multiple sites.

### Troubleshooting

1. The package does not recognise the unit conversion

	Unit conversions are performed by the function `ChangeUnits`, you can find this under `R/Conversions.R`. A template is provided towards the end of this function for adding a new conversion.

2. I would like to output model-specific attributes for sites

	Model-specific attributes for each site, such as the plant functional type, can be saved in the `data/Site_metadata.csv` file (see example for CABLE PFT). Model-specific attributes are then set in the function `initialise_model` (stored in `R/Model_initialisations.R`) where a template for adding a new model attribute is provided. Finally, to write these in the NetCDF files, set the argument `model` to your model name when running the package.

## Citation

Ukkola, A. M., Haughton, N., De Kauwe, M. G., Abramowitz, G., and Pitman, A. J.: [FluxnetLSM R package (v1.0): a community tool for processing FLUXNET data for use in land surface modelling](https://doi.org/10.5194/gmd-10-3379-2017), Geosci. Model Dev., 10, 3379-3390, 2017

## Acknowledgements

We acknowledge the support of the Australian
Research Council Centre of Excellence for Climate System Science
(CE110001028). Martin G. De Kauwe was supported by Australian
Research Council Linkage grant LP140100232. This work used
eddy covariance data acquired and shared by the FLUXNET
community, including these networks: AmeriFlux, AfriFlux,
AsiaFlux, CarboAfrica, CarboEuropeIP, CarboItaly, CarboMont,
ChinaFlux, Fluxnet-Canada, GreenGrass, ICOS, KoFlux, LBA,
NECC, OzFlux-TERN, TCOS-Siberia and USCCC. The ERA-
Interim reanalysis data are provided by ECMWF and processed
by LSCE. The FLUXNET eddy covariance data processing and
harmonisation was carried out by the European Fluxes Database
Cluster, AmeriFlux Management Project and Fluxdata project of
FLUXNET, with the support of CDIAC and ICOS Ecosystem
Thematic Center, and the OzFlux, ChinaFlux and AsiaFlux offices. Additional
development was supported by the LEMONTREE project and funded by Schmidt Futures
and under the umbrella of the Virtual Earth System Research Institute (VESRI)
and [BlueGreen Labs](bluegreenlabs.org).