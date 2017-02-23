TODO:
----

- Add UTC offset to file attributes?

- Add function to change timestep size?

- Write time step interval to file name? Some sites hourly, others half-hourly

- Need a separate qc flag for time steps filled using ERAinterim data. DONE, but not documented. Do we want to add info about qf flags in the netcdf files?

- Some variables are outside our specified range in the original dataset. E.g. SWup has negative values in Howard Springs. Do we want to  change these to zero or change the range in variables.csv?

-VPD2RH function: RH outside (0,100) range at some sites, setting it to 0.01 and 100, respectively. Do we want a better solution?

- Provide an example code in python? I think it's possible to call R from python??

- Add % gap-filled and missing as a variable attribute

- Move secs_per_day from Check_and_gapfill to Constants. Also used (as 86400) in FluxTowerSpreadsheetToNc.

- Gap check function is long and needs tidying up



Some info to create package...

package depends on: rvest, R.utils, pals.R

- Either move all functions from R/functions into R/, or merge back into palsR, and depend on that.

- Document undocumented functions
- Fill out documentation in /man/FluxnetProcessing-package.Rd ?

