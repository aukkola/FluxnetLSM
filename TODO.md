TODO:
----

- Currently duplicates error message, fix

- Use QC flags in a better way? Good, medium, poor quality gap-fill. Currently only checks if gap-filled or not

- Check if VPD range set appropriately in variables.csv

- Add UFC offset to file attributes?

- Fix RH to Qair conversion

- Add function to change timestep size?

- Write time step interval to file name? Some sites hourly, others half-hourly

- Provide option to produce PALS-style plots at the end of data processing?

- Need a separate qc flag for time steps filled using ERAinterim data




Some info to create package...

package depends on: rvest, R.utils

- Either move all functions from R/functions into R/, or merge back into palsR, and depend on that.
- Document undocumented functions
- Fill out documentation in /man/FluxnetProcessing-package.Rd ?

