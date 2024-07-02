### STEP 1-1

## Reading in and formatting STEPPS mean data product
## Going from NCDF data structure to arrays

## Input: data/input/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc
## netCDF file with mean relative abundances from STEPPS model
## from https://portal.edirepository.org/nis/mapbrowse?scope=msb-paleon&identifier=22

## Output: data/processed/mean_STEPPS.RData
## coordinates, time, and relative abundances in array format
## Used in 1.2.Plot_STEPPS.R, 3.1.subsample_stepps.R

rm(list = ls())

# Get raw data
fc <- ncdf4::nc_open('data/input/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')

# Extract dimensions
x <- ncdf4::ncvar_get(nc = fc, varid = 'x')
y <- ncdf4::ncvar_get(nc = fc, varid = 'y')
time <- ncdf4::ncvar_get(nc = fc, varid = 'Time')

# Extract relative abundances
ash <- ncdf4::ncvar_get(nc = fc, varid = 'ASH')
beech <- ncdf4::ncvar_get(nc = fc, varid = 'BEECH')
birch <- ncdf4::ncvar_get(nc = fc, varid = 'BIRCH')
elm <- ncdf4::ncvar_get(nc = fc, varid = 'ELM')
hemlock <- ncdf4::ncvar_get(nc = fc, varid = 'HEMLOCK')
maple <- ncdf4::ncvar_get(nc = fc, varid = 'MAPLE')
oak <- ncdf4::ncvar_get(nc = fc, varid = 'OAK')
other_conifer <- ncdf4::ncvar_get(nc = fc, varid = 'OTHER.CONIFER')
other_hardwood <- ncdf4::ncvar_get(nc = fc, varid = 'OTHER.HARDWOOD')
pine <- ncdf4::ncvar_get(nc = fc, varid = 'PINE')
spruce <- ncdf4::ncvar_get(nc = fc, varid = 'SPRUCE')
tamarack <- ncdf4::ncvar_get(nc = fc, varid = 'TAMARACK')

# Save
save(x, y, time, ash, beech, birch, elm, hemlock, maple, oak,
     other_conifer, other_hardwood, pine, spruce, tamarack,
     file = 'data/processed/mean_STEPPS.RData')