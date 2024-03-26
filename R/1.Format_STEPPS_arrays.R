## Reading in and formatting STEPPS data product
## Going from NCDF data structure to arrays

rm(list = ls())

# Get raw data
fc <- ncdf4::nc_open('data/raw/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')

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

rm(list = ls())

# Same for SD
# Get raw data
fc <- ncdf4::nc_open('data/raw/msb-paleon-2/2Kyrs_Comp_SD_Level2_v1.0.nc')

# Extract dimensions
x <- ncdf4::ncvar_get(nc = fc, varid = 'x')
y <- ncdf4::ncvar_get(nc = fc, varid = 'y')
time <- ncdf4::ncvar_get(nc = fc, varid = 'Time')

# Extract relative abundances
ash_sd <- ncdf4::ncvar_get(nc = fc, varid = 'ASH')
beech_sd <- ncdf4::ncvar_get(nc = fc, varid = 'BEECH')
birch_sd <- ncdf4::ncvar_get(nc = fc, varid = 'BIRCH')
elm_sd <- ncdf4::ncvar_get(nc = fc, varid = 'ELM')
hemlock_sd <- ncdf4::ncvar_get(nc = fc, varid = 'HEMLOCK')
maple_sd <- ncdf4::ncvar_get(nc = fc, varid = 'MAPLE')
oak_sd <- ncdf4::ncvar_get(nc = fc, varid = 'OAK')
other_conifer_sd <- ncdf4::ncvar_get(nc = fc, varid = 'OTHER.CONIFER')
other_hardwood_sd <- ncdf4::ncvar_get(nc = fc, varid = 'OTHER.HARDWOOD')
pine_sd <- ncdf4::ncvar_get(nc = fc, varid = 'PINE')
spruce_sd <- ncdf4::ncvar_get(nc = fc, varid = 'SPRUCE')
tamarack_sd <- ncdf4::ncvar_get(nc = fc, varid = 'TAMARACK')

# Save
save(x, y, time, ash_sd, beech_sd, birch_sd, elm_sd, hemlock_sd, maple_sd, oak_sd,
     other_conifer_sd, other_hardwood_sd, pine_sd, spruce_sd, tamarack_sd,
     file = 'data/processed/sd_STEPPS.RData')