## Helper functions for various steps in data preparation
## Each function described below

## Used in 2.Visualize_data.R
# Melts array to dataframe and adds column names
# to data frame
melt_array <- function(taxon_mat, x, y, time, col_names){
  # Apply names of dimensions to array
  dimnames(taxon_mat) <- list(x, y, time)
  # Melt to data frame
  melted <- reshape2::melt(taxon_mat)
  # Add column names (x, y, time, name of taxon)
  colnames(melted) <- col_names
  # Return data frame
  return(melted)
}

## Used in 2.Visualize_data.R and 4.Visualize_residuals.R
# Combines pre-defined maps of Minnesota and Wisconsin
# and county maps of Upper Michigan to produce a continuous
# map of only minnesota, wisconsin, and upper michigan
map_states <- function(){
  # Map of study region
  mnwi <- tigris::states(cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME %in% c('Minnesota', 'Wisconsin'))
  mi <- tigris::states(cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Michigan')
  
  mi_ke <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Keweenaw')
  mi_on <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Ontonagon')
  mi_lu <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Luce')
  mi_sc <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Schoolcraft')
  mi_ba <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Baraga')
  mi_al <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Alger')
  mi_ir <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Iron')
  mi_ma <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Mackinac')
  mi_go <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Gogebic')
  mi_me <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Menominee')
  mi_ch <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Chippewa')
  mi_de <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Delta')
  mi_di <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Dickinson')
  mi_mr <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Marquette')
  mi_ho <- tigris::counties(state = 'MI', cb = TRUE, class = 'sf') |>
    dplyr::filter(NAME == 'Houghton')
  
  sf::sf_use_s2(FALSE)
  
  clip_ke <- sf::st_crop(x = mi, y = mi_ke)
  clip_on <- sf::st_crop(x = mi, y = mi_on)
  clip_lu <- sf::st_crop(x = mi, y = mi_lu)
  clip_sc <- sf::st_crop(x = mi, y = mi_sc)
  clip_ba <- sf::st_crop(x = mi, y = mi_ba)
  clip_al <- sf::st_crop(x = mi, y = mi_al)
  clip_ir <- sf::st_crop(x = mi, y = mi_ir)
  clip_ma <- sf::st_crop(x = mi, y = mi_ma)
  clip_go <- sf::st_crop(x = mi, y = mi_go)
  clip_me <- sf::st_crop(x = mi, y = mi_me)
  clip_ch <- sf::st_crop(x = mi, y = mi_ch)
  clip_de <- sf::st_crop(x = mi, y = mi_de)
  clip_di <- sf::st_crop(x = mi, y = mi_di)
  clip_mr <- sf::st_crop(x = mi, y = mi_mr)
  clip_ho <- sf::st_crop(x = mi, y = mi_ho)
  
  mi_join <- sf::st_union(x = clip_ke, y = clip_on)
  mi_join <- sf::st_union(x = mi_join, y = clip_lu)
  mi_join <- sf::st_union(x = mi_join, y = clip_sc)
  mi_join <- sf::st_union(x = mi_join, y = clip_ba)
  mi_join <- sf::st_union(x = mi_join, y = clip_al)
  mi_join <- sf::st_union(x = mi_join, y = clip_ir)
  mi_join <- sf::st_union(x = mi_join, y = clip_ma)
  mi_join <- sf::st_union(x = mi_join, y = clip_go)
  mi_join <- sf::st_union(x = mi_join, y = clip_me)
  mi_join <- sf::st_union(x = mi_join, y = clip_ch)
  mi_join <- sf::st_union(x = mi_join, y = clip_de)
  mi_join <- sf::st_union(x = mi_join, y = clip_di)
  mi_join <- sf::st_union(x = mi_join, y = clip_mr)
  mi_join <- sf::st_union(x = mi_join, y = clip_ho)
  
  states <- sf::st_union(x = mnwi, y = mi_join)
  states <- sf::st_transform(x = states, crs = 'EPSG:3175')
  return(states)
}

## Used in: 3.Detrend_abundance.R
# Finds the four (or fewer) "rook" adjacent cells to a given cell
# then averages relative abundance over those cells at each
# time point
average_adjacent_cells <- function(taxon_mat, ncell, x, y, time){
  # Convert the input array to a raster
  rr <- terra::rast(taxon_mat)
  
  # Storage array
  # Dimensions are the same as the array for a given taxon
  # because we are simply defining what the relative abundance
  # of surrounding cells is for each cell at each time point
  adj_arr <- array(, dim = c(length(x), length(y), length(time)))
  # Add dimension names to allow for easier comparison between
  # original array and adjacent array
  dimnames(adj_arr) <- list(x, y, time)
  
  # Way to match indexing because the way rasters work requires cells
  # to be indexed in one dimension but for our arrays we need to index
  # in two dimensions
  ij_cell <- matrix(data = 1:ncell, nrow = length(x), ncol = length(y), byrow = TRUE)
  
  # Loop over x and y dimensions
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      # The current location
      loc <- rr[i,j,]
      # If the relative abundance at a given location is NA at all
      # time periods, then this cell doesn't exist (it's outside our)
      # spatial domain of interest.
      # For these locations, we don't have to estimate adjacent relative
      # abundance
      if(all(is.na(loc))){
        adj_arr[i,j,] <- NA
      }else{
        # For all cells within our domain, we find which cells are
        # adjacent to the given cell in the four rook directions
        adj_cell <- terra::adjacent(rr, cells = ij_cell[i,j], directions = 'rook')
        # Find values corresponding to the adjacent cells
        adj_val <- rr[adj_cell[1,]]
        # Find mean for each time step and remove any NAs
        # NAs occur when there are not four adjacent cells that are
        # within our domain
        adj_val <- colMeans(adj_val, na.rm = TRUE)
        # Add the mean for each time period to the storage array
        adj_arr[i,j,] <- adj_val
      }
    }
  }
  return(adj_arr)
}

## Used in: 3.Detrend_abundance.R
# Takes the original array for each taxon and matches it 
# with the array with adjacent locations averaged
# The end product is a melted dataframe with the relative
# abundance of a given location at a given time period 
# and the average relative abundance of the surrounding
# grid cells at the same point in time that can be used in
# linear modeling
melt_join <- function(taxon_mat, adj_mat){
  # Melt to dataframe
  taxon_mat_melt <- reshape2::melt(taxon_mat)
  # Add column names
  colnames(taxon_mat_melt) <- c('x', 'y', 'time', 'abundance')
  # Melt adjacent array to dataframe
  adj_mat_melt <- reshape2::melt(adj_mat)
  # Add column names
  colnames(adj_mat_melt) <- c('x', 'y', 'time', 'adjacent_abundance')
  # Join by location and time step
  joined <- dplyr::left_join(x = taxon_mat_melt, y = adj_mat_melt, by = c('x', 'y', 'time'))
  return(joined)
}