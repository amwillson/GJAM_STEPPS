## Helper functions for various steps in data preparation
## Each function described below

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