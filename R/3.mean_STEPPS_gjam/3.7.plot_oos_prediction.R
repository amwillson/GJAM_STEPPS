### STEP 3-7

## Plotting out-of-sample predictions in time
## Out-of-sample prediction of 300 YBP for same spatial locations as
## used to fit the model

## Input: out/mean/oos_prediciton_time.RData
## OOS predictions for 300 YBP using non-conditional, conditional, and
## conditional on only oak methods

## Input: data/processed/mean_stepps_soil_clim.RData
## OOS data

## Output: none

rm(list = ls())

source('R/funs.R')

# Load out-of-sample predictions
load('out/mean/oos_prediction_time.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

#### Non-conditional prediction ####

# Map of study region
states <- map_states()

# Predictive mean
pred_mean <- pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)
pred_mean$x <- taxon_oos_all$stepps_x
pred_mean$y <- taxon_oos_all$stepps_y

### Plot each taxon's predictive mean over space ###

## BEECH

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oc), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oh), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y

# Plot

## BEECH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_conifer), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_hardwood), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Conditional prediction ####

# Loop through each output
pred_cond <- matrix(, nrow = nrow(pred_mean), ncol = 11)

for(i in 1:11){
  temp <- cond_pred[[i]]
  pred_cond[,i] <- temp$sdList$yMu[,i]
}

colnames(pred_cond) <- colnames(pred_mean)[1:11]
pred_cond <- as.data.frame(pred_cond)
pred_cond$x <- pred_mean$x
pred_cond$y <- pred_mean$y

### Plot each taxon's predictive mean over space ###

## BEECH

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('OAK') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oc), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oh), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_cond |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_cond, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y

# Plot

## BEECH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_conifer), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_hardwood), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Conditional on oak ####

# Predictive mean
pred_mean <- oak_cond_pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)
pred_mean$x <- taxon_oos_all$stepps_x
pred_mean$y <- taxon_oos_all$stepps_y

### Plot each taxon's predictive mean over space ###

## BEECH

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oc), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oh), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

pred_mean |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y

# Plot

## BEECH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_conifer), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_hardwood), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_gradient2(low = 'red', 
                                 high = 'darkblue', 
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
