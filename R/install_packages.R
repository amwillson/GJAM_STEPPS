# Install packages required through full analysis
# Installing specific version except where it threw an error

if(!require(remotes)) install.packages('remotes')
library(remotes)

remotes::install_version(package = 'car',
                         version = '3.1.2')
remotes::install_version(package = 'corrplot',
                         version = '0.92')
remotes::install_version(package = 'cowplot',
                         version = '1.1.3')
remotes::install_version(package = 'dplyr',
                         version = '1.1.4')
install.packages('fields') # version 15.2
remotes::install_version(package = 'ggplot2',
                         version = '3.5.1')
install.packages('gjam') # version 2.6.2
remotes::install_version(package = 'lme4',
                         version = '1.1.35.3')
install.packages('ncdf4') # version 1.22
remotes::install_version(package = 'R.matlab',
                         version = '3.7.0')
remotes::install_version(package = 'RColorBrewer',
                         version = '1.1.3')
remotes::install_version(package = 'reshape2',
                         version = '1.4.4')
install.packages('sf') # version 1.0.16
remotes::install_version(package = 'sfheaders',
                         version = '0.4.4')
remotes::install_version(package = 'tibble',
                         version = '3.2.1')
remotes::install_version(package = 'tidyr',
                         version = '1.3.1')
remotes::install_version(package = 'tidytext',
                         version = '0.4.2')
remotes::install_version(package = 'tigris',
                         version = '2.1')
