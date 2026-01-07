library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(tibble)
library(shinycssloaders)

# ---- Load CSV once ----
final_data <- readRDS("data/combined_clean_flat.rds")

# ---- Define helper functions ----
source("R/functions.R")

# ---- Define UI variables ----
categorytags <- c('Everything', 'Coding Help', 'Math Help', 'Real World Topics', 
                  'Problem Solving', 'Creative Thinking', 'Complex Knowledge', 'Creative Writing')
plottypes <- c('Bar Plot', 'Heat Map')
models <- unique(final_data$model_a)