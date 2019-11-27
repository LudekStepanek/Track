# tracking main

plot_folder <- ("C:\\Users\\Ludek\\Documents\\R\\plots\\")

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\ts_sample\\"


data_folder <- "K:\\t_sample\\"
data_folder <- "K:\\TrackingExperimentProcessed\\"
data_folder <- "K:\\project_starved\\"

setwd("C:\\Users\\Ludek\\Documents\\R\\Track\\")

source("libraries_sources.R")

pixel_size <- 0.65

tracks <- import_tracks(data_folder, Load = F)

add_1(tracks)

add_DP_simplify(tracks, 120)

tracks_old <- readRDS("K:\\TrackingExperimentProcessed\\data_add2.sav")
setDT(tracks_old)

tracks_starved <- tracks

tracks_starved <- readRDS("K:\\TrackingExperimentProcessed\\data_add2.sav")
setDT(tracks_starved)

add_DP_simplify(tracks, 24)
print_tracks_by_quantile(tracks_starved)

