# tracking main

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\ts_sample\\"

setwd("C:\\Users\\Ludek\\Documents\\R\\track\\")

source("libraries_sources.R")

pixel_size <- 0.65

tracks <- import_tracks(data_folder, Load = F)

add_1(tracks)

add_DP_simplify(tracks, 3)