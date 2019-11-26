# tracking main

plot_folder <- ("C:\\Users\\Ludek\\Documents\\R\\plots\\")

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\ts_sample\\"
data_folder <- "K:\\t_sample\\"


setwd("C:\\Users\\Ludek\\Documents\\R\\Track\\")

source("libraries_sources.R")

pixel_size <- 0.65

tracks <- import_tracks(data_folder, Load = T)

add_1(tracks)

add_DP_simplify(tracks, 200)

DP_scan(tracks,120)

